extern crate proc_macro;

use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, ItemStruct};

#[derive(Clone, Copy, Debug)]
enum Endianness {
    Little,
    Big,
}

fn match_single_angle_bracketed_type(args: &syn::PathArguments) -> Option<syn::Ident> {
    if let syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments {
        args, ..
    }) = args
    {
        if args.len() != 1 {
            return None;
        }
        if let syn::GenericArgument::Type(syn::Type::Path(syn::TypePath {
            qself: None,
            path:
                syn::Path {
                    leading_colon: None,
                    ref segments,
                },
        })) = args.first().unwrap()
        {
            if segments.len() != 1 {
                return None;
            }
            Some(segments.first().unwrap().ident.clone())
        } else {
            None
        }
    } else {
        None
    }
}

fn match_endianness_specifier(field: &syn::Field) -> Option<(Endianness, syn::Ident)> {
    if let syn::Type::Path(syn::TypePath {
        qself: None,
        path: syn::Path {
            leading_colon: None,
            ref segments,
        },
    }) = field.ty
    {
        if segments.len() != 1 {
            return None;
        }
        let first = segments.first().unwrap();

        let endianness = if first.ident == syn::Ident::new("Le", proc_macro2::Span::call_site()) {
            Endianness::Little
        } else if first.ident == syn::Ident::new("Be", proc_macro2::Span::call_site()) {
            Endianness::Big
        } else {
            return None;
        };

        let ident = match match_single_angle_bracketed_type(&first.arguments) {
            Some(i) => i,
            None => return None,
        };

        Some((endianness, ident))
    } else {
        None
    }
}

#[derive(Clone, Debug)]
enum Item {
    RegularField(Box<syn::Field>),
    FieldWithEndianness(syn::Visibility, syn::Ident, (Endianness, syn::Ident)),
    Align(syn::LitInt),
    Pad(syn::LitInt),
    Conditional(Vec<Item>, Box<syn::Expr>),
    CustomParsed(Box<syn::Field>, Box<syn::Type>, Box<syn::Expr>),
}

impl Item {
    fn ident(&self) -> Option<&syn::Ident> {
        match self {
            Self::RegularField(f) => f.ident.as_ref(),
            Self::FieldWithEndianness(_, i, _) => Some(i),
            Self::Conditional(items, _) => items.last().map(|item| item.ident().unwrap()),
            _ => None,
        }
    }
}

#[derive(Debug)]
struct AttrParseError;

fn parse_number(tokens: &proc_macro2::TokenStream) -> Result<syn::LitInt, AttrParseError> {
    let group = syn::parse2::<proc_macro2::Group>(tokens.clone()).map_err(|_| AttrParseError)?;

    if let syn::Lit::Int(int) =
        syn::parse2::<syn::Lit>(group.stream()).map_err(|_| AttrParseError)?
    {
        Ok(int)
    } else {
        Err(AttrParseError)
    }
}

fn field_attribute_items(field: &syn::Field) -> Vec<Item> {
    field
        .attrs
        .iter()
        .map(|attr| {
            let attr_path_ident = attr.path.get_ident().ok_or(AttrParseError)?;

            if attr_path_ident == &syn::Ident::new("align", proc_macro2::Span::call_site()) {
                Ok(Item::Align(parse_number(&attr.tokens)?))
            } else if attr_path_ident == &syn::Ident::new("pad", proc_macro2::Span::call_site()) {
                Ok(Item::Pad(parse_number(&attr.tokens)?))
            } else {
                Err(AttrParseError)
            }
        })
        .collect::<Result<Vec<_>, _>>()
        .unwrap()
}

fn match_field_conditional(field: &mut syn::Field) -> Option<syn::Expr> {
    let (position, _, tokens) = match field
        .attrs
        .iter()
        .enumerate()
        .filter_map(|(idx, attr)| {
            attr.path
                .get_ident()
                .map(|ident| (idx, ident, attr.tokens.clone()))
        })
        .find(|(_, ident, _)| {
            ident == &&syn::Ident::new("condition", proc_macro2::Span::call_site())
        }) {
        Some(p) => p,
        None => return None,
    };
    field.attrs.remove(position);
    Some(syn::parse2::<syn::Expr>(tokens).unwrap())
}

fn match_field_custom(field: &mut syn::Field) -> Option<(syn::Type, syn::Expr)> {
    let (position, _, tokens) = match field
        .attrs
        .iter()
        .enumerate()
        .filter_map(|(idx, attr)| {
            attr.path
                .get_ident()
                .map(|ident| (idx, ident, attr.tokens.clone()))
        })
        .find(|(_, ident, _)| ident == &&syn::Ident::new("custom", proc_macro2::Span::call_site()))
    {
        Some(p) => p,
        None => return None,
    };
    field.attrs.remove(position);

    let tuple = syn::parse2::<syn::ExprTuple>(tokens).unwrap();
    let mut iterator = tuple
        .elems
        .into_pairs()
        .map(syn::punctuated::Pair::into_value);

    let path = if let Some(syn::Expr::Path(syn::ExprPath { path, .. })) = iterator.next() {
        syn::Type::Path(syn::TypePath { qself: None, path })
    } else {
        return None;
    };

    let data_expr = if let Some(data_expr) = iterator.next() {
        data_expr
    } else {
        return None;
    };

    if iterator.next() != None {
        return None;
    }

    Some((path, data_expr))
}

fn item_from_field(mut field: syn::Field) -> Vec<Item> {
    if let Some(condition) = match_field_conditional(&mut field) {
        return vec![Item::Conditional(
            item_from_field(field),
            Box::new(condition),
        )];
    } else if let Some((ty, meta)) = match_field_custom(&mut field) {
        return vec![Item::CustomParsed(
            Box::new(field),
            Box::new(ty),
            Box::new(meta),
        )];
    }

    let mut items: Vec<Item> = vec![];
    items.extend(field_attribute_items(&field));

    items.push(
        if let Some((endianness, ty)) = match_endianness_specifier(&field) {
            Item::FieldWithEndianness(field.vis, field.ident.unwrap(), (endianness, ty))
        } else {
            Item::RegularField(Box::new(field))
        },
    );
    items
}

fn regular_parser_func(ty: &syn::Type) -> syn::Expr {
    let (qself, original_path) = match ty {
        syn::Type::Path(syn::TypePath {
            path: syn::Path { ref segments, .. },
            ref qself,
            ..
        }) => (qself.clone(), segments.clone()),
        other => panic!("{:?}", other),
    };

    let mut segments = original_path;
    segments.push(syn::PathSegment {
        ident: syn::Ident::new("parse", proc_macro2::Span::call_site()),
        arguments: syn::PathArguments::None,
    });

    syn::Expr::Path(syn::ExprPath {
        attrs: vec![],
        qself,
        path: syn::Path {
            leading_colon: None,
            segments,
        },
    })
}

fn regular_serializer_func(ty: &syn::Type) -> syn::Expr {
    let (qself, original_path) = match ty {
        syn::Type::Path(syn::TypePath {
            path: syn::Path { ref segments, .. },
            ref qself,
            ..
        }) => (qself.clone(), segments.clone()),
        other => panic!("{:?}", other),
    };

    let mut segments = original_path;
    segments.push(syn::PathSegment {
        ident: syn::Ident::new("serialize", proc_macro2::Span::call_site()),
        arguments: syn::PathArguments::None,
    });

    syn::Expr::Path(syn::ExprPath {
        attrs: vec![],
        qself,
        path: syn::Path {
            leading_colon: None,
            segments,
        },
    })
}

fn empty_tuple() -> syn::Expr {
    syn::Expr::Tuple(syn::ExprTuple {
        attrs: vec![],
        paren_token: Default::default(),
        elems: syn::punctuated::Punctuated::new(),
    })
}

fn regular_parser_expr(field: syn::Field) -> syn::Expr {
    generic_parser_expr(field.ty.clone(), field.ty, empty_tuple())
}

fn parser_or_serializer_as_trait(
    parser_type: syn::Type,
    trait_ident: syn::Ident,
    target_type: syn::Type,
) -> syn::Type {
    syn::Type::Path(syn::TypePath {
        qself: Some(syn::QSelf {
            as_token: Some(Default::default()),
            lt_token: Default::default(),
            gt_token: Default::default(),
            ty: Box::new(parser_type),
            position: 2,
        }),
        path: syn::Path {
            leading_colon: Some(syn::Token!(::)(proc_macro2::Span::call_site())),
            segments: vec![
                syn::PathSegment {
                    ident: syn::Ident::new("sbp", proc_macro2::Span::call_site()),
                    arguments: syn::PathArguments::None,
                },
                syn::PathSegment {
                    ident: trait_ident,
                    arguments: syn::PathArguments::AngleBracketed(
                        syn::AngleBracketedGenericArguments {
                            lt_token: syn::Token!(<)(proc_macro2::Span::call_site()),
                            gt_token: syn::Token!(>)(proc_macro2::Span::call_site()),
                            colon2_token: Some(syn::Token!(::)(proc_macro2::Span::call_site())),
                            args: vec![
                                syn::GenericArgument::Lifetime(syn::Lifetime::new(
                                    "'_",
                                    proc_macro2::Span::call_site(),
                                )),
                                syn::GenericArgument::Type(target_type),
                            ]
                            .into_iter()
                            .collect::<syn::punctuated::Punctuated<_, syn::Token![,]>>(),
                        },
                    ),
                },
            ]
            .into_iter()
            .collect(),
        },
    })
}

fn generic_parser_expr(
    parser_type: syn::Type,
    target_type: syn::Type,
    data_expr: syn::Expr,
) -> syn::Expr {
    // Assume that every type this function is called for is sbp::Parse, rather than sbp::Parser.

    let ty = parser_or_serializer_as_trait(
        parser_type,
        syn::Ident::new("Parser", proc_macro2::Span::call_site()),
        target_type,
    );

    syn::Expr::Call(syn::ExprCall {
        attrs: vec![],
        func: Box::new(regular_parser_func(&ty)),
        paren_token: syn::token::Paren {
            span: proc_macro2::Span::call_site(),
        },
        args: vec![
            data_expr,
            syn::Expr::Reference(syn::ExprReference {
                attrs: vec![],
                expr: Box::new(syn::Expr::Index(syn::ExprIndex {
                    attrs: vec![],
                    bracket_token: Default::default(),
                    expr: Box::new(simple_expr(syn::Ident::new(
                        "__sbp_proc_macro_bytes",
                        proc_macro2::Span::call_site(),
                    ))),
                    index: Box::new(syn::Expr::Range(syn::ExprRange {
                        attrs: vec![],
                        limits: syn::RangeLimits::HalfOpen(Default::default()),
                        from: Some(Box::new(simple_expr(syn::Ident::new(
                            "__sbp_proc_macro_offset",
                            proc_macro2::Span::call_site(),
                        )))),
                        to: None,
                    })),
                })),
                mutability: None,
                and_token: Default::default(),
                raw: Default::default(),
            }),
        ]
        .into_iter()
        .collect::<syn::punctuated::Punctuated<_, syn::Token![,]>>(),
    })
}

fn optionize(ty: syn::Type) -> syn::Type {
    syn::Type::Path(syn::TypePath {
        qself: None,
        path: syn::Path {
            leading_colon: Some(Default::default()),
            segments: vec![
                syn::PathSegment {
                    arguments: syn::PathArguments::None,
                    ident: syn::Ident::new("core", proc_macro2::Span::call_site()),
                },
                syn::PathSegment {
                    arguments: syn::PathArguments::None,
                    ident: syn::Ident::new("option", proc_macro2::Span::call_site()),
                },
                syn::PathSegment {
                    arguments: syn::PathArguments::AngleBracketed(
                        syn::AngleBracketedGenericArguments {
                            colon2_token: None,
                            gt_token: Default::default(),
                            lt_token: Default::default(),
                            args: vec![syn::GenericArgument::Type(ty)].into_iter().collect(),
                        },
                    ),
                    ident: syn::Ident::new("Option", proc_macro2::Span::call_site()),
                },
            ]
            .into_iter()
            .collect(),
        },
    })
}

fn item_to_field(item: Item) -> Option<Vec<syn::Field>> {
    fn inner(item: Item, will_optionize: bool) -> Option<Vec<syn::Field>> {
        match item {
            Item::Align(_) => None,
            Item::Pad(_) => None,
            Item::FieldWithEndianness(vis, ident, (_, ty)) => Some(vec![syn::Field {
                attrs: vec![],
                ident: Some(ident),
                vis,
                ty: if !will_optionize {
                    syn::Type::Path(syn::TypePath {
                        qself: None,
                        path: syn::Path::from(ty),
                    })
                } else {
                    optionize(syn::Type::Path(syn::TypePath {
                        qself: None,
                        path: syn::Path::from(ty),
                    }))
                },
                colon_token: None,
            }]),
            Item::RegularField(field) => Some(vec![if !will_optionize {
                *field
            } else {
                syn::Field {
                    attrs: field.attrs,
                    colon_token: field.colon_token,
                    ident: field.ident,
                    vis: field.vis,
                    ty: optionize(field.ty),
                }
            }]),
            Item::Conditional(items, _) => Some(
                items
                    .into_iter()
                    .filter_map(|item| inner(item, true))
                    .flatten()
                    .collect(),
            ),
            Item::CustomParsed(field, _, _) => Some(vec![*field]),
        }
    }
    inner(item, false)
}

fn pattern() -> syn::Pat {
    syn::Pat::Tuple(syn::PatTuple {
        attrs: vec![],
        paren_token: Default::default(),
        elems: vec![
            syn::Pat::Ident(syn::PatIdent {
                attrs: vec![],
                by_ref: None,
                mutability: None,
                ident: syn::Ident::new("value", proc_macro2::Span::call_site()),
                subpat: None,
            }),
            syn::Pat::Ident(syn::PatIdent {
                attrs: vec![],
                by_ref: None,
                mutability: None,
                ident: syn::Ident::new("additional_bytes", proc_macro2::Span::call_site()),
                subpat: None,
            }),
        ]
        .into_iter()
        .collect(),
    })
}

fn simple_pattern(ident: syn::Ident) -> syn::Pat {
    syn::Pat::Ident(syn::PatIdent {
        attrs: vec![],
        by_ref: None,
        mutability: None,
        ident,
        subpat: None,
    })
}

fn simple_path(ident: syn::Ident) -> syn::Path {
    syn::Path {
        leading_colon: None,
        segments: std::iter::once(syn::PathSegment {
            ident,
            arguments: syn::PathArguments::None,
        })
        .collect::<syn::punctuated::Punctuated<_, syn::Token![::]>>(),
    }
}

fn simple_type(ident: syn::Ident) -> syn::Type {
    syn::Type::Path(syn::TypePath {
        qself: None,
        path: simple_path(ident),
    })
}

fn simple_expr(ident: syn::Ident) -> syn::Expr {
    syn::Expr::Path(syn::ExprPath {
        attrs: vec![],
        qself: None,
        path: syn::Path {
            leading_colon: None,
            segments: std::iter::once(syn::PathSegment {
                arguments: syn::PathArguments::None,
                ident,
            })
            .collect(),
        },
    })
}

fn pod_combinator_type(endianness: Endianness) -> syn::Type {
    syn::Type::Path(syn::TypePath {
        qself: None,
        path: syn::Path {
            leading_colon: Some(Default::default()),
            segments: vec![
                syn::PathSegment {
                    ident: syn::Ident::new("sbp", proc_macro2::Span::call_site()),
                    arguments: syn::PathArguments::None,
                },
                syn::PathSegment {
                    ident: syn::Ident::new(
                        match endianness {
                            Endianness::Big => "Be",
                            Endianness::Little => "Le",
                        },
                        proc_macro2::Span::call_site(),
                    ),
                    arguments: syn::PathArguments::None,
                },
            ]
            .into_iter()
            .collect::<syn::punctuated::Punctuated<_, syn::Token![::]>>(),
        },
    })
}

fn pod_expr(endianness: Endianness, ty: syn::Ident) -> syn::Expr {
    generic_parser_expr(
        pod_combinator_type(endianness),
        simple_type(ty),
        empty_tuple(),
    )
}

fn offset_incr_expr(to_wrap: syn::Expr) -> syn::Expr {
    syn::Expr::Block(syn::ExprBlock {
        attrs: vec![],
        label: None,
        block: syn::Block {
            brace_token: Default::default(),
            stmts: vec![
                syn::Stmt::Local(syn::Local {
                    attrs: vec![],
                    let_token: Default::default(),
                    init: Some((Default::default(), Box::new(to_wrap))),
                    semi_token: Default::default(),
                    pat: pattern(),
                }),
                syn::Stmt::Semi(
                    syn::Expr::AssignOp(syn::ExprAssignOp {
                        attrs: vec![],
                        op: syn::BinOp::AddEq(Default::default()),
                        left: Box::new(syn::Expr::Path(syn::ExprPath {
                            attrs: vec![],
                            qself: None,
                            path: simple_path(syn::Ident::new(
                                "__sbp_proc_macro_offset",
                                proc_macro2::Span::call_site(),
                            )),
                        })),
                        right: Box::new(syn::Expr::Path(syn::ExprPath {
                            attrs: vec![],
                            qself: None,
                            path: simple_path(syn::Ident::new(
                                "additional_bytes",
                                proc_macro2::Span::call_site(),
                            )),
                        })),
                    }),
                    Default::default(),
                ),
                syn::Stmt::Expr(simple_expr(syn::Ident::new(
                    "value",
                    proc_macro2::Span::call_site(),
                ))),
            ],
        },
    })
}

fn align_stmt(
    alignment: syn::LitInt,
    offset_expr: &syn::Expr,
    align_func: &syn::Expr,
) -> syn::Stmt {
    syn::Stmt::Semi(
        syn::Expr::Assign(syn::ExprAssign {
            attrs: vec![],
            eq_token: syn::Token!(=)(proc_macro2::Span::call_site()),
            left: Box::new(offset_expr.clone()),
            right: Box::new(syn::Expr::Call(syn::ExprCall {
                attrs: vec![],
                func: Box::new(align_func.clone()),
                paren_token: syn::token::Paren {
                    span: proc_macro2::Span::call_site(),
                },
                args: vec![
                    offset_expr.clone(),
                    syn::Expr::Lit(syn::ExprLit {
                        lit: syn::Lit::Int(alignment),
                        attrs: vec![],
                    }),
                ]
                .into_iter()
                .collect::<syn::punctuated::Punctuated<_, syn::Token![,]>>(),
            })),
        }),
        syn::Token!(;)(proc_macro2::Span::call_site()),
    )
}

fn pad_stmt(padding: syn::LitInt, offset_expr: &syn::Expr) -> syn::Stmt {
    syn::Stmt::Semi(
        syn::Expr::AssignOp(syn::ExprAssignOp {
            attrs: vec![],
            left: Box::new(offset_expr.clone()),
            op: syn::BinOp::AddEq(Default::default()),
            right: Box::new(syn::Expr::Lit(syn::ExprLit {
                attrs: vec![],
                lit: syn::Lit::Int(padding),
            })),
        }),
        Default::default(),
    )
}

fn item_to_decl(
    item: Item,
    offset_expr: &syn::Expr,
    align_func: &syn::Expr,
    field_idents: &[syn::Ident],
) -> syn::Stmt {
    match item {
        Item::Align(alignment) => align_stmt(alignment, offset_expr, align_func),
        Item::Pad(padment) => pad_stmt(padment, offset_expr),
        Item::RegularField(field) => syn::Stmt::Local(syn::Local {
            attrs: vec![],
            let_token: syn::Token!(let)(proc_macro2::Span::call_site()),
            pat: simple_pattern(field.ident.clone().unwrap()),
            init: Some((
                syn::Token!(=)(proc_macro2::Span::call_site()),
                Box::new(offset_incr_expr(question_mark_operator_expr(
                    regular_parser_expr(*field),
                ))),
            )),
            semi_token: Default::default(),
        }),

        Item::CustomParsed(field, parser_type, data_expr) => syn::Stmt::Local(syn::Local {
            attrs: vec![],
            let_token: syn::Token!(let)(proc_macro2::Span::call_site()),
            pat: simple_pattern(field.ident.clone().unwrap()),
            init: Some((syn::Token!(=)(proc_macro2::Span::call_site()), {
                let syn::Field { ty, ident, .. } = *field;
                Box::new(offset_incr_expr(question_mark_operator_expr(
                    generic_parser_expr(
                        *parser_type,
                        ty,
                        include_fields(*data_expr, field_idents, false, Some(&ident.unwrap())),
                    ),
                )))
            })),
            semi_token: Default::default(),
        }),

        Item::FieldWithEndianness(_, ident, (endianness, ty)) => syn::Stmt::Local(syn::Local {
            attrs: vec![],
            let_token: Default::default(),
            pat: simple_pattern(ident),
            init: Some((
                Default::default(),
                Box::new(offset_incr_expr(question_mark_operator_expr(pod_expr(
                    endianness, ty,
                )))),
            )),
            semi_token: Default::default(),
        }),

        Item::Conditional(items, condition) => {
            let ident = items.iter().filter_map(Item::ident).next().unwrap().clone();

            syn::Stmt::Local(syn::Local {
                attrs: vec![],
                let_token: Default::default(),
                pat: simple_pattern(
                    Item::Conditional(items.clone(), condition.clone())
                        .ident()
                        .unwrap()
                        .clone(),
                ),
                semi_token: Default::default(),
                init: Some((
                    Default::default(),
                    Box::new(conditional_expr(
                        items,
                        include_fields(*condition, field_idents, false, Some(&ident)),
                        offset_expr,
                        align_func,
                        field_idents,
                    )),
                )),
            })
        }
    }
}

fn generic_serialize(
    field_ident: syn::Ident,
    serializer_type: syn::Type,
    target_type: syn::Type,
    meta_expr: syn::Expr,
    unwrap: bool,
) -> syn::Expr {
    let ty = parser_or_serializer_as_trait(
        serializer_type,
        syn::Ident::new("Serializer", proc_macro2::Span::call_site()),
        target_type,
    );

    let field_expr = syn::Expr::Field(syn::ExprField {
        attrs: vec![],
        dot_token: Default::default(),
        base: Box::new(simple_expr(syn::Ident::new(
            "__sbp_proc_macro_data",
            proc_macro2::Span::call_site(),
        ))),
        member: syn::Member::Named(field_ident),
    });

    let field_ref_expr = if !unwrap {
        syn::Expr::Reference(syn::ExprReference {
            and_token: Default::default(),
            attrs: vec![],
            expr: Box::new(field_expr),
            mutability: None,
            raw: Default::default(),
        })
    } else {
        syn::parse2(quote! { #field_expr.as_ref().unwrap() }).unwrap()
    };

    question_mark_operator_expr(syn::Expr::Call(syn::ExprCall {
        attrs: vec![],
        func: Box::new(regular_serializer_func(&ty)),
        paren_token: Default::default(),
        args: vec![
            field_ref_expr,
            meta_expr,
            syn::Expr::Reference(syn::ExprReference {
                attrs: vec![],
                expr: Box::new(syn::Expr::Index(syn::ExprIndex {
                    attrs: vec![],
                    bracket_token: Default::default(),
                    expr: Box::new(simple_expr(syn::Ident::new(
                        "__sbp_proc_macro_bytes",
                        proc_macro2::Span::call_site(),
                    ))),
                    index: Box::new(syn::Expr::Range(syn::ExprRange {
                        attrs: vec![],
                        limits: syn::RangeLimits::HalfOpen(Default::default()),
                        from: Some(Box::new(simple_expr(syn::Ident::new(
                            "__sbp_proc_macro_offset",
                            proc_macro2::Span::call_site(),
                        )))),
                        to: None,
                    })),
                })),
                mutability: Some(Default::default()),
                and_token: Default::default(),
                raw: Default::default(),
            }),
        ]
        .into_iter()
        .collect::<syn::punctuated::Punctuated<_, syn::Token![,]>>(),
    }))
}

fn field_with_endianness_stmt(
    ident: syn::Ident,
    endianness: Endianness,
    ty: syn::Ident,
    offset_expr: &syn::Expr,
    unwrap: bool,
) -> syn::Stmt {
    syn::Stmt::Semi(
        syn::Expr::AssignOp(syn::ExprAssignOp {
            attrs: vec![],
            left: Box::new(offset_expr.clone()),
            op: syn::BinOp::AddEq(Default::default()),
            right: Box::new(generic_serialize(
                ident,
                pod_combinator_type(endianness),
                simple_type(ty),
                empty_tuple(),
                unwrap,
            )),
        }),
        Default::default(),
    )
}

fn regular_field_stmt(field: syn::Field, offset_expr: &syn::Expr, unwrap: bool) -> syn::Stmt {
    syn::Stmt::Semi(
        syn::Expr::AssignOp(syn::ExprAssignOp {
            attrs: vec![],
            left: Box::new(offset_expr.clone()),
            op: syn::BinOp::AddEq(Default::default()),
            right: Box::new(generic_serialize(
                field.ident.unwrap(),
                field.ty.clone(),
                field.ty,
                empty_tuple(),
                unwrap,
            )),
        }),
        Default::default(),
    )
}

fn include_fields(
    expr: syn::Expr,
    field_idents: &[syn::Ident],
    base: bool,
    self_ident: Option<&syn::Ident>,
) -> syn::Expr {
    if base {
        syn::parse2(quote! {
            {
                #(let #field_idents = &__sbp_proc_macro_data.#field_idents;)*
                #expr
            }
        })
        .unwrap()
    } else {
        let field_idents = field_idents
            .iter()
            .take_while(|item| item != &self_ident.unwrap());
        syn::parse2(quote! {
            {
                #(let #field_idents = &#field_idents;)*
                #expr
            }
        })
        .unwrap()
    }
}

fn custom_parsed_stmt(
    field: syn::Field,
    parser: syn::Type,
    meta_expr: syn::Expr,
    offset_expr: &syn::Expr,
    field_idents: &[syn::Ident],
    unwrap: bool,
) -> syn::Stmt {
    syn::Stmt::Semi(
        syn::Expr::AssignOp(syn::ExprAssignOp {
            attrs: vec![],
            left: Box::new(offset_expr.clone()),
            op: syn::BinOp::AddEq(Default::default()),
            right: Box::new(generic_serialize(
                field.ident.unwrap(),
                parser,
                field.ty,
                include_fields(meta_expr, field_idents, true, None),
                unwrap,
            )),
        }),
        Default::default(),
    )
}

fn conditional_stmt(
    items: Vec<Item>,
    cond: syn::Expr,
    offset_expr: &syn::Expr,
    align_func: &syn::Expr,
    field_idents: &[syn::Ident],
) -> syn::Stmt {
    syn::Stmt::Expr(syn::Expr::If(syn::ExprIf {
        attrs: vec![],
        cond: Box::new(include_fields(cond, field_idents, true, None)),
        else_branch: None,
        if_token: Default::default(),
        then_branch: syn::Block {
            brace_token: Default::default(),
            stmts: items
                .into_iter()
                .map(|item| item_to_stmt(item, offset_expr, align_func, field_idents, true))
                .collect(),
        },
    }))
}

fn item_to_stmt(
    item: Item,
    offset_expr: &syn::Expr,
    align_func: &syn::Expr,
    field_idents: &[syn::Ident],
    unwrap: bool,
) -> syn::Stmt {
    match item {
        Item::Align(alignment) => align_stmt(alignment, offset_expr, align_func),
        Item::Pad(padding) => pad_stmt(padding, offset_expr),
        Item::FieldWithEndianness(_, ident, (endianness, ty)) => {
            field_with_endianness_stmt(ident, endianness, ty, offset_expr, unwrap)
        }
        Item::RegularField(field) => regular_field_stmt(*field, offset_expr, unwrap),
        Item::CustomParsed(field, parser, meta_expr) => custom_parsed_stmt(
            *field,
            *parser,
            *meta_expr,
            offset_expr,
            field_idents,
            unwrap,
        ),
        Item::Conditional(items, cond) => {
            conditional_stmt(items, *cond, offset_expr, align_func, field_idents)
        }
    }
}

fn conditional_expr(
    items: Vec<Item>,
    condition: syn::Expr,
    offset_expr: &syn::Expr,
    align_func: &syn::Expr,
    field_idents: &[syn::Ident],
) -> syn::Expr {
    syn::Expr::If(syn::ExprIf {
        attrs: vec![],
        cond: Box::new(condition),
        if_token: Default::default(),
        then_branch: syn::Block {
            brace_token: Default::default(),
            stmts: items
                .iter()
                .cloned()
                .map(|item| {
                    vec![
                        item_to_decl(item, offset_expr, align_func, field_idents),
                        syn::Stmt::Expr(syn::Expr::Call(syn::ExprCall {
                            attrs: vec![],
                            func: Box::new(syn::Expr::Path(syn::ExprPath {
                                attrs: vec![],
                                qself: None,
                                path: syn::Path {
                                    leading_colon: Some(Default::default()),
                                    segments: vec![
                                        syn::PathSegment {
                                            arguments: syn::PathArguments::None,
                                            ident: syn::Ident::new(
                                                "core",
                                                proc_macro2::Span::call_site(),
                                            ),
                                        },
                                        syn::PathSegment {
                                            arguments: syn::PathArguments::None,
                                            ident: syn::Ident::new(
                                                "option",
                                                proc_macro2::Span::call_site(),
                                            ),
                                        },
                                        syn::PathSegment {
                                            arguments: syn::PathArguments::None,
                                            ident: syn::Ident::new(
                                                "Option",
                                                proc_macro2::Span::call_site(),
                                            ),
                                        },
                                        syn::PathSegment {
                                            arguments: syn::PathArguments::None,
                                            ident: syn::Ident::new(
                                                "Some",
                                                proc_macro2::Span::call_site(),
                                            ),
                                        },
                                    ]
                                    .into_iter()
                                    .collect(),
                                },
                            })),
                            paren_token: Default::default(),
                            args: vec![simple_expr(items.last().unwrap().ident().unwrap().clone())]
                                .into_iter()
                                .collect(),
                        })),
                    ]
                })
                .flatten()
                .collect(),
        },
        else_branch: Some((
            Default::default(),
            Box::new(syn::Expr::Block(syn::ExprBlock {
                attrs: vec![],
                label: None,
                block: syn::Block {
                    brace_token: Default::default(),
                    stmts: vec![syn::Stmt::Expr(syn::Expr::Path(syn::ExprPath {
                        attrs: vec![],
                        qself: None,
                        path: syn::Path {
                            leading_colon: Some(Default::default()),
                            segments: vec![
                                syn::PathSegment {
                                    arguments: syn::PathArguments::None,
                                    ident: syn::Ident::new("core", proc_macro2::Span::call_site()),
                                },
                                syn::PathSegment {
                                    arguments: syn::PathArguments::None,
                                    ident: syn::Ident::new(
                                        "option",
                                        proc_macro2::Span::call_site(),
                                    ),
                                },
                                syn::PathSegment {
                                    arguments: syn::PathArguments::None,
                                    ident: syn::Ident::new(
                                        "Option",
                                        proc_macro2::Span::call_site(),
                                    ),
                                },
                                syn::PathSegment {
                                    arguments: syn::PathArguments::None,
                                    ident: syn::Ident::new("None", proc_macro2::Span::call_site()),
                                },
                            ]
                            .into_iter()
                            .collect(),
                        },
                    }))],
                },
            })),
        )),
    })
}

fn question_mark_operator_expr(to_wrap: syn::Expr) -> syn::Expr {
    syn::Expr::Try(syn::ExprTry {
        attrs: vec![],
        expr: Box::new(to_wrap),
        question_token: Default::default(),
    })
}

fn align_func() -> Box<syn::Expr> {
    Box::new(syn::Expr::Path(syn::ExprPath {
        attrs: vec![],
        qself: None,
        path: syn::Path {
            leading_colon: Some(Default::default()),
            segments: vec![
                syn::PathSegment {
                    ident: syn::Ident::new("sbp", proc_macro2::Span::call_site()),
                    arguments: syn::PathArguments::None,
                },
                syn::PathSegment {
                    ident: syn::Ident::new("align", proc_macro2::Span::call_site()),
                    arguments: syn::PathArguments::None,
                },
            ]
            .into_iter()
            .collect::<syn::punctuated::Punctuated<_, syn::Token![::]>>(),
        },
    }))
}

fn parsable(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as ItemStruct);

    let attrs = ast.attrs.into_iter();

    let items = if let syn::Fields::Named(syn::FieldsNamed { ref named, .. }) = ast.fields {
        named
            .iter()
            .cloned()
            .map(item_from_field)
            .flatten()
            .collect::<Vec<_>>()
    } else {
        panic!()
    };

    let fields = items
        .iter()
        .cloned()
        .filter_map(item_to_field)
        .flatten()
        .collect::<Vec<_>>();

    let align_func = align_func();
    let offset_expr = Box::new(simple_expr(syn::Ident::new(
        "__sbp_proc_macro_offset",
        proc_macro2::Span::call_site(),
    )));
    let fields_idents = fields
        .iter()
        .cloned()
        .map(|field| field.ident.unwrap())
        .collect::<Vec<_>>();

    let declarations = items
        .into_iter()
        .map(|item| item_to_decl(item, &offset_expr, &align_func, &fields_idents));

    let ident = &ast.ident;
    let visibility = &ast.vis;

    let tokens = quote! {
        #(#attrs)*

        #visibility struct #ident {
            #(#fields,)*
        }

        #[allow(unused_parens, unused_variables)]
        impl<'a> ::sbp::Parser<'a, #ident> for #ident {
            type Meta = ();
            type Error = ::sbp::BasicOutOfSpaceError;

            fn parse(_: Self::Meta, __sbp_proc_macro_bytes: &'a [u8]) -> Result<(Self, usize), Self::Error> {
                let mut __sbp_proc_macro_offset = 0;

                #(#declarations)*

                Ok((#ident {
                    #(#fields_idents,)*
                }, __sbp_proc_macro_offset))
            }
        }
    };

    tokens.into()
}

fn serializable(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as ItemStruct);

    let ident = ast.ident;

    let items = if let syn::Fields::Named(syn::FieldsNamed { ref named, .. }) = ast.fields {
        named
            .iter()
            .cloned()
            .map(item_from_field)
            .flatten()
            .collect::<Vec<_>>()
    } else {
        panic!()
    };
    let fields = items
        .iter()
        .cloned()
        .filter_map(item_to_field)
        .flatten()
        .collect::<Vec<_>>();

    let fields_idents = fields
        .iter()
        .cloned()
        .map(|field| field.ident.unwrap())
        .collect::<Vec<_>>();

    let align_func = align_func();
    let offset_expr = Box::new(simple_expr(syn::Ident::new(
        "__sbp_proc_macro_offset",
        proc_macro2::Span::call_site(),
    )));

    let statements = items
        .into_iter()
        .map(|item| item_to_stmt(item, &offset_expr, &align_func, &fields_idents, false));

    let tokens = quote! {
        #[allow(unused_parens, unused_variables)]
        impl<'a> ::sbp::Serializer<'a, #ident> for #ident {
            type Meta = ();
            type Error = ::sbp::BasicOutOfSpaceError;

            fn serialize(__sbp_proc_macro_data: &Self, _: Self::Meta, __sbp_proc_macro_bytes: &'a mut [u8]) -> Result<usize, Self::Error> {
                let mut __sbp_proc_macro_offset = 0;

                #(#statements)*

                Ok(__sbp_proc_macro_offset)
            }
        }
    };
    tokens.into()
}

#[proc_macro_attribute]
pub fn sbp(attr: TokenStream, input: TokenStream) -> TokenStream {
    let attr = proc_macro2::TokenStream::from(attr);

    let mut output = proc_macro::TokenStream::from(quote! {});

    // TODO: It doesn't seem like syn has a parser for the args of an attribute-like proc macro.
    let args = attr.into_iter().collect::<Vec<_>>();

    // TODO: Flattening here isn't really correct.
    for arg in args
        .split(|tt| {
            if let proc_macro2::TokenTree::Punct(_) = tt {
                true
            } else {
                false
            }
        })
        .flatten()
    {
        match arg {
            proc_macro2::TokenTree::Ident(ident) => {
                if ident == &syn::Ident::new("parsable", proc_macro2::Span::call_site()) {
                    output.extend(parsable(input.clone()));
                } else if ident == &syn::Ident::new("serializable", proc_macro2::Span::call_site())
                {
                    output.extend(serializable(input.clone()));
                }
            }
            _ => panic!(),
        }
    }

    output
}
