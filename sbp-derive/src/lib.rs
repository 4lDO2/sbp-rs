extern crate proc_macro;

use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, ItemStruct};

#[derive(Clone, Copy)]
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

#[derive(Clone)]
enum Item {
    RegularField(syn::Field),
    FieldWithEndianness(syn::Visibility, syn::Ident, (Endianness, syn::Ident)),
    Align(syn::LitInt),
    Pad(syn::LitInt),
    Conditional(Vec<Item>, syn::Expr),
    CustomParsed(syn::Field, syn::Type, syn::Expr),
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
    let group = syn::parse2::<proc_macro2::Group>(tokens.clone())
        .map_err(|_| AttrParseError)?;

    if let syn::Lit::Int(int) =
        syn::parse2::<syn::Lit>(group.stream()).map_err(|_| AttrParseError)?
    {
        Ok(int)
    } else {
        return Err(AttrParseError);
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
            } else if attr_path_ident == &syn::Ident::new("pad", proc_macro2::Span::call_site())
            {
                Ok(Item::Pad(parse_number(&attr.tokens)?))
            } else {
                Err(AttrParseError)
            }
        })
        .collect::<Result<Vec<_>, _>>()
        .unwrap()
}

fn match_field_conditional(field: &mut syn::Field) -> Option<syn::Expr> {
    let (position, _, tokens) = match field.attrs.iter().enumerate().filter_map(|(idx, attr)| {
        attr.path.get_ident().map(|ident| (idx, ident, attr.tokens.clone()))
    }).find(|(_, ident, _)| {
        ident == &&syn::Ident::new("condition", proc_macro2::Span::call_site())
    }) {
        Some(p) => p,
        None => return None,
    };
    field.attrs.remove(position);
    Some(syn::parse2::<syn::Expr>(tokens).unwrap())
}

fn match_field_custom(field: &mut syn::Field) -> Option<(syn::Type, syn::Expr)> {
    let (position, _, tokens) = match field.attrs.iter().enumerate().filter_map(|(idx, attr)| {
        attr.path.get_ident().map(|ident| (idx, ident, attr.tokens.clone()))
    }).find(|(_, ident, _)| {
        ident == &&syn::Ident::new("custom", proc_macro2::Span::call_site())
    }) {
        Some(p) => p,
        None => return None,
    };
    field.attrs.remove(position);

    let tuple = syn::parse2::<syn::ExprTuple>(tokens).unwrap();
    let mut iterator = tuple.elems.into_pairs().map(syn::punctuated::Pair::into_value);

    let path = if let Some(syn::Expr::Path(syn::ExprPath { path, .. })) = iterator.next() {
        syn::Type::Path(syn::TypePath {
            qself: None,
            path,
        })
    } else {
        return None;
    };

    let data_expr = if let Some(data_expr) = iterator.next() {
        data_expr
    } else {
        return None;
    };

    if iterator.next() != None { return None; }

    Some((path, data_expr))
}

fn item_from_field(mut field: syn::Field) -> Vec<Item> {
    if let Some(condition) = match_field_conditional(&mut field) {
        return vec! [Item::Conditional(item_from_field(field), condition)];
    } else if let Some((ty, data)) = match_field_custom(&mut field) {
        return vec! [Item::CustomParsed(field, ty, data)];
    }

    let mut items: Vec<Item> = vec![];
    items.extend(field_attribute_items(&field));

    items.push(
        if let Some((endianness, ty)) = match_endianness_specifier(&field) {
            Item::FieldWithEndianness(field.vis, field.ident.unwrap(), (endianness, ty))
        } else {
            Item::RegularField(field)
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
        qself: qself,
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
    generic_parser_expr(
        field.ty.clone(), field.ty,
        empty_tuple(),
    )
}

fn generic_parser_expr(parser_type: syn::Type, target_type: syn::Type, data_expr: syn::Expr) -> syn::Expr {
    // Assume that every type this function is called for is sbp::Parse, rather than sbp::Parser.

    let position = 2;

    let ty = syn::Type::Path(syn::TypePath {
        qself: Some(syn::QSelf {
            as_token: Some(syn::Token!(as)(proc_macro2::Span::call_site())),
            lt_token: syn::Token!(<)(proc_macro2::Span::call_site()),
            gt_token: syn::Token!(>)(proc_macro2::Span::call_site()),
            ty: Box::new(parser_type),
            position,
        }),
        path: syn::Path {
            leading_colon: Some(syn::Token!(::)(proc_macro2::Span::call_site())),
            segments: vec![
                syn::PathSegment {
                    ident: syn::Ident::new("sbp", proc_macro2::Span::call_site()),
                    arguments: syn::PathArguments::None,
                },
                syn::PathSegment {
                    ident: syn::Ident::new("Parser", proc_macro2::Span::call_site()),
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
    });

    syn::Expr::Call(syn::ExprCall {
        attrs: vec![],
        func: Box::new(regular_parser_func(&ty)),
        paren_token: syn::token::Paren {
            span: proc_macro2::Span::call_site(),
        },
        args: vec! [
            data_expr,
            syn::Expr::Reference(syn::ExprReference {
                attrs: vec! [],
                expr: Box::new(syn::Expr::Index(syn::ExprIndex {
                    attrs: vec! [],
                    bracket_token: Default::default(),
                    expr: Box::new(simple_expr(syn::Ident::new("__sbp_proc_macro_bytes", proc_macro2::Span::call_site()))),
                    index: Box::new(syn::Expr::Range(syn::ExprRange {
                        attrs: vec! [],
                        limits: syn::RangeLimits::HalfOpen(Default::default()),
                        from: Some(Box::new(simple_expr(syn::Ident::new("__sbp_proc_macro_offset", proc_macro2::Span::call_site())))),
                        to: None,
                    })),
                })),
                mutability: None,
                and_token: Default::default(),
                raw: Default::default(),
            }),
        ].into_iter().collect::<syn::punctuated::Punctuated<_, syn::Token![,]>>(),
    })
}

fn optionize(ty: syn::Type) -> syn::Type {
    syn::Type::Path(syn::TypePath {
        qself: None,
        path: syn::Path {
            leading_colon: Some(Default::default()),
            segments: vec! [
                syn::PathSegment {
                    arguments: syn::PathArguments::None,
                    ident: syn::Ident::new("core", proc_macro2::Span::call_site()),
                },
                syn::PathSegment {
                    arguments: syn::PathArguments::None,
                    ident: syn::Ident::new("option", proc_macro2::Span::call_site()),
                },
                syn::PathSegment {
                    arguments: syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments {
                        colon2_token: None,
                        gt_token: Default::default(),
                        lt_token: Default::default(),
                        args: vec!(syn::GenericArgument::Type(ty)).into_iter().collect(),
                    }),
                    ident: syn::Ident::new("Option", proc_macro2::Span::call_site()),
                },
            ].into_iter().collect(),
        },
    })
}

fn item_to_field(item: Item) -> Option<Vec<syn::Field>> {
    fn inner(item: Item, will_optionize: bool) -> Option<Vec<syn::Field>> {
        match item {
            Item::Align(_) => None,
            Item::Pad(_) => None,
            Item::FieldWithEndianness(vis, ident, (_, ty)) => Some(vec!(syn::Field {
                attrs: vec![],
                ident: Some(ident),
                vis,
                ty: if !will_optionize {
                    syn::Type::Path(syn::TypePath {
                        qself: None,
                        path: syn::Path::from(ty),
                    })
                } else {
                    optionize(
                        syn::Type::Path(syn::TypePath {
                            qself: None,
                            path: syn::Path::from(ty),
                        })
                    )
                },
                colon_token: None,
            })),
            Item::RegularField(field) => Some(vec![
                if !will_optionize {
                    field
                } else {
                    syn::Field {
                        attrs: field.attrs,
                        colon_token: field.colon_token,
                        ident: field.ident,
                        vis: field.vis,
                        ty: optionize(field.ty),
                    }
                }
            ]),
            Item::Conditional(items, _) => Some(items.into_iter().filter_map(|item| inner(item, true)).flatten().collect()),
            Item::CustomParsed(field, _, _) => Some(vec!(field)),
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
        ident: ident,
        subpat: None,
    })
}

fn simple_path(ident: syn::Ident) -> syn::Path {
    syn::Path {
        leading_colon: None,
        segments: std::iter::once(syn::PathSegment {
            ident: ident.clone(),
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

fn pod_expr(endianness: Endianness, ty: syn::Ident) -> syn::Expr {
    generic_parser_expr(
        syn::Type::Path(syn::TypePath {
            qself: None,
            path: syn::Path {
                leading_colon: Some(syn::Token!(::)(proc_macro2::Span::call_site())),
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
        }),
        simple_type(ty),
        empty_tuple(),
    )
}

/*fn pod_expr(endianness: Endianness, ty: &syn::Ident) -> syn::Expr {
    // plain old data type expression
    syn::Expr::Call(syn::ExprCall {
        attrs: vec![],
        func: Box::new(regular_parser_func(&syn::Type::Path(syn::TypePath {
            qself: Some(syn::QSelf {
                as_token: Some(syn::Token!(as)(proc_macro2::Span::call_site())),
                lt_token: syn::Token!(<)(proc_macro2::Span::call_site()),
                gt_token: syn::Token!(>)(proc_macro2::Span::call_site()),
                ty: ,
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
                        ident: syn::Ident::new("Parser", proc_macro2::Span::call_site()),
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
                                    syn::GenericArgument::Type(simple_type(ty.clone())),
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
        }))),
        paren_token: syn::token::Paren {
            span: proc_macro2::Span::call_site(),
        },
        args: vec![
            syn::Expr::Tuple(syn::ExprTuple {
                attrs: vec![],
                paren_token: syn::token::Paren {
                    span: proc_macro2::Span::call_site(),
                },
                elems: syn::punctuated::Punctuated::new(),
            }),
            syn::Expr::Reference(syn::ExprReference {
                attrs: vec! [],
                expr: Box::new(syn::Expr::Index(syn::ExprIndex {
                    attrs: vec! [],
                    bracket_token: Default::default(),
                    expr: Box::new(simple_expr(syn::Ident::new("__sbp_proc_macro_bytes", proc_macro2::Span::call_site()))),
                    index: Box::new(syn::Expr::Range(syn::ExprRange {
                        attrs: vec! [],
                        limits: syn::RangeLimits::HalfOpen(Default::default()),
                        from: Some(Box::new(simple_expr(syn::Ident::new("__sbp_proc_macro_offset", proc_macro2::Span::call_site())))),
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
*/

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
                    init: Some((Default::default(), Box::new(to_wrap.clone()))),
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
                syn::Stmt::Expr(simple_expr(syn::Ident::new("value", proc_macro2::Span::call_site()))),
            ],
        },
    })
}

fn item_to_decl(item: Item, offset_expr: &Box<syn::Expr>, align_func: &Box<syn::Expr>) -> syn::Stmt {
    match item {
        Item::Align(alignment) => syn::Stmt::Semi(
            syn::Expr::Assign(syn::ExprAssign {
                attrs: vec![],
                eq_token: syn::Token!(=)(proc_macro2::Span::call_site()),
                left: offset_expr.clone(),
                right: Box::new(syn::Expr::Call(syn::ExprCall {
                    attrs: vec![],
                    func: align_func.clone(),
                    paren_token: syn::token::Paren {
                        span: proc_macro2::Span::call_site(),
                    },
                    args: vec![
                        (**offset_expr).clone(),
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
        ),
        Item::Pad(padment) => syn::Stmt::Semi(
            syn::Expr::AssignOp(syn::ExprAssignOp {
                attrs: vec![],
                left: offset_expr.clone(),
                op: syn::BinOp::AddEq(syn::Token!(+=)(proc_macro2::Span::call_site())),
                right: Box::new(syn::Expr::Lit(syn::ExprLit {
                    attrs: vec![],
                    lit: syn::Lit::Int(padment),
                })),
            }),
            syn::Token!(;)(proc_macro2::Span::call_site()),
        ),
        Item::RegularField(field) => syn::Stmt::Local(syn::Local {
            attrs: vec![],
            let_token: syn::Token!(let)(proc_macro2::Span::call_site()),
            pat: simple_pattern(field.ident.clone().unwrap()),
            init: Some((
                syn::Token!(=)(proc_macro2::Span::call_site()),
                Box::new(offset_incr_expr(
                    question_mark_operator_expr(regular_parser_expr(field)),
                )),
            )),
            semi_token: syn::Token!(;)(proc_macro2::Span::call_site()),
        }),

        Item::CustomParsed(field, parser_type, data_expr) => syn::Stmt::Local(syn::Local {
            attrs: vec![],
            let_token: syn::Token!(let)(proc_macro2::Span::call_site()),
            pat: simple_pattern(field.ident.unwrap()),
            init: Some((
                syn::Token!(=)(proc_macro2::Span::call_site()),
                Box::new(offset_incr_expr(
                    question_mark_operator_expr(generic_parser_expr(parser_type, field.ty, data_expr)),
                )),
            )),
            semi_token: syn::Token!(;)(proc_macro2::Span::call_site()),
        }),

        Item::FieldWithEndianness(_, ident, (endianness, ty)) => syn::Stmt::Local(syn::Local {
            attrs: vec![],
            let_token: syn::Token!(let)(proc_macro2::Span::call_site()),
            pat: simple_pattern(ident.clone()),
            init: Some((
                syn::Token!(=)(proc_macro2::Span::call_site()),
                Box::new(offset_incr_expr(
                    question_mark_operator_expr(pod_expr(endianness, ty)),
                )),
            )),
            semi_token: syn::Token!(;)(proc_macro2::Span::call_site()),
        }),

        Item::Conditional(items, condition) => syn::Stmt::Local(syn::Local {
            attrs: vec! [],
            let_token: Default::default(),
            pat: simple_pattern(Item::Conditional(items.clone(), condition.clone()).ident().unwrap().clone()),
            semi_token: Default::default(),
            init: Some((Default::default(), Box::new(conditional_expr(items, condition, offset_expr, align_func)))),
        }),
    }
}

fn conditional_expr(items: Vec<Item>, condition: syn::Expr, offset_expr: &Box<syn::Expr>, align_func: &Box<syn::Expr>) -> syn::Expr {
    syn::Expr::If(syn::ExprIf {
        attrs: vec! [],
        cond: Box::new(condition),
        if_token: Default::default(),
        then_branch: syn::Block {
            brace_token: Default::default(),
            stmts: items.iter().cloned().map(|item| {
                vec! [
                    item_to_decl(item, offset_expr, align_func),
                    syn::Stmt::Expr(syn::Expr::Call(syn::ExprCall {
                        attrs: vec! [],
                        func: Box::new(syn::Expr::Path(syn::ExprPath {
                            attrs: vec! [],
                            qself: None,
                            path: syn::Path {
                                leading_colon: Some(Default::default()),
                                segments: vec! [
                                    syn::PathSegment {
                                        arguments: syn::PathArguments::None,
                                        ident: syn::Ident::new("core", proc_macro2::Span::call_site()),
                                    },
                                    syn::PathSegment {
                                        arguments: syn::PathArguments::None,
                                        ident: syn::Ident::new("option", proc_macro2::Span::call_site()),
                                    },
                                    syn::PathSegment {
                                        arguments: syn::PathArguments::None,
                                        ident: syn::Ident::new("Option", proc_macro2::Span::call_site()),
                                    },
                                    syn::PathSegment {
                                        arguments: syn::PathArguments::None,
                                        ident: syn::Ident::new("Some", proc_macro2::Span::call_site()),
                                    },
                                ].into_iter().collect(),
                            },
                        })),
                        paren_token: Default::default(),
                        args: vec! [
                            simple_expr(items.last().unwrap().ident().unwrap().clone())
                        ].into_iter().collect(),
                    })),
                ]
            }).flatten().collect(),
        },
        else_branch: Some((Default::default(), Box::new(syn::Expr::Block(syn::ExprBlock {
            attrs: vec! [],
            label: None,
            block: syn::Block {
                brace_token: Default::default(),
                stmts: vec![
                    syn::Stmt::Expr(syn::Expr::Path(syn::ExprPath {
                        attrs: vec! [],
                        qself: None,
                        path: syn::Path {
                            leading_colon: Some(Default::default()),
                            segments: vec! [
                                syn::PathSegment {
                                    arguments: syn::PathArguments::None,
                                    ident: syn::Ident::new("core", proc_macro2::Span::call_site()),
                                },
                                syn::PathSegment {
                                    arguments: syn::PathArguments::None,
                                    ident: syn::Ident::new("option", proc_macro2::Span::call_site()),
                                },
                                syn::PathSegment {
                                    arguments: syn::PathArguments::None,
                                    ident: syn::Ident::new("Option", proc_macro2::Span::call_site()),
                                },
                                syn::PathSegment {
                                    arguments: syn::PathArguments::None,
                                    ident: syn::Ident::new("None", proc_macro2::Span::call_site()),
                                },
                            ].into_iter().collect(),
                        },
                    }))
                ],
            },
        })))),
    })
}

fn question_mark_operator_expr(to_wrap: syn::Expr) -> syn::Expr {
    syn::Expr::Try(syn::ExprTry {
        attrs: vec![],
        expr: Box::new(to_wrap),
        question_token: Default::default(),
    })
}

#[proc_macro_attribute]
pub fn parsable(_attr: TokenStream, input: TokenStream) -> TokenStream {
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
        .filter_map(item_to_field).flatten()
        .collect::<Vec<_>>();

    let align_func = Box::new(syn::Expr::Path(syn::ExprPath {
        attrs: vec![],
        qself: None,
        path: syn::Path {
            leading_colon: Some(syn::Token!(::)(proc_macro2::Span::call_site())),
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
    }));

    let offset_expr = Box::new(syn::Expr::Path(syn::ExprPath {
        attrs: vec![],
        qself: None,
        path: syn::Path::from(syn::Ident::new("__sbp_proc_macro_offset", proc_macro2::Span::call_site())),
    }));

    let declarations = items.iter().cloned().map(|item| item_to_decl(item, &offset_expr, &align_func));

    let ident = &ast.ident;
    let visibility = &ast.vis;

    let fields_idents = fields.iter().map(|field| &field.ident);

    let tokens = quote! {
        #(#attrs)*

        #visibility struct #ident {
            #(#fields,)*
        }

        impl<'a> ::sbp::Parser<'a, #ident> for #ident {
            type Data = ();
            type Error = ::sbp::BasicParseError;

            fn parse(_: Self::Data, __sbp_proc_macro_bytes: &'a [u8]) -> Result<(Self, usize), Self::Error> {
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
