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
}

#[derive(Debug)]
struct AttrParseError;

fn field_attribute_items(field: &syn::Field) -> Vec<Item> {
    field
        .attrs
        .iter()
        .map(|attr| {
            let segments = match attr.path {
                syn::Path {
                    leading_colon: None,
                    ref segments,
                } => segments,
                _ => return Err(AttrParseError),
            };

            if segments.len() != 1 {
                return Err(AttrParseError);
            }

            let group = syn::parse2::<proc_macro2::Group>(attr.tokens.clone())
                .map_err(|_| AttrParseError)?;

            let number = if let syn::Lit::Int(int) =
                syn::parse2::<syn::Lit>(group.stream()).map_err(|_| AttrParseError)?
            {
                int
            } else {
                return Err(AttrParseError);
            };

            let first_segment = segments.first().unwrap();

            if first_segment.ident == syn::Ident::new("align", proc_macro2::Span::call_site()) {
                Ok(Item::Align(number))
            } else if first_segment.ident == syn::Ident::new("pad", proc_macro2::Span::call_site())
            {
                Ok(Item::Pad(number))
            } else {
                Err(AttrParseError)
            }
        })
        .collect::<Result<Vec<_>, _>>()
        .unwrap()
}

fn item_from_field(field: syn::Field) -> Vec<Item> {
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

fn regular_parser_expr(field: &syn::Field) -> syn::Expr {
    // Assume that every type this function is called for is sbp::Parse, rather than sbp::Parser.
    syn::Expr::Call(syn::ExprCall {
        attrs: vec![],
        func: Box::new(regular_parser_func(&field.ty)),
        paren_token: syn::token::Paren {
            span: proc_macro2::Span::call_site(),
        },
        args: Vec::<syn::Expr>::new()
            .into_iter()
            .collect::<syn::punctuated::Punctuated<_, syn::Token![,]>>(),
    })
}

fn pattern(ident: syn::Ident) -> syn::Pat {
    syn::Pat::Tuple(syn::PatTuple {
        attrs: vec![],
        paren_token: Default::default(),
        elems: vec![
            syn::Pat::Ident(syn::PatIdent {
                attrs: vec![],
                by_ref: None,
                mutability: None,
                ident,
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

fn pod_expr(endianness: Endianness, ty: &syn::Ident) -> syn::Expr {
    // plain old data type expression
    syn::Expr::Call(syn::ExprCall {
        attrs: vec![],
        func: Box::new(regular_parser_func(&syn::Type::Path(syn::TypePath {
            qself: Some(syn::QSelf {
                as_token: Some(syn::Token!(as)(proc_macro2::Span::call_site())),
                lt_token: syn::Token!(<)(proc_macro2::Span::call_site()),
                gt_token: syn::Token!(>)(proc_macro2::Span::call_site()),
                ty: Box::new(syn::Type::Path(syn::TypePath {
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
                })),
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
                    expr: Box::new(simple_expr(syn::Ident::new("bytes", proc_macro2::Span::call_site()))),
                    index: Box::new(syn::Expr::Range(syn::ExprRange {
                        attrs: vec! [],
                        limits: syn::RangeLimits::HalfOpen(Default::default()),
                        from: Some(Box::new(simple_expr(syn::Ident::new("offset", proc_macro2::Span::call_site())))),
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

fn offset_incr_expr(ident: syn::Ident, to_wrap: syn::Expr) -> syn::Expr {
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
                    pat: pattern(ident.clone()),
                }),
                syn::Stmt::Semi(
                    syn::Expr::AssignOp(syn::ExprAssignOp {
                        attrs: vec![],
                        op: syn::BinOp::AddEq(Default::default()),
                        left: Box::new(syn::Expr::Path(syn::ExprPath {
                            attrs: vec![],
                            qself: None,
                            path: simple_path(syn::Ident::new(
                                "offset",
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
                syn::Stmt::Expr(simple_expr(ident)),
            ],
        },
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
        .filter_map(|item| match item {
            Item::Align(_) => None,
            Item::Pad(_) => None,
            Item::FieldWithEndianness(vis, ident, (_, ty)) => Some(syn::Field {
                attrs: vec![],
                ident: Some(ident),
                vis,
                ty: syn::Type::Path(syn::TypePath {
                    qself: None,
                    path: syn::Path::from(ty),
                }),
                colon_token: None,
            }),
            Item::RegularField(field) => Some(field),
        })
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
        path: syn::Path::from(syn::Ident::new("offset", proc_macro2::Span::call_site())),
    }));

    let declarations = items.iter().cloned().map(|item| match item {
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
                        (*offset_expr).clone(),
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
            pat: pattern(field.ident.clone().unwrap()),
            init: Some((
                syn::Token!(=)(proc_macro2::Span::call_site()),
                Box::new(offset_incr_expr(
                    field.ident.clone().unwrap(),
                    regular_parser_expr(&field),
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
                    ident,
                    question_mark_operator_expr(pod_expr(endianness, &ty)),
                )),
            )),
            semi_token: syn::Token!(;)(proc_macro2::Span::call_site()),
        }),
    });

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

            fn parse(_: Self::Data, bytes: &'a [u8]) -> Result<(Self, usize), Self::Error> {
                let mut offset = 0;

                #(#declarations)*

                Ok((#ident {
                    #(#fields_idents,)*
                }, offset))
            }
        }
    };

    tokens.into()
}
