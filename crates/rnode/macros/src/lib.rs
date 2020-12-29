#![feature(box_syntax)]

extern crate proc_macro;

use pmutil::q;
use pmutil::smart_quote;
use pmutil::IdentExt;
use pmutil::Quote;
use pmutil::SpanExt;
use proc_macro2::Span;
use std::todo;
use syn::fold::fold_type;
use syn::fold::Fold;
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::Arm;
use syn::Attribute;
use syn::Block;
use syn::Expr;
use syn::ExprBlock;
use syn::ExprMatch;
use syn::Field;
use syn::Fields;
use syn::FieldsNamed;
use syn::FieldsUnnamed;
use syn::GenericArgument;
use syn::Ident;
use syn::Item;
use syn::ItemEnum;
use syn::ItemStruct;
use syn::Pat;
use syn::PatIdent;
use syn::Path;
use syn::PathArguments;
use syn::Stmt;
use syn::Token;
use syn::Type;
use syn::TypePath;
use syn::Variant;
use syn::VisPublic;
use syn::Visibility;

///
/// # Struct attributed
///
/// ## `#[skip_node_id]`
///
/// Don't inject `node_id`.
#[proc_macro]
pub fn define_rnode(module: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let module: Block = syn::parse(module).expect("#[rnode] should be applied to a module");

    let mut nodes_to_convert = vec![];
    let mut node_names = vec![];
    for stmt in &module.stmts {
        match stmt {
            Stmt::Item(item) => match item {
                Item::Enum(e) => {
                    nodes_to_convert.push(e.ident.to_string());
                    node_names.push(e.ident.clone());
                }
                Item::Struct(s) => {
                    nodes_to_convert.push(s.ident.to_string());
                    node_names.push(s.ident.clone());
                }
                _ => {}
            },
            _ => unreachable!("Sttm other than item in rnode definition"),
        }
    }

    let mut tts = q!({});

    for stmt in module.stmts {
        match stmt {
            Stmt::Item(item) => {
                let generated_items = handle_item(&nodes_to_convert, item);
                for i in generated_items {
                    tts.push_tokens(&i);
                }
            }
            _ => unreachable!("Sttm other than item in rnode definition"),
        }
    }

    tts.into()
}

fn handle_item(nodes_to_convert: &[String], item: Item) -> Vec<Item> {
    let mut gen = vec![];

    match item {
        Item::Enum(e) => {
            let enum_name = e.ident.new_ident_with(|s| format!("R{}", s));
            let info = e
                .variants
                .iter()
                .map(|v| (v, handle_variant(v)))
                .collect::<Vec<_>>();

            let mut variants = Punctuated::<_, Token![,]>::default();
            let (mut from_orig_arms, mut to_orig_arms) = (vec![], vec![]);

            for (variant, info) in info {
                variants.push(Variant {
                    attrs: variant.attrs.clone(),
                    ident: variant.ident.clone(),
                    fields: info.fields,
                    discriminant: None,
                });

                let (from_orig, to_orig) = handle_enum_variant_fields(
                    nodes_to_convert,
                    Some(&e.ident),
                    &variant.ident,
                    &variant.fields,
                );
                from_orig_arms.push(from_orig);
                to_orig_arms.push(to_orig);
            }

            gen.push(Item::Enum(ItemEnum {
                attrs: take_attrs(q!({
                    #[derive(
                        Debug,
                        Clone,
                        PartialEq,
                        ::swc_common::FromVariant,
                        ::swc_common::Spanned,
                        ::swc_common::EqIgnoreSpan,
                        ::rnode::Visit,
                    )]
                    struct Dummy;
                })),
                vis: e.vis.clone(),
                enum_token: e.enum_token,
                ident: enum_name.clone(),
                generics: Default::default(),
                brace_token: e.brace_token,
                variants,
            }));

            let from_orig_body = Expr::Match(ExprMatch {
                attrs: vec![],
                match_token: Span::call_site().as_token(),
                expr: q!({ orig }).parse(),
                brace_token: Span::call_site().as_token(),
                arms: from_orig_arms,
            });

            let to_orig_body = Expr::Match(ExprMatch {
                attrs: vec![],
                match_token: Span::call_site().as_token(),
                expr: q!({ self }).parse(),
                brace_token: Span::call_site().as_token(),
                arms: to_orig_arms,
            });

            gen.push(
                q!(
                    Vars {
                        REnum: enum_name,
                        OrigType: &e.ident,
                        from_orig_body,
                        to_orig_body,
                    },
                    {
                        #[automatically_derived]
                        impl rnode::RNode for REnum {
                            type Orig = OrigType;

                            fn from_orig(
                                id_gen: &mut rnode::NodeIdGenerator,
                                orig: Self::Orig,
                            ) -> Self {
                                from_orig_body
                            }

                            fn into_orig(self) -> Self::Orig {
                                to_orig_body
                            }
                        }
                    }
                )
                .parse(),
            );
        }
        Item::Struct(s) => {
            //
            match &s.fields {
                Fields::Named(fields) => {
                    let (from_orig_arm, to_orig_arm) =
                        handle_struct_fields(&s.attrs, nodes_to_convert, &s.ident, &s.fields);
                    let field_rnode_info = fields
                        .named
                        .iter()
                        .map(|f| {
                            (
                                f,
                                handle_field(
                                    nodes_to_convert,
                                    &f.attrs,
                                    f.ident.as_ref().unwrap(),
                                    &f.ty,
                                ),
                            )
                        })
                        .collect::<Vec<_>>();

                    let orig_name = s.ident.clone();
                    let struct_name = s.ident.new_ident_with(|name| format!("R{}", name));

                    let mut named = Punctuated::<_, Token![,]>::default();
                    if !skip_node_id(&s.attrs) {
                        named.push(Field {
                            attrs: Default::default(),
                            vis: Visibility::Public(VisPublic {
                                pub_token: struct_name.span().as_token(),
                            }),
                            ident: Some(orig_name.new_ident_with(|_| "node_id")),
                            colon_token: Some(struct_name.span().as_token()),
                            ty: Quote::new(orig_name.span())
                                .quote_with(smart_quote!(Vars {}, (::rnode::NodeId)))
                                .parse(),
                        });
                    }

                    for (field, info) in field_rnode_info {
                        named.push(Field {
                            attrs: field
                                .attrs
                                .iter()
                                .filter(|attr| {
                                    !attr.path.is_ident("rc") && !attr.path.is_ident("refcell")
                                })
                                .cloned()
                                .collect(),
                            vis: field.vis.clone(),
                            ident: field.ident.clone(),
                            colon_token: field.colon_token,
                            ty: info.ty,
                        });
                    }

                    gen.push(Item::Struct(ItemStruct {
                        attrs: take_attrs(q!({
                            #[derive(
                                Debug,
                                Clone,
                                PartialEq,
                                ::swc_common::Spanned,
                                ::swc_common::EqIgnoreSpan,
                                ::rnode::Visit,
                            )]
                            struct Dummy;
                        })),
                        vis: syn::Visibility::Public(VisPublic {
                            pub_token: struct_name.span().as_token(),
                        }),
                        struct_token: s.struct_token,
                        ident: struct_name.clone(),
                        generics: Default::default(),
                        fields: Fields::Named(FieldsNamed {
                            brace_token: fields.brace_token,
                            named,
                        }),
                        semi_token: None,
                    }));

                    gen.push(
                        q!(
                            Vars {
                                RStruct: &struct_name,
                                OrigType: &orig_name,
                                from_orig_arm,
                                to_orig_arm,
                            },
                            {
                                #[automatically_derived]
                                impl rnode::RNode for RStruct {
                                    type Orig = OrigType;

                                    fn from_orig(
                                        id_gen: &mut rnode::NodeIdGenerator,
                                        orig: Self::Orig,
                                    ) -> Self {
                                        match orig {
                                            from_orig_arm
                                        }
                                    }

                                    fn into_orig(self) -> Self::Orig {
                                        match self {
                                            to_orig_arm
                                        }
                                    }
                                }
                            }
                        )
                        .parse(),
                    );
                }
                Fields::Unnamed(_) => todo!("Tuple structs"),
                Fields::Unit => return vec![],
            }
        }
        _ => unreachable!("Item other than sturct or enum in rnode definition"),
    }

    gen
}

/// Cretes `(from_orig_arm, to_orig_arm)`
fn handle_enum_variant_fields(
    nodes_to_convert: &[String],
    enum_name: Option<&Ident>,
    variant_name: &Ident,
    f: &Fields,
) -> (Arm, Arm) {
    let mut from_orig_body: Vec<Stmt> = vec![];
    let mut to_orig_body: Vec<Stmt> = vec![];

    let mut from_orig_bindings = Punctuated::<_, Token![,]>::default();
    let mut to_orig_bindings = Punctuated::<_, Token![,]>::default();

    let mut bindings = Punctuated::<_, Token![,]>::default();

    for (idx, field, info) in f.iter().enumerate().map(|(idx, field)| {
        (
            idx,
            field,
            handle_field(
                nodes_to_convert,
                &field.attrs,
                &field
                    .ident
                    .clone()
                    .unwrap_or(Ident::new(&format!("_{}", idx), field.ident.span())),
                &field.ty,
            ),
        )
    }) {
        let binding = &field
            .ident
            .clone()
            .unwrap_or(Ident::new(&format!("_{}", idx), field.ident.span()));

        bindings.push(binding.clone());

        from_orig_body.push(
            q!(
                Vars {
                    binding,
                    expr: &info.from_orig
                },
                {
                    let binding = expr;
                }
            )
            .parse(),
        );
        to_orig_body.push(
            q!(
                Vars {
                    binding,
                    expr: &info.to_orig
                },
                {
                    let binding = expr;
                }
            )
            .parse(),
        );

        from_orig_bindings.push(Pat::Ident(PatIdent {
            attrs: vec![],
            by_ref: Default::default(),
            mutability: Default::default(),
            ident: binding.clone(),
            subpat: None,
        }));
        to_orig_bindings.push(Pat::Ident(PatIdent {
            attrs: vec![],
            by_ref: Default::default(),
            mutability: Default::default(),
            ident: binding.clone(),
            subpat: None,
        }));
    }

    from_orig_body.push(
        q!(
            Vars {
                Variant: variant_name,
                bindings: &bindings,
            },
            {
                return Self::Variant(bindings);
            }
        )
        .parse(),
    );
    to_orig_body.push(
        q!(
            Vars {
                OrigType: &enum_name,
                Variant: variant_name,
                bindings: &bindings,
            },
            {
                return OrigType::Variant(bindings);
            }
        )
        .parse(),
    );

    let from_orig_arm = Arm {
        attrs: Default::default(),
        pat: q!(
            Vars {
                OrigType: &enum_name,
                Variant: &variant_name,
                bindings: &from_orig_bindings,
            },
            (OrigType::Variant(bindings))
        )
        .parse(),
        guard: None,
        fat_arrow_token: variant_name.span().as_token(),
        body: box Expr::Block(ExprBlock {
            attrs: Default::default(),
            label: Default::default(),
            block: Block {
                brace_token: variant_name.span().as_token(),
                stmts: from_orig_body,
            },
        }),
        comma: Some(variant_name.span().as_token()),
    };
    let to_orig_arm = Arm {
        attrs: Default::default(),
        pat: q!(
            Vars {
                Variant: &variant_name,
                bindings: &to_orig_bindings,
            },
            (Self::Variant(bindings))
        )
        .parse(),
        guard: None,
        fat_arrow_token: variant_name.span().as_token(),
        body: box Expr::Block(ExprBlock {
            attrs: Default::default(),
            label: Default::default(),
            block: Block {
                brace_token: variant_name.span().as_token(),
                stmts: to_orig_body,
            },
        }),
        comma: Some(variant_name.span().as_token()),
    };

    (from_orig_arm, to_orig_arm)
}

fn skip_node_id(attrs: &[Attribute]) -> bool {
    attrs.iter().any(|attr| attr.path.is_ident("skip_node_id"))
}

/// Cretes `(from_orig_arm, to_orig_arm)`
fn handle_struct_fields(
    attrs: &[Attribute],
    nodes_to_convert: &[String],
    struct_name: &Ident,
    f: &Fields,
) -> (Arm, Arm) {
    let skip_node_id = skip_node_id(attrs);

    let mut from_orig_body: Vec<Stmt> = vec![];
    let mut to_orig_body: Vec<Stmt> = vec![];

    let mut from_orig_bindings = Punctuated::<_, Token![,]>::default();
    let mut to_orig_bindings = Punctuated::<_, Token![,]>::default();

    let mut bindings = Punctuated::<_, Token![,]>::default();

    if !skip_node_id {
        to_orig_bindings.push(Pat::Ident(PatIdent {
            attrs: Default::default(),
            by_ref: None,
            mutability: None,
            ident: struct_name.new_ident_with(|_| "node_id"),
            subpat: None,
        }));
        from_orig_body.push(
            q!({
                let node_id = id_gen.gen();
            })
            .parse(),
        );
    }

    for (idx, field, info) in f.iter().enumerate().map(|(idx, field)| {
        (
            idx,
            field,
            handle_field(
                nodes_to_convert,
                &field.attrs,
                &field
                    .ident
                    .clone()
                    .unwrap_or(Ident::new(&format!("_{}", idx), field.ident.span())),
                &field.ty,
            ),
        )
    }) {
        let binding = &field
            .ident
            .clone()
            .unwrap_or(Ident::new(&format!("_{}", idx), field.ident.span()));

        bindings.push(binding.clone());

        from_orig_body.push(
            q!(
                Vars {
                    binding,
                    expr: &info.from_orig
                },
                {
                    let binding = expr;
                }
            )
            .parse(),
        );
        to_orig_body.push(
            q!(
                Vars {
                    binding,
                    expr: &info.to_orig
                },
                {
                    let binding = expr;
                }
            )
            .parse(),
        );

        from_orig_bindings.push(Pat::Ident(PatIdent {
            attrs: vec![],
            by_ref: Default::default(),
            mutability: Default::default(),
            ident: binding.clone(),
            subpat: None,
        }));
        to_orig_bindings.push(Pat::Ident(PatIdent {
            attrs: vec![],
            by_ref: Default::default(),
            mutability: Default::default(),
            ident: binding.clone(),
            subpat: None,
        }));
    }

    from_orig_body.push(if skip_node_id {
        q!(
            Vars {
                bindings: &bindings,
            },
            {
                return Self { bindings };
            }
        )
        .parse()
    } else {
        q!(
            Vars {
                bindings: &bindings,
            },
            {
                return Self { node_id, bindings };
            }
        )
        .parse()
    });
    to_orig_body.push(
        q!(
            Vars {
                Type: struct_name,
                bindings: &bindings,
            },
            {
                return Type { bindings };
            }
        )
        .parse(),
    );

    let from_orig_arm = Arm {
        attrs: Default::default(),
        pat: q!(
            Vars {
                Type: struct_name,
                bindings: &from_orig_bindings,
            },
            (Type { bindings })
        )
        .parse(),
        guard: None,
        fat_arrow_token: struct_name.span().as_token(),
        body: box Expr::Block(ExprBlock {
            attrs: Default::default(),
            label: Default::default(),
            block: Block {
                brace_token: struct_name.span().as_token(),
                stmts: from_orig_body,
            },
        }),
        comma: Some(struct_name.span().as_token()),
    };
    let new_strudct_name = struct_name.new_ident_with(|s| format!("R{}", s));
    let to_orig_arm = Arm {
        attrs: Default::default(),
        pat: q!(
            Vars {
                Type: &new_strudct_name,
                bindings: &to_orig_bindings,
            },
            (Type { bindings })
        )
        .parse(),
        guard: None,
        fat_arrow_token: struct_name.span().as_token(),
        body: box Expr::Block(ExprBlock {
            attrs: Default::default(),
            label: Default::default(),
            block: Block {
                brace_token: struct_name.span().as_token(),
                stmts: to_orig_body,
            },
        }),
        comma: Some(struct_name.span().as_token()),
    };

    (from_orig_arm, to_orig_arm)
}

struct OptionReplacer<'a> {
    nodes_to_convert: &'a [String],
}

impl Fold for OptionReplacer<'_> {
    fn fold_type(&mut self, ty: Type) -> Type {
        let ty = fold_type(self, ty);

        match &ty {
            Type::Path(inner_path) => {
                //
                if let Some(inner_name) = inner_path.path.get_ident() {
                    let is_rnode = self
                        .nodes_to_convert
                        .iter()
                        .any(|n| inner_name == &*format!("R{}", n));
                    if is_rnode {
                        return q!(Vars { inner_name }, (Option<inner_name>)).parse();
                    }
                }
            }
            _ => {}
        }

        ty
    }
}

struct RNodeField {
    from_orig: Expr,
    to_orig: Expr,
    ty: Type,
}

/// Look for attributes, namely `#[rc]` or `#[refcell]`.
fn handle_field(
    nodes_to_convert: &[String],
    attrs: &[Attribute],
    match_binding: &Ident,
    ty: &Type,
) -> RNodeField {
    // let rc = attrs.iter().any(|attr| attr.path.is_ident("rc"));
    // let ref_cell = attrs.iter().any(|attr| attr.path.is_ident("refcell"));
    let rc = false;
    let ref_cell = false;

    if rc && ref_cell {
        panic!(
            "#[rc] and #[ref_cell] cannot be applied to same field because #[rc] implies \
             Rc<Refell<T>>"
        )
    }

    // If type can be converted to RNode, do it.
    match ty {
        Type::Path(path_ty) => {
            if let Some(name) = path_ty.path.get_ident() {
                let rnode_name = Path::from(Ident::new(&format!("R{}", name), path_ty.path.span()));
                if nodes_to_convert.iter().any(|n| name == n) {
                    return RNodeField {
                        ty: Type::Path(TypePath {
                            path: rnode_name.clone(),
                            qself: None,
                        }),
                        from_orig: q!(
                            Vars {
                                RNodeType: &rnode_name,
                                match_binding
                            },
                            ({
                                use rnode::IntoRNode;
                                match_binding.into_rnode(id_gen)
                            })
                        )
                        .parse(),
                        to_orig: q!(
                            Vars { match_binding },
                            ({
                                use rnode::RNode;
                                match_binding.into_orig()
                            })
                        )
                        .parse(),
                    };
                }
            }
        }
        _ => {}
    }

    if let Some(ty) = extract_box(&ty) {
        let res = handle_field(nodes_to_convert, attrs, match_binding, ty);

        if rc {
            return RNodeField {
                to_orig: q!(
                    Vars {
                        match_binding,
                        inner: &res.to_orig
                    },
                    (Box::new(inner))
                )
                .parse(),
                from_orig: q!(
                    Vars {
                        match_binding,
                        inner: &res.from_orig
                    },
                    ({
                        let match_binding = *match_binding;
                        inner
                    })
                )
                .parse(),
                ty: q!(Vars { ty: &res.ty }, (std::rc::Rc<std::cell::RefCell<ty>>)).parse(),
                ..res
            };
        } else if ref_cell {
            return RNodeField {
                from_orig: q!(
                    Vars {
                        orig: match_binding,
                        inner_from_org: &res.from_orig,
                    },
                    ({
                        let orig = *orig;
                        std::cell::RefCell::new(inner_from_org)
                    })
                )
                .parse(),
                to_orig: q!(
                    Vars {
                        val: match_binding,
                        inner_to_orig: &res.to_orig
                    },
                    ({
                        let val = val.into_inner();
                        let res = inner_to_orig;
                        res.map(Box::new)
                    })
                )
                .parse(),
                ty: q!(Vars { ty: &res.ty }, (std::cell::RefCell<ty>)).parse(),
            };
        } else {
            return RNodeField {
                from_orig: q!(
                    Vars {
                        orig: match_binding,
                        inner_from_org: &res.from_orig,
                    },
                    ({
                        let orig = *orig;
                        Box::new(inner_from_org)
                    })
                )
                .parse(),
                to_orig: q!(
                    Vars {
                        inner_to_orig: &res.to_orig
                    },
                    ({
                        let res = inner_to_orig;
                        Box::new(res)
                    })
                )
                .parse(),
                ty: q!(Vars { ty: &res.ty }, (Box<ty>)).parse(),
            };
        }
    }

    if let Some(ty) = extract_opt(&ty) {
        let mut info = handle_field(nodes_to_convert, &[], match_binding, ty);
        let boxed = extract_box(ty);

        if !rc && !ref_cell {
            return RNodeField {
                from_orig: q!(
                    Vars {
                        v: &match_binding,
                        inner: &info.from_orig
                    },
                    (v.map(|v| { inner }))
                )
                .parse(),
                to_orig: q!(
                    Vars {
                        v: &match_binding,
                        inner: &info.to_orig
                    },
                    (v.map(|v| { inner }))
                )
                .parse(),
                ty: q!(Vars { ty: &info.ty }, (Option<ty>)).parse(),
            };
        }

        info.ty = OptionReplacer { nodes_to_convert }.fold_type(info.ty);

        if rc {
            info.from_orig = q!(
                Vars {
                    v: &match_binding,
                    inner: &info.from_orig,
                },
                {
                    match v.map(|v| inner) {
                        Some(v) => std::rc::Rc::new(std::cell::RefCell::new(Some(v))),
                        None => Default::default(),
                    }
                }
            )
            .parse();
            info.ty = q!(Vars { ty: &info.ty }, (std::rc::Rc<std::cell::RefCell<ty>>)).parse()
        } else {
            info.from_orig = q!(
                Vars {
                    v: &match_binding,
                    inner: &info.from_orig,
                },
                {
                    match v.map(|v| inner) {
                        Some(v) => std::cell::RefCell::new(Some(v)),
                        None => Default::default(),
                    }
                }
            )
            .parse();
            if let Some(boxed) = boxed {
                let inner_info = handle_field(nodes_to_convert, attrs, match_binding, boxed);
                info.to_orig = q!(
                    Vars {
                        inner: &inner_info.to_orig,
                    },
                    { inner.map(Box::new) }
                )
                .parse();
                info.ty = q!(
                    Vars { ty: &inner_info.ty },
                    (std::cell::RefCell<Option<Box<ty>>>)
                )
                .parse();
            } else {
                info.ty = q!(Vars { ty: &info.ty }, (std::cell::RefCell<ty>)).parse()
            }
        }

        return RNodeField { ..info };
    }

    if let Some(ty) = extract_vec(&ty) {
        let mut info = handle_field(nodes_to_convert, attrs, match_binding, ty);
        info.ty = q!(Vars { ty: &info.ty }, (Vec<ty>)).parse();

        if !rc && !ref_cell {
            info.to_orig = q!(
                Vars {
                    v: match_binding,
                    inner: &info.to_orig,
                },
                { v.into_iter().map(|v| { inner }).collect() }
            )
            .parse();
            info.from_orig = q!(
                Vars {
                    v: match_binding,
                    inner: &info.from_orig,
                },
                { v.into_iter().map(|v| { inner }).collect() }
            )
            .parse();
            return info;
        }

        return RNodeField {
            from_orig: q!(
                Vars {
                    v: &match_binding,
                    inner: &info.from_orig
                },
                { v.into_iter().map(|v| { inner }).collect() }
            )
            .parse(),
            ..info
        };
    }

    if !rc && !ref_cell {
        return RNodeField {
            from_orig: q!(Vars { match_binding }, { match_binding }).parse(),
            to_orig: q!(Vars { match_binding }, { match_binding }).parse(),
            ty: ty.clone(),
        };
    }

    // Vec<T> -> Vec<Rc<RefCell<T>>>
    // T -> Rc<RefCell<T>>
    if rc {
        if let Some(ty) = extract_vec(&ty) {
            return RNodeField {
                from_orig: q!(Vars { match_binding }, {
                    match_binding
                        .into_iter()
                        .map(std::cell::RefCell::new)
                        .map(std::rc::Rc::new)
                        .collect()
                })
                .parse(),
                to_orig: q!(Vars { match_binding }, { match_binding }).parse(),
                ty: q!(Vars { ty }, (Vec<std::rc::Rc<std::cell::RefCell<ty>>>)).parse(),
            };
        }

        return RNodeField {
            from_orig: q!(Vars { match_binding }, { match_binding }).parse(),
            to_orig: q!(Vars { match_binding }, { match_binding }).parse(),
            ty: q!(Vars { ty }, (std::rc::Rc<std::cell::RefCell<ty>>)).parse(),
        };
    }

    if ref_cell {
        if let Some(ty) = extract_vec(&ty) {
            return RNodeField {
                from_orig: q!(Vars { match_binding }, {
                    match_binding
                        .into_iter()
                        .map(std::cell::RefCell::new)
                        .collect()
                })
                .parse(),
                to_orig: q!(Vars { match_binding }, { match_binding }).parse(),
                ty: q!(Vars { ty }, (Vec<std::cell::RefCell<ty>>)).parse(),
            };
        }

        return RNodeField {
            from_orig: q!(Vars { match_binding }, { match_binding }).parse(),
            to_orig: q!(Vars { match_binding }, { match_binding }).parse(),
            ty: q!(Vars { ty }, (std::cell::RefCell<ty>)).parse(),
        };
    }

    unreachable!()
}

struct RNodeVariant {
    fields: Fields,
}

fn handle_variant(variant: &Variant) -> RNodeVariant {
    match &variant.fields {
        Fields::Named(_) => todo!("named fields in enum"),
        Fields::Unnamed(fields) => {
            //
            RNodeVariant {
                fields: Fields::Unnamed(FieldsUnnamed {
                    unnamed: fields
                        .unnamed
                        .clone()
                        .into_pairs()
                        .map(|mut pair| {
                            let f = pair.value_mut();

                            let new_ty = prefix_type_name(&f.ty);
                            f.ty = new_ty;

                            pair
                        })
                        .collect(),
                    ..fields.clone()
                }),
            }
        }
        Fields::Unit => unimplemented!("Unit variant in enum: {}", variant.ident.to_string()),
    }
}

fn prefix_type_name(ty: &Type) -> Type {
    if let Some(ty) = extract_box(ty) {
        let ty = prefix_type_name(ty);
        return q!(Vars { ty }, (Box<ty>)).parse();
    }

    if let Some(ty) = extract_opt(ty) {
        let ty = prefix_type_name(ty);
        return q!(Vars { ty }, (Option<ty>)).parse();
    }

    if let Some(ty) = extract_vec(ty) {
        let ty = prefix_type_name(ty);
        return q!(Vars { ty }, (Vec<ty>)).parse();
    }

    match ty {
        Type::Path(p) => {
            let new_name = p
                .path
                .get_ident()
                .unwrap()
                .new_ident_with(|s| format!("R{}", s));

            return q!(Vars { new_name }, (new_name)).parse();
        }
        _ => unimplemented!("field type other than `Path`"),
    }
}

fn extract_generic<'a>(name: &str, ty: &'a Type) -> Option<&'a Type> {
    match ty {
        Type::Path(p) => {
            let last = p.path.segments.last().unwrap();

            if !last.arguments.is_empty() {
                if last.ident == name {
                    match &last.arguments {
                        PathArguments::AngleBracketed(tps) => {
                            let arg = tps.args.first().unwrap();

                            match arg {
                                GenericArgument::Type(arg) => return Some(arg),
                                _ => unimplemented!("generic parameter other than type"),
                            }
                        }
                        _ => unimplemented!("Box() -> T or Box without a type parameter"),
                    }
                }
            }
        }
        _ => {}
    }

    None
}

fn extract_vec(ty: &Type) -> Option<&Type> {
    extract_generic("Vec", ty)
}

fn extract_box(ty: &Type) -> Option<&Type> {
    extract_generic("Box", ty)
}

fn extract_opt(ty: &Type) -> Option<&Type> {
    extract_generic("Option", ty)
}

fn take_attrs(q: Quote) -> Vec<Attribute> {
    q.parse::<ItemStruct>().attrs
}
