#![feature(box_patterns)]
#![deny(unused_variables)]
#![recursion_limit = "4096"]

#[macro_use]
extern crate pmutil;
extern crate proc_macro;

use pmutil::{Quote, ToTokensExt};
use swc_macros_common::prelude::*;
use syn::{
    fold::Fold, Block, ExprTryBlock, FnArg, Ident, ImplItem, ImplItemMethod, ItemImpl, Lifetime, LitStr, ReturnType,
    Token, Type, TypeReference,
};

#[proc_macro_attribute]
pub fn context(arg: proc_macro::TokenStream, item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let context_arg: LitStr = syn::parse(arg).unwrap();
    let mut item: ImplItemMethod = syn::parse(item).expect("failed to parse input as an item");

    let body = q!(
        Vars {
            body: &item.block,
            context_arg
        },
        ({
            let _ctx = stc_utils::error::context(context_arg);
            let res: Result<_, stc_ts_errors::Error> = try { body };

            res.context(context_arg)
        })
    )
    .parse::<Block>();

    item.block = body;

    print("context", item.dump())
}

/// This macro converts
///
/// ```ignore
/// impl Foo {
///     #[extra_validator]
///     fn validate_foo(&mut self, arg: Arg1) -> Result<Ret, ()> {
///         // body
///         Err(err)?;
///     }
/// }
/// ```
///
/// to
///
///
/// ```ignore
/// impl Foo {
///     fn validate_foo(&mut self, arg: Arg1) -> Result<Ret, ()> {
///         let res: Result<Ret, Error> = try {
///             // body
///             Err(err)?
///         };
///
///         match res {
///             Ok(v) => Ok(v),
///             Err(err) => {
///                 self.info.errors.push(err);
///                 Err(())
///             }
///         }
///     }
/// }
/// ```
#[proc_macro_attribute]
pub fn extra_validator(_: proc_macro::TokenStream, item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    fn expand_extra_validator(i: ImplItemMethod) -> ImplItemMethod {
        let should_return = match i.sig.output {
            ReturnType::Default => false,
            _ => true,
        };

        let try_block = ExprTryBlock {
            attrs: Default::default(),
            try_token: call_site(),
            block: i.block,
        };

        let block = if should_return {
            Quote::new_call_site()
                .quote_with(smart_quote!(Vars { try_block: &try_block }, {
                    {
                        let res: Result<_, Error> = try_block;

                        match res {
                            Ok(v) => Ok(v),
                            Err(err) => {
                                self.storage.report(err);
                                Err(())
                            }
                        }
                    }
                }))
                .parse()
        } else {
            Quote::new_call_site()
                .quote_with(smart_quote!(Vars { try_block: &try_block }, {
                    {
                        let res: Result<_, Error> = try_block;

                        match res {
                            Err(err) => {
                                self.storage.report(err);
                            }
                            _ => {}
                        }
                    }
                }))
                .parse()
        };

        ImplItemMethod { block, ..i }
    }

    let item = syn::parse(item).expect("failed to parse input as an item");
    let item = expand_extra_validator(item);
    print("extra_validator", item.dump())
}

/// This trait implements Validate with proper types.
#[proc_macro_attribute]
pub fn validator(_: proc_macro::TokenStream, item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let impl_item: ItemImpl = syn::parse(item).expect("failed to parse input as an ItemImpl");
    let visitor_type = &*impl_item.self_ty;

    let mut tokens = q!({});
    for mtd in &impl_item.items {
        let mtd = match mtd {
            ImplItem::Method(m) => m,
            _ => unimplemented!("items other than method is not supported yet"),
        };
        let sig = &mtd.sig;
        assert_eq!(
            sig.ident, "validate",
            "#[validator] wants the name of method `validate`"
        );

        // We want to implement Validate<'context, T> for Analyzer, so we need to find
        // `T`.
        let mut node_pat = None;
        let mut node_type = None;
        let mut context_types = Punctuated::<_, Token![,]>::default();
        let mut context_pats = Punctuated::<_, Token![,]>::default();
        for input in sig.inputs.pairs().skip(1) {
            match input.value() {
                FnArg::Receiver(_) => panic!("Expected type, not receiver"),
                FnArg::Typed(pat_ty) => {
                    let ty = &*pat_ty.ty;

                    // Find `T`
                    if node_type == None {
                        match ty {
                            Type::Reference(ty) if ty.mutability.is_none() => {
                                node_type = Some(ty.elem.clone());
                            }
                            _ => unimplemented!("first argument should be self and second argument must be `&T`"),
                        }
                        node_pat = Some(pat_ty.pat.clone());
                        continue;
                    }

                    // Now we look for extra context args
                    // TODO: Fix span
                    {
                        let mut ty = ty.clone();
                        ty = LifetimeReplacer.fold_type(ty);

                        context_types.push(ty);
                        context_pats.push(pat_ty.pat.clone());
                    }
                }
            }
        }

        let ret_ty = &mtd.sig.output;
        let default_ty;
        let ret_ty = match ret_ty {
            ReturnType::Type(_, ty) => &**ty,
            ReturnType::Default => {
                default_ty = q!((crate::ValidationResult<()>)).parse();
                &default_ty
            }
        };

        let mut item = q!(
            Vars {
                VisitorType: visitor_type,
                NodeType: &node_type,
                ReturnType: &ret_ty,
                ContextType: &context_types,
                body: &mtd.block,
                node_pat: &node_pat.unwrap(),
                conext_pats: &context_pats,
            },
            {
                impl<'context> crate::validator::Validate<'context, NodeType> for VisitorType {
                    type Output = ReturnType;
                    type Context = (ContextType);

                    fn validate(&mut self, node_pat: &NodeType, ctxt: Self::Context) -> ReturnType {
                        let start = std::time::Instant::now();
                        let (conext_pats) = ctxt;

                        let ret = (|| body)();

                        let end = std::time::Instant::now();

                        slog::debug!(
                            self.logger,
                            "Validate<{}>: (time = {:?})",
                            stringify!(NodeType),
                            end - start
                        );

                        ret
                    }
                }
            }
        )
        .parse::<ItemImpl>();
        item.attrs.extend(impl_item.attrs.clone());

        tokens.push_tokens(&item)
    }

    print("validator", tokens.dump())
}

struct LifetimeReplacer;

impl Fold for LifetimeReplacer {
    fn fold_type_reference(&mut self, mut i: TypeReference) -> TypeReference {
        i.lifetime = Some(Lifetime {
            apostrophe: call_site(),
            ident: Ident::new("context", call_site()),
        });
        i
    }
}
