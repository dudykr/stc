use pmutil::{q, IdentExt, SpanExt, ToTokensExt};
use syn::{
    parse, punctuated::Punctuated, spanned::Spanned, Arm, Block, Data, DeriveInput, Expr, ExprBlock, ExprMatch, Field, FieldPat,
    FieldValue, Fields, Ident, Index, Item, Member, Pat, PatPath, PatStruct, Path, Stmt, Token,
};

/// Note: This generates `FoldWith`, `VisitWith` and `VisitMutWith` although
/// it's `#[derive(Visit)]`
#[proc_macro_derive(Visit, attributes(visit))]
pub fn derive(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse::<DeriveInput>(item).unwrap();
    let items = make_item(&input);

    let mut tts = q!({});

    tts.push_tokens(&items.fold);
    tts.push_tokens(&items.visit_mut);
    tts.push_tokens(&items.visit);

    tts.push_tokens(&q!(Vars { Type: &input.ident }, {
        #[automatically_derived]
        impl stc_visit::Visitable for Type {}
    }));

    tts.dump().into()
}

fn make_item(input: &DeriveInput) -> Set<Item> {
    let body = make_body(input);

    Set {
        fold: q!(
            Vars {
                Type: &input.ident,
                body: &body.fold
            },
            {
                #[automatically_derived]
                impl<V> stc_visit::FoldWith<V> for Type
                where
                    V: ?Sized,
                {
                    fn fold_children_with(self, _visitor: &mut V) -> Self {
                        body
                    }
                }
            }
        )
        .parse(),
        visit: q!(
            Vars {
                Type: &input.ident,
                body: &body.visit
            },
            {
                #[automatically_derived]
                impl<V> stc_visit::VisitWith<V> for Type
                where
                    V: ?Sized,
                {
                    fn visit_children_with(&self, _visitor: &mut V) {
                        body
                    }
                }
            }
        )
        .parse(),
        visit_mut: q!(
            Vars {
                Type: &input.ident,
                body: &body.visit_mut
            },
            {
                #[automatically_derived]
                impl<V> stc_visit::VisitMutWith<V> for Type
                where
                    V: ?Sized,
                {
                    fn visit_mut_children_with(&mut self, _visitor: &mut V) {
                        body
                    }
                }
            }
        )
        .parse(),
    }
}

fn make_body(input: &DeriveInput) -> Set<Expr> {
    let mut arms = Set::default();

    match &input.data {
        Data::Struct(s) => {
            let path = q!({ Self }).parse();

            arms.push(make_arm(&path, &s.fields));
        }
        Data::Enum(e) => {
            for v in &e.variants {
                let path = q!(Vars { Variant: &v.ident }, { Self::Variant }).parse();
                arms.push(make_arm(&path, &v.fields));
            }
        }
        _ => unreachable!(),
    }

    let make_body = |arms| {
        Expr::Match(ExprMatch {
            attrs: Default::default(),
            match_token: input.span().as_token(),
            expr: q!(Vars {}, { self }).parse(),
            brace_token: input.span().as_token(),
            arms,
        })
    };

    Set {
        fold: make_body(arms.fold),
        visit: make_body(arms.visit),
        visit_mut: make_body(arms.visit_mut),
    }
}

fn make_arm(path: &Path, fields: &Fields) -> Set<Arm> {
    let mut stmts = Set::<Vec<_>>::default();
    let mut bindings = Punctuated::<_, Token![,]>::default();
    let mut fold_ret = Punctuated::<FieldValue, Token![,]>::default();

    for (i, f) in fields.iter().enumerate() {
        let val = match &f.ident {
            Some(v) => v.new_ident_with(|name| format!("_{}", name)),
            None => f.ty.span().new_ident(format!("_{}", i)),
        };
        bindings.push(FieldPat {
            attrs: Default::default(),
            member: match f.ident.clone() {
                Some(i) => Member::Named(i),
                None => Member::Unnamed(Index {
                    index: i as _,
                    span: f.ty.span(),
                }),
            },
            colon_token: Some(val.span().as_token()),
            pat: Box::new(Pat::Path(PatPath {
                attrs: Default::default(),
                qself: None,
                path: Path::from(val.clone()),
            })),
        });

        fold_ret.push(FieldValue {
            attrs: Default::default(),
            member: match f.ident.clone() {
                Some(i) => Member::Named(i),
                None => Member::Unnamed(Index {
                    index: i as _,
                    span: f.ty.span(),
                }),
            },
            colon_token: Some(val.span().as_token()),
            expr: q!(Vars { val: &val }, { val }).parse(),
        });

        let exprs = make_visit_expr(&val, f);
        stmts.push(Set {
            fold: q!(
                Vars {
                    val: &val,
                    expr: &exprs.fold
                },
                {
                    let val = expr;
                }
            )
            .parse(),
            visit: Stmt::Semi(exprs.visit, val.span().as_token()),
            visit_mut: Stmt::Semi(exprs.visit_mut, val.span().as_token()),
        });
    }

    stmts.fold.push(Stmt::Semi(
        q!(
            Vars {
                Path: &path,
                fields: &fold_ret
            },
            { return Path { fields } }
        )
        .parse(),
        path.span().as_token(),
    ));

    let make = |stmts| Arm {
        attrs: Default::default(),
        pat: Pat::Struct(PatStruct {
            attrs: Default::default(),
            path: path.clone(),
            brace_token: fields.span().as_token(),
            fields: bindings.clone(),
            dot2_token: None,
        }),
        guard: None,
        fat_arrow_token: fields.span().as_token(),
        body: Box::new(Expr::Block(ExprBlock {
            attrs: Default::default(),
            label: None,
            block: Block {
                brace_token: fields.span().as_token(),
                stmts,
            },
        })),
        comma: None,
    };

    Set {
        fold: make(stmts.fold),
        visit: make(stmts.visit),
        visit_mut: make(stmts.visit_mut),
    }
}

fn make_visit_expr(val: &Ident, _field: &Field) -> Set<Expr> {
    Set {
        fold: q!(Vars { val }, { val.fold_with(_visitor) }).parse(),
        visit: q!(Vars { val }, { val.visit_with(_visitor) }).parse(),
        visit_mut: q!(Vars { val }, { val.visit_mut_with(_visitor) }).parse(),
    }
}

#[derive(Debug, Default)]
struct Set<T> {
    fold: T,
    visit: T,
    visit_mut: T,
}

impl<T> Set<Vec<T>> {
    pub fn push(&mut self, v: Set<T>) {
        self.fold.push(v.fold);
        self.visit.push(v.visit);
        self.visit_mut.push(v.visit_mut);
    }
}
