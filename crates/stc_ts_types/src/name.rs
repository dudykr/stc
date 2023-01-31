use std::{
    convert::{TryFrom, TryInto},
    fmt::{self, Debug, Formatter},
};

use stc_ts_ast_rnode::{
    RComputedPropName, RExpr, RIdent, RLit, RMemberExpr, RMemberProp, ROptChainBase, ROptChainExpr, RParenExpr, RThisExpr, RTsEntityName,
    RTsThisType, RTsThisTypeOrIdent,
};
use swc_atoms::{js_word, JsWord};
use swc_common::{iter::IdentifyLast, SyntaxContext};

use crate::Id;

/// Efficient alternative for names with variable length like `foo.bar.baz.qux`.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Name(Id, Vec<JsWord>);

impl Name {
    pub fn new(name: JsWord, ctxt: SyntaxContext) -> Self {
        Self(Id::new(name, ctxt), vec![])
    }

    pub const fn get_ctxt(&self) -> SyntaxContext {
        self.0.ctxt()
    }

    pub fn push(&mut self, sym: JsWord) {
        self.1.push(sym)
    }

    pub fn top(&self) -> Id {
        self.0.clone()
    }

    #[allow(clippy::len_without_is_empty)]
    pub fn len(&self) -> usize {
        self.1.len() + 1
    }

    pub fn as_ids(&self) -> (&Id, &[JsWord]) {
        (&self.0, &self.1)
    }
}

impl Debug for Name {
    #[cold]
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "{}", self.0)?;

        for s in self.1.iter() {
            write!(f, ".{}", s)?;
        }

        Ok(())
    }
}

impl From<&'_ RIdent> for Name {
    #[inline]
    fn from(i: &RIdent) -> Name {
        Id::from(i).into()
    }
}

impl From<RIdent> for Name {
    #[inline]
    fn from(i: RIdent) -> Name {
        Id::from(i).into()
    }
}

impl From<&'_ Id> for Name {
    #[inline]
    fn from(v: &Id) -> Name {
        Name(smallvec![v.clone()])
    }
}

impl From<Id> for Name {
    #[inline]
    fn from(v: Id) -> Name {
        Name(smallvec![v])
    }
}

impl From<RThisExpr> for Name {
    #[inline]
    fn from(this: RThisExpr) -> Name {
        let this: Id = RIdent::new(js_word!("this"), this.span.with_ctxt(SyntaxContext::empty())).into();
        Name(smallvec![this])
    }
}

impl From<RTsThisType> for Name {
    #[inline]
    fn from(this: RTsThisType) -> Name {
        let this: Id = RIdent::new(js_word!("this"), this.span.with_ctxt(SyntaxContext::empty())).into();
        Name(smallvec![this])
    }
}

impl From<&'_ [Id]> for Name {
    #[inline]
    fn from(v: &[Id]) -> Name {
        v.to_vec().into()
    }
}

impl From<Vec<Id>> for Name {
    #[inline]
    fn from(v: Vec<Id>) -> Name {
        Name(v.into())
    }
}

impl From<RTsEntityName> for Name {
    fn from(n: RTsEntityName) -> Self {
        fn expand(buf: &mut Inner, n: RTsEntityName) {
            match n {
                RTsEntityName::Ident(i) => buf.push(Id::from(i)),

                RTsEntityName::TsQualifiedName(box q) => {
                    expand(buf, q.left);
                    buf.push(Id::word(q.right.sym));
                }
            }
        }

        let mut buf = Inner::default();
        expand(&mut buf, n);
        Self(buf)
    }
}

impl From<&'_ RTsEntityName> for Name {
    fn from(n: &RTsEntityName) -> Self {
        fn expand(buf: &mut Inner, n: &RTsEntityName) {
            match n {
                RTsEntityName::Ident(i) => buf.push(i.into()),

                RTsEntityName::TsQualifiedName(box q) => {
                    expand(buf, &q.left);
                    buf.push(q.right.clone().into());
                }
            }
        }

        let mut buf = Inner::default();
        expand(&mut buf, n);
        Self(buf)
    }
}

impl TryFrom<&'_ RExpr> for Name {
    type Error = ();

    fn try_from(e: &RExpr) -> Result<Self, Self::Error> {
        match e {
            RExpr::Ident(i) => Ok(i.into()),
            RExpr::Member(m) => m.try_into(),
            RExpr::OptChain(ROptChainExpr {
                base: ROptChainBase::Member(m),
                ..
            }) => m.try_into(),
            RExpr::This(this) => Ok({
                let this: Id = RIdent::new(js_word!("this"), this.span.with_ctxt(SyntaxContext::empty())).into();

                this.into()
            }),
            RExpr::Paren(RParenExpr { expr, .. }) => (&**expr).try_into(),

            // TODO
            _ => Err(()),
        }
    }
}

impl From<&'_ RTsThisTypeOrIdent> for Name {
    fn from(ty: &RTsThisTypeOrIdent) -> Self {
        match ty {
            RTsThisTypeOrIdent::TsThisType(this) => Name::from(RIdent::new(js_word!("this"), this.span.with_ctxt(SyntaxContext::empty()))),
            RTsThisTypeOrIdent::Ident(ref i) => Name::from(i),
        }
    }
}

impl<'a> TryFrom<&'a RMemberExpr> for Name {
    type Error = ();

    fn try_from(e: &RMemberExpr) -> Result<Self, ()> {
        let mut name: Name = (&*e.obj).try_into()?;

        name.0.push(match &e.prop {
            RMemberProp::Ident(i) => i.clone().into(),
            RMemberProp::Computed(RComputedPropName {
                expr: box RExpr::Lit(RLit::Str(s)),
                ..
            }) => Id::word(s.value.clone()),
            _ => return Err(()),
        });

        Ok(name)
    }
}
