use std::{
    convert::{TryFrom, TryInto},
    fmt::{self, Debug, Formatter},
};

use stc_ts_ast_rnode::{
    RComputedPropName, RExpr, RIdent, RLit, RMemberExpr, RMemberProp, ROptChainBase, ROptChainExpr, RParenExpr, RThisExpr, RTsEntityName,
    RTsThisType, RTsThisTypeOrIdent,
};
use swc_atoms::{js_word, JsWord};
use swc_common::SyntaxContext;

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

    pub fn len(&self) -> usize {
        self.1.len() + 1
    }

    pub const fn is_empty(&self) -> bool {
        false
    }

    pub fn inner(&self) -> (&Id, &[JsWord]) {
        (&self.0, &self.1)
    }

    pub fn last(&self) -> &JsWord {
        self.1.last().unwrap_or_else(|| self.0.sym())
    }

    pub fn slice_to(&self, end: usize) -> Name {
        let mut v = self.1.clone();
        v.truncate(end - 1);
        Name(self.0.clone(), v)
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
        Self::from(v.clone())
    }
}

impl From<Id> for Name {
    #[inline]
    fn from(v: Id) -> Name {
        Name(v, vec![])
    }
}

impl From<RThisExpr> for Name {
    #[inline]
    fn from(this: RThisExpr) -> Name {
        let this: Id = RIdent::new(js_word!("this"), this.span.with_ctxt(SyntaxContext::empty())).into();
        this.into()
    }
}

impl From<RTsThisType> for Name {
    #[inline]
    fn from(this: RTsThisType) -> Name {
        let this: Id = RIdent::new(js_word!("this"), this.span.with_ctxt(SyntaxContext::empty())).into();
        this.into()
    }
}

impl From<RTsEntityName> for Name {
    fn from(n: RTsEntityName) -> Self {
        Self::from(&n)
    }
}

impl From<&'_ RTsEntityName> for Name {
    fn from(n: &RTsEntityName) -> Self {
        fn expand(buf: &mut Vec<JsWord>, n: &RTsEntityName) -> Id {
            match n {
                RTsEntityName::Ident(i) => {
                    buf.push(i.sym.clone());
                    Id::from(i)
                }

                RTsEntityName::TsQualifiedName(box q) => {
                    let top = expand(buf, &q.left);
                    buf.push(q.right.sym.clone());

                    top
                }
            }
        }

        let mut buf = Default::default();

        let top = expand(&mut buf, n);
        Self(top, buf)
    }
}

impl TryFrom<&'_ RExpr> for Name {
    type Error = ();

    fn try_from(e: &RExpr) -> Result<Self, Self::Error> {
        match e {
            RExpr::Ident(i) => Ok(i.into()),
            RExpr::Member(m) => m.try_into(),
            RExpr::OptChain(ROptChainExpr {
                base: box ROptChainBase::Member(m),
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

        name.1.push(match &e.prop {
            RMemberProp::Ident(i) => i.sym.clone(),
            RMemberProp::Computed(RComputedPropName {
                expr: box RExpr::Lit(RLit::Str(s)),
                ..
            }) => s.value.clone(),
            _ => return Err(()),
        });

        Ok(name)
    }
}
