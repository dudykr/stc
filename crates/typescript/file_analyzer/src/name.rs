use smallvec::{smallvec, SmallVec};
use stc_ts_ast_rnode::RExpr;
use stc_ts_ast_rnode::RExprOrSuper;
use stc_ts_ast_rnode::RIdent;
use stc_ts_ast_rnode::RMemberExpr;
use stc_ts_ast_rnode::RTsEntityName;
use stc_ts_ast_rnode::RTsThisTypeOrIdent;
use stc_ts_types::Id;
use std::{
    convert::{TryFrom, TryInto},
    fmt::{self, Debug, Formatter},
};
use swc_atoms::js_word;
use swc_common::{iter::IdentifyLast, DUMMY_SP};

type Inner = SmallVec<[Id; 4]>;

/// Efficient alternative for [RTsEntityName].
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Name(Inner);

impl Debug for Name {
    #[cold]
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        for (last, s) in self.0.iter().identify_last() {
            write!(f, "{}", s)?;
            if !last {
                write!(f, ".")?;
            }
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
        match *e {
            RExpr::Ident(ref i) => Ok(i.into()),
            // TODO
            _ => Err(()),
        }
    }
}

impl From<&'_ RTsThisTypeOrIdent> for Name {
    fn from(ty: &RTsThisTypeOrIdent) -> Self {
        match *ty {
            RTsThisTypeOrIdent::TsThisType(..) => {
                Name::from(RIdent::new(js_word!("this"), DUMMY_SP))
            }
            RTsThisTypeOrIdent::Ident(ref i) => Name::from(i),
        }
    }
}

impl<'a> TryFrom<&'a RMemberExpr> for Name {
    type Error = ();

    fn try_from(e: &RMemberExpr) -> Result<Self, ()> {
        if e.computed {
            return Err(());
        }

        match &e.obj {
            RExprOrSuper::Expr(box RExpr::Ident(i)) => Ok(Name::from(i)),
            RExprOrSuper::Expr(box RExpr::Member(member)) => {
                let mut obj: Name = member.try_into()?;
                obj.0.push(match &*e.prop {
                    RExpr::Ident(i) => i.clone().into(),
                    _ => return Err(()),
                });
                Ok(obj)
            }
            _ => Err(()),
        }
    }
}
