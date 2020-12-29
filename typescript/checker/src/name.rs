use smallvec::{smallvec, SmallVec};
use stc_types::Id;
use std::{
    convert::{TryFrom, TryInto},
    fmt::{self, Debug, Formatter},
};
use swc_atoms::js_word;
use swc_common::{iter::IdentifyLast, DUMMY_SP};
use swc_ecma_ast::*;

type Inner = SmallVec<[Id; 4]>;

/// Efficient alternative for [TsEntityName].
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

impl From<&'_ Ident> for Name {
    #[inline]
    fn from(i: &Ident) -> Name {
        Id::from(i).into()
    }
}

impl From<Ident> for Name {
    #[inline]
    fn from(i: Ident) -> Name {
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

impl From<TsEntityName> for Name {
    fn from(n: TsEntityName) -> Self {
        fn expand(buf: &mut Inner, n: TsEntityName) {
            match n {
                TsEntityName::Ident(i) => buf.push(Id::from(i)),

                TsEntityName::TsQualifiedName(box q) => {
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

impl From<&'_ TsEntityName> for Name {
    fn from(n: &TsEntityName) -> Self {
        fn expand(buf: &mut Inner, n: &TsEntityName) {
            match n {
                TsEntityName::Ident(i) => buf.push(i.into()),

                TsEntityName::TsQualifiedName(box q) => {
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

impl TryFrom<&'_ Expr> for Name {
    type Error = ();

    fn try_from(e: &Expr) -> Result<Self, Self::Error> {
        match *e {
            Expr::Ident(ref i) => Ok(i.into()),
            // TODO
            _ => Err(()),
        }
    }
}

impl From<&'_ TsThisTypeOrIdent> for Name {
    fn from(ty: &TsThisTypeOrIdent) -> Self {
        match *ty {
            TsThisTypeOrIdent::TsThisType(..) => Name::from(Ident::new(js_word!("this"), DUMMY_SP)),
            TsThisTypeOrIdent::Ident(ref i) => Name::from(i),
        }
    }
}

impl<'a> TryFrom<&'a MemberExpr> for Name {
    type Error = ();

    fn try_from(e: &MemberExpr) -> Result<Self, ()> {
        if e.computed {
            return Err(());
        }

        match &e.obj {
            ExprOrSuper::Expr(box Expr::Ident(i)) => Ok(Name::from(i)),
            ExprOrSuper::Expr(box Expr::Member(member)) => {
                let mut obj: Name = member.try_into()?;
                obj.0.push(match &*e.prop {
                    Expr::Ident(i) => i.clone().into(),
                    _ => return Err(()),
                });
                Ok(obj)
            }
            _ => Err(()),
        }
    }
}
