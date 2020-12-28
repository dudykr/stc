use stc_ast_rnode::RIdent;
use stc_ast_rnode::RTsEntityName;
use stc_visit::Visit;
use std::{
    cmp::PartialEq,
    fmt::{self, Debug, Display, Formatter},
};
use swc_atoms::JsWord;
use swc_common::EqIgnoreSpan;
use swc_common::TypeEq;
use swc_common::{SyntaxContext, DUMMY_SP};
use swc_ecma_ast::Ident;
use swc_ecma_utils::ident::IdentLike;

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EqIgnoreSpan, TypeEq, Visit)]
pub struct Id {
    sym: JsWord,
    ctxt: SyntaxContext,
}

impl Id {
    pub fn as_str(&self) -> &str {
        &self.sym
    }

    pub fn sym(&self) -> &JsWord {
        &self.sym
    }

    pub fn word(sym: JsWord) -> Self {
        Id {
            sym,
            ctxt: SyntaxContext::empty(),
        }
    }
}

impl From<&'_ RIdent> for Id {
    fn from(i: &RIdent) -> Self {
        Id {
            sym: i.sym.clone(),
            ctxt: i.span.ctxt(),
        }
    }
}

impl From<RIdent> for Id {
    fn from(i: RIdent) -> Self {
        Id {
            sym: i.sym,
            ctxt: i.span.ctxt(),
        }
    }
}

impl From<Id> for RIdent {
    fn from(i: Id) -> Self {
        RIdent {
            span: DUMMY_SP.with_ctxt(i.ctxt),
            sym: i.sym,
            type_ann: Default::default(),
            optional: false,
        }
    }
}

impl From<Id> for RTsEntityName {
    fn from(i: Id) -> Self {
        RTsEntityName::Ident(i.into())
    }
}

/// This makes life easier.
impl Display for Id {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}{:?}", self.sym, self.ctxt)
    }
}

impl PartialEq<Ident> for Id {
    fn eq(&self, other: &Ident) -> bool {
        self.sym == other.sym && self.ctxt == other.span.ctxt()
    }
}

impl PartialEq<&'_ Ident> for Id {
    fn eq(&self, other: &&Ident) -> bool {
        self.sym == other.sym && self.ctxt == other.span.ctxt()
    }
}

impl PartialEq<RIdent> for Id {
    fn eq(&self, other: &RIdent) -> bool {
        self.sym == other.sym && self.ctxt == other.span.ctxt()
    }
}

impl PartialEq<&'_ RIdent> for Id {
    fn eq(&self, other: &&RIdent) -> bool {
        self.sym == other.sym && self.ctxt == other.span.ctxt()
    }
}

impl IdentLike for Id {
    fn from_ident(i: &Ident) -> Self {
        Self {
            sym: i.sym.clone(),
            ctxt: i.span.ctxt,
        }
    }

    fn to_id(&self) -> (JsWord, SyntaxContext) {
        (self.sym.clone(), self.ctxt)
    }

    fn into_id(self) -> (JsWord, SyntaxContext) {
        (self.sym, self.ctxt)
    }
}

impl Debug for Id {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(self, f)
    }
}
