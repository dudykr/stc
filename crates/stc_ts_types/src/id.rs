use std::{
    cmp::PartialEq,
    fmt::{self, Debug, Display, Formatter},
};

use rnode::NodeId;
use serde::{Deserialize, Serialize};
use stc_ts_ast_rnode::{RIdent, RModuleExportName, RTsEntityName};
use stc_visit::Visit;
use swc_atoms::JsWord;
use swc_common::{EqIgnoreSpan, SyntaxContext, TypeEq, DUMMY_SP};
use swc_ecma_ast::Ident;
use swc_ecma_utils::ident::IdentLike;

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EqIgnoreSpan, TypeEq, Visit, Serialize, Deserialize)]
pub struct Id {
    sym: JsWord,
    ctxt: SyntaxContext,
}

impl Id {
    pub const fn new(sym: JsWord, ctxt: SyntaxContext) -> Self {
        Self { sym, ctxt }
    }

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
            node_id: NodeId::invalid(),
            span: DUMMY_SP.with_ctxt(i.ctxt),
            sym: i.sym,
            optional: false,
        }
    }
}

impl From<Id> for RTsEntityName {
    fn from(i: Id) -> Self {
        RTsEntityName::Ident(i.into())
    }
}

impl PartialEq<&'_ str> for Id {
    fn eq(&self, other: &&'_ str) -> bool {
        *self.sym == **other
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

impl PartialEq<str> for Id {
    fn eq(&self, other: &str) -> bool {
        self.sym == *other
    }
}

impl From<RModuleExportName> for Id {
    fn from(v: RModuleExportName) -> Self {
        match v {
            RModuleExportName::Ident(i) => i.into(),
            RModuleExportName::Str(s) => Id::word(s.value),
        }
    }
}

impl From<&'_ RModuleExportName> for Id {
    fn from(v: &RModuleExportName) -> Self {
        match v {
            RModuleExportName::Ident(i) => i.into(),
            RModuleExportName::Str(s) => Id::word(s.value.clone()),
        }
    }
}
