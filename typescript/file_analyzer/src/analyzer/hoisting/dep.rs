use fxhash::FxHashMap;
use fxhash::FxHashSet;
use petgraph::graphmap::DiGraphMap;
use rnode::Visit;
use rnode::VisitWith;
use stc_ts_ast_rnode::RIdent;
use stc_ts_types::Id;
use swc_ecma_ast::VarDeclKind;

use crate::util::AsModuleDecl;

#[derive(Default)]
pub struct VarInfo {
    pub is_cyclic: bool,
    pub kind: Option<VarDeclKind>,
    pub used_before_decl: bool,
}

#[derive(Default)]
pub struct AnalysisResult {
    pub hoisted_vars: FxHashMap<Id, VarInfo>,
}

pub(super) fn analyze<N>(n: &N) -> AnalysisResult
where
    N: VisitWith<DepAnalyzer>,
{
    let mut graph = DiGraphMap::default();
}

fn vars_created_by<T>(n: &T)
where
    T: AsModuleDecl,
{
}

#[derive(Default)]
pub(super) struct DepAnalyzer {
    data: AnalysisResult,
}

impl Visit<RIdent> for DepAnalyzer {
    fn visit(&mut self, i: &RIdent) {
        // TODO
    }
}
