use rnode::Visit;
use stc_ts_ast_rnode::RModule;
use stc_ts_ast_rnode::RProgram;
use stc_ts_ast_rnode::RScript;
use stc_ts_errors::Error;

pub trait Pass: Visit<RModule> + Visit<RScript> + Visit<RProgram> {
    fn name() -> &'static str;

    fn take_errors(&mut self) -> Vec<Error>;
}
