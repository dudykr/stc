use rnode::Visit;
use stc_ts_types::{MethodSignature, PropertySignature, Union};

#[derive(Debug, Default)]
pub struct UnionFinder {
    pub found: bool,
}

impl Visit<PropertySignature> for UnionFinder {
    fn visit(&mut self, _: &PropertySignature) {}
}

impl Visit<MethodSignature> for UnionFinder {
    fn visit(&mut self, _: &MethodSignature) {}
}

impl Visit<Union> for UnionFinder {
    fn visit(&mut self, _: &Union) {
        self.found = true;
    }
}
