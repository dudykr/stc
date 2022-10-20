/// Handles folding of Arc<Type> and Static
#[macro_export]
macro_rules! fold_helper {
    () => {
        fn fold_arc(&mut self, ty: Arc<$crate::Type>) -> Arc<$crate::Type> {
            ty
        }

        fn fold_static(&mut self, ty: $crate::Static) -> $crate::Static {
            ty
        }
    };
}

/// Handles visiting of Arc<Type> and Static
#[macro_export]
macro_rules! visit_helper {
    () => {
        fn visit_arc(&mut self, ty: &Arc<$crate::Type>, parent: &dyn $crate::TypeNode) -> Arc<$crate::Type> {
            self.visit_type(&**ty, parent);
        }

        fn visit_static(&mut self, ty: $crate::Static, parent: &dyn $crate::TypeNode) -> $crate::Static {
            self.visit_type(ty.ty, parent);
        }
    };
}
