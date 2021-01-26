use crate::analyzer::Analyzer;
use stc_ts_types::Type;

impl Analyzer<'_, '_> {
    /// Convert type parameters declared in dead scopes to `{}`.
    ///
    /// This does not touch type parameters declared in parent scopes and this
    /// method should be callled when a type had escaped a scope.
    ///
    /// In this way, we can handle both of cases below.
    ///
    /// ## Invalid assignment.
    ///
    /// ```ts
    /// function foo<T, U>() {
    ///     var a: T;
    ///     var b: U;
    ///     a = b; // This is wrong.
    /// }
    /// ```
    ///
    /// This case is handled because type parameters are not touched (while we
    /// are analyzing function) by the method.
    ///
    /// ## Escaping
    ///
    /// ```ts
    /// function foo<T>() {}
    /// function bar<T>() {}
    ///
    /// var a = foo(); // type is {}
    /// a = bar();
    /// ```
    ///
    /// This method is called at the end of each call and each `T` is converted
    /// to `{}` even though span hygiene differs.
    pub(crate) fn replace_invalid_type_params(&mut self, ty: Box<Type>) -> Box<Type> {}
}

struct TypeParamHandler<'a, 'b, 'c> {
    analyzer: &'a mut Analyzer<'b, 'c>,
}
