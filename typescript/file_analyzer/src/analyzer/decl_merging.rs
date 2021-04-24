use super::Analyzer;

impl Analyzer<'_, '_> {
    fn merge_from_to(&mut self, a: Type, b: Type) -> ValidationResult<Option<Type>> {
        debug_assert!(a.is_clone_cheap());
        debug_assert!(b.is_clone_cheap());

        match (a.normalize(), b.normalize()) {
            (Type::ClassDef(a), Type::Interface(b)) => {}
            _ => {}
        }

        Ok(None)
    }

    fn merge_types(&mut self, orig: Type, new: Type) -> ValidationResult<Type> {
        debug_assert!(orig.is_clone_cheap());
        debug_assert!(new.is_clone_cheap());

        if let Some(new_ty) = self.merge_from_to(orig.clone(), new.clone())? {
            return Ok(new_ty);
        }
        if let Some(new_ty) = self.merge_from_to(new.clone(), orig)? {
            return Ok(new_ty);
        }

        Ok(new)
    }

    pub(crate) fn merge_decl_with_name(&mut self, name: Id, new: Type) -> ValidationResult<Type> {
        let orig = self.find_type(self.ctx.module_id, &name)?;
        let mut orig = match orig {
            Some(v) => v,
            None => return Ok(new),
        };

        let orig = orig.next().unwrap().into_owned();

        self.merge_types(orig, new)
    }
}
