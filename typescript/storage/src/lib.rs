#![feature(box_syntax)]

use fxhash::FxHashMap;
use stc_ts_errors::{Error, Errors};
use stc_ts_types::{Id, ModuleId, ModuleTypeData, Type};
use stc_utils::path::intern::FileId;
use std::{collections::hash_map::Entry, mem::take, path::PathBuf, sync::Arc};
use swc_atoms::JsWord;
use swc_common::{iter::IdentifyLast, Span, TypeEq, DUMMY_SP};

#[derive(Debug, Default)]
pub struct Info {
    pub errors: Errors,
    pub exports: ModuleTypeData,
}

pub type Storage<'b> = Box<dyn 'b + Mode>;

pub trait ErrorStore {
    fn report(&mut self, err: Error);
    fn report_all(&mut self, err: Errors);
    fn take_errors(&mut self) -> Errors;
}
pub trait TypeStore: Send + Sync {
    fn get_local_type(&self, ctxt: ModuleId, id: Id) -> Option<Type>;
    fn get_local_var(&self, ctxt: ModuleId, id: Id) -> Option<Type>;

    fn store_private_type(&mut self, ctxt: ModuleId, id: Id, ty: Type, should_override: bool);
    fn store_private_var(&mut self, ctxt: ModuleId, id: Id, ty: Type);

    fn export_type(&mut self, span: Span, ctxt: ModuleId, id: Id);
    fn export_var(&mut self, span: Span, ctxt: ModuleId, id: Id, orig_name: Id);

    fn reexport_type(&mut self, span: Span, ctxt: ModuleId, id: JsWord, ty: Type);
    fn reexport_var(&mut self, span: Span, ctxt: ModuleId, id: JsWord, ty: Type);

    fn take_info(&mut self, ctxt: ModuleId) -> ModuleTypeData;
}

/// Metadata for the input provided to the analyser.
///
/// The analyzer can work on multiple module at once, in case of circular
/// imports.
pub trait Mode: TypeStore + ErrorStore {
    /// Returns the id of the module where statement #`stmt_index` came from.
    fn module_id(&self, stmt_index: usize) -> ModuleId;

    fn path(&self, id: ModuleId) -> Arc<PathBuf>;

    fn subscope(&self) -> Storage;

    fn merge_back(&mut self, mut subscope: Storage) {
        let errors = subscope.take_errors();
        self.report_all(errors);
    }
}

impl<'a, T> ErrorStore for &'a mut T
where
    T: ErrorStore,
{
    fn report(&mut self, err: Error) {
        (**self).report(err);
    }

    fn report_all(&mut self, err: Errors) {
        (**self).report_all(err);
    }

    fn take_errors(&mut self) -> Errors {
        (**self).take_errors()
    }
}

impl<'a, T> TypeStore for &'a mut T
where
    T: TypeStore,
{
    fn get_local_type(&self, ctxt: ModuleId, id: Id) -> Option<Type> {
        (**self).get_local_type(ctxt, id)
    }

    fn get_local_var(&self, ctxt: ModuleId, id: Id) -> Option<Type> {
        (**self).get_local_var(ctxt, id)
    }

    fn store_private_type(&mut self, ctxt: ModuleId, id: Id, ty: Type, should_override: bool) {
        (**self).store_private_type(ctxt, id, ty, should_override)
    }

    fn store_private_var(&mut self, ctxt: ModuleId, id: Id, ty: Type) {
        (**self).store_private_var(ctxt, id, ty)
    }

    fn export_type(&mut self, span: Span, ctxt: ModuleId, id: Id) {
        (**self).export_type(span, ctxt, id)
    }

    fn export_var(&mut self, span: Span, ctxt: ModuleId, id: Id, orig_name: Id) {
        (**self).export_var(span, ctxt, id, orig_name)
    }

    fn take_info(&mut self, ctxt: ModuleId) -> ModuleTypeData {
        (**self).take_info(ctxt)
    }

    fn reexport_type(&mut self, span: Span, ctxt: ModuleId, id: JsWord, ty: Type) {
        (**self).reexport_type(span, ctxt, id, ty)
    }

    fn reexport_var(&mut self, span: Span, ctxt: ModuleId, id: JsWord, ty: Type) {
        (**self).reexport_var(span, ctxt, id, ty)
    }
}

impl<'a, T> Mode for &'a mut T
where
    T: Mode,
{
    fn module_id(&self, stmt_index: usize) -> ModuleId {
        (**self).module_id(stmt_index)
    }

    fn path(&self, id: ModuleId) -> Arc<PathBuf> {
        (**self).path(id)
    }

    fn subscope(&self) -> Storage {
        (**self).subscope()
    }
}

#[derive(Debug)]
pub struct Single<'a> {
    pub parent: Option<&'a Single<'a>>,
    pub id: ModuleId,
    pub path: Arc<PathBuf>,
    pub info: Info,
}

impl ErrorStore for Single<'_> {
    fn report(&mut self, err: Error) {
        self.info.errors.push(err);
    }

    fn report_all(&mut self, err: Errors) {
        self.info.errors.extend(err);
    }

    fn take_errors(&mut self) -> Errors {
        take(&mut self.info.errors)
    }
}

impl TypeStore for Single<'_> {
    fn store_private_type(&mut self, ctxt: ModuleId, id: Id, ty: Type, should_override: bool) {
        debug_assert_eq!(ctxt, self.id);

        if should_override {
            if self.info.exports.types.contains_key(&id.sym()) {
                self.info.exports.types.insert(id.sym().clone(), vec![ty.clone()]);
            }
            self.info.exports.private_types.insert(id, vec![ty]);
        } else {
            self.info.exports.private_types.entry(id).or_default().push(ty);
        }
    }

    fn store_private_var(&mut self, ctxt: ModuleId, id: Id, ty: Type) {
        debug_assert_eq!(ctxt, self.id);

        match self.info.exports.private_vars.entry(id) {
            Entry::Occupied(e) => {
                if e.get().type_eq(&ty) {
                    return;
                }

                let (id, prev_ty) = e.remove_entry();
                self.info
                    .exports
                    .private_vars
                    .insert(id, Type::union(vec![prev_ty, ty]));
            }
            Entry::Vacant(e) => {
                e.insert(ty);
            }
        }
    }

    fn export_var(&mut self, span: Span, ctxt: ModuleId, id: Id, orig_name: Id) {
        debug_assert_eq!(ctxt, self.id);

        match self.info.exports.private_vars.get(&orig_name).cloned() {
            Some(ty) => match self.info.exports.vars.insert(id.sym().clone(), ty) {
                Some(..) => {}
                None => {}
            },
            None => {
                dbg!();
                self.report(Error::NoSuchVar { span, name: id })
            }
        }
    }

    fn export_type(&mut self, span: Span, ctxt: ModuleId, id: Id) {
        debug_assert_eq!(ctxt, self.id);

        match self.info.exports.private_types.get(&id).cloned() {
            Some(ty) => {
                *self.info.exports.types.entry(id.sym().clone()).or_default() = ty.clone();
            }
            None => {
                dbg!();
                self.report(Error::NoSuchVar { span, name: id })
            }
        }
    }

    fn get_local_type(&self, ctxt: ModuleId, id: Id) -> Option<Type> {
        debug_assert_eq!(ctxt, self.id);
        let ty = self
            .info
            .exports
            .private_types
            .get(&id)
            .map(|types| Type::intersection(DUMMY_SP, types.iter().cloned()));

        match ty {
            Some(ty) => return Some(ty),
            None => self.parent?.get_local_type(ctxt, id),
        }
    }

    fn get_local_var(&self, ctxt: ModuleId, id: Id) -> Option<Type> {
        debug_assert_eq!(ctxt, self.id);

        match self.info.exports.private_vars.get(&id) {
            Some(v) => return Some(v.clone()),
            None => self.parent?.get_local_var(ctxt, id),
        }
    }

    fn take_info(&mut self, ctxt: ModuleId) -> ModuleTypeData {
        debug_assert_eq!(ctxt, self.id);
        take(&mut self.info.exports)
    }

    fn reexport_type(&mut self, _span: Span, ctxt: ModuleId, id: JsWord, ty: Type) {
        debug_assert_eq!(ctxt, self.id);
        self.info.exports.types.entry(id).or_default().push(ty);
    }

    fn reexport_var(&mut self, _span: Span, ctxt: ModuleId, id: JsWord, ty: Type) {
        debug_assert_eq!(ctxt, self.id);
        // TODO: error reporting for duplicate
        self.info.exports.vars.insert(id, ty);
    }
}

impl<'a> Mode for Single<'a> {
    fn module_id(&self, _stmt_index: usize) -> ModuleId {
        self.id
    }

    fn path(&self, id: ModuleId) -> Arc<PathBuf> {
        debug_assert_eq!(id, self.id);
        self.path.clone()
    }

    fn subscope(&self) -> Storage {
        box Single {
            parent: Some(self),
            id: self.id,
            path: self.path.clone(),
            info: Default::default(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct File {
    pub id: ModuleId,
    pub path: FileId,
    pub stmt_count: usize,
}

#[derive(Debug)]
pub struct Group<'a> {
    pub parent: Option<&'a Group<'a>>,
    pub files: Arc<Vec<File>>,
    pub errors: Errors,
    pub info: FxHashMap<ModuleId, ModuleTypeData>,
}

impl ErrorStore for Group<'_> {
    fn report(&mut self, err: Error) {
        self.errors.push(err);
    }

    fn report_all(&mut self, err: Errors) {
        self.errors.extend(err);
    }

    fn take_errors(&mut self) -> Errors {
        take(&mut self.errors)
    }
}

impl TypeStore for Group<'_> {
    fn store_private_type(&mut self, ctxt: ModuleId, id: Id, ty: Type, should_override: bool) {
        if should_override {
            if self.info.entry(ctxt).or_default().types.contains_key(&id.sym()) {
                self.info
                    .entry(ctxt)
                    .or_default()
                    .types
                    .insert(id.sym().clone(), vec![ty.clone()]);
            }

            self.info.entry(ctxt).or_default().private_types.insert(id, vec![ty]);
        } else {
            self.info
                .entry(ctxt)
                .or_default()
                .private_types
                .entry(id)
                .or_default()
                .push(ty);
        }
    }

    fn store_private_var(&mut self, ctxt: ModuleId, id: Id, ty: Type) {
        let map = self.info.entry(ctxt).or_default();

        match map.private_vars.entry(id) {
            Entry::Occupied(e) => {
                let (id, prev_ty) = e.remove_entry();
                map.private_vars.insert(id, Type::union(vec![prev_ty, ty]));
            }
            Entry::Vacant(e) => {
                e.insert(ty);
            }
        }
    }

    fn export_var(&mut self, span: Span, ctxt: ModuleId, id: Id, orig_name: Id) {
        let e = self.info.entry(ctxt).or_default();
        match e.private_vars.get(&orig_name) {
            Some(v) => {
                e.vars.insert(id.sym().clone(), v.clone());
            }
            None => {
                dbg!();
                self.report(Error::NoSuchVar { span, name: id })
            }
        }
    }

    fn export_type(&mut self, span: Span, ctxt: ModuleId, id: Id) {
        let e = self.info.entry(ctxt).or_default();
        match e.private_types.get(&id) {
            Some(v) => {
                e.types.insert(id.sym().clone(), v.clone());
            }
            None => self.report(Error::NoSuchType { span, name: id }),
        }
    }

    fn get_local_type(&self, ctxt: ModuleId, id: Id) -> Option<Type> {
        let ty = self
            .info
            .get(&ctxt)?
            .private_types
            .get(&id)
            .map(|types| Type::intersection(DUMMY_SP, types.iter().cloned()));
        match ty {
            Some(ty) => Some(ty),
            None => self.parent?.get_local_type(ctxt, id),
        }
    }

    fn get_local_var(&self, ctxt: ModuleId, id: Id) -> Option<Type> {
        match self.info.get(&ctxt)?.private_vars.get(&id).cloned() {
            Some(ty) => Some(ty),
            None => self.parent?.get_local_var(ctxt, id),
        }
    }

    fn take_info(&mut self, ctxt: ModuleId) -> ModuleTypeData {
        self.info.remove(&ctxt).unwrap_or_default()
    }

    fn reexport_type(&mut self, _span: Span, ctxt: ModuleId, id: JsWord, ty: Type) {
        self.info.entry(ctxt).or_default().types.entry(id).or_default().push(ty);
    }

    fn reexport_var(&mut self, _span: Span, ctxt: ModuleId, id: JsWord, ty: Type) {
        // TODO: Error reporting for duplicates
        self.info.entry(ctxt).or_default().vars.insert(id, ty);
    }
}

impl Mode for Group<'_> {
    fn module_id(&self, stmt_index: usize) -> ModuleId {
        let mut cur = 0;
        for (last, file) in self.files.iter().identify_last() {
            if cur <= stmt_index && stmt_index < cur + file.stmt_count {
                return file.id;
            }

            if last {
                return file.id;
            }

            cur += file.stmt_count;
        }

        unreachable!("failed to get module id")
    }

    fn path(&self, id: ModuleId) -> Arc<PathBuf> {
        for file in self.files.iter() {
            if file.id == id {
                return file.path.clone();
            }
        }

        unreachable!("failed to get path by module id({:?}):  {:?}", id, self.files)
    }

    fn subscope(&self) -> Storage {
        box Group {
            parent: Some(self),
            files: self.files.clone(),
            errors: Default::default(),
            info: Default::default(),
        }
    }
}

#[derive(Debug, Default)]
pub struct Builtin {
    pub vars: FxHashMap<JsWord, Type>,
    pub types: FxHashMap<JsWord, Vec<Type>>,
}

impl ErrorStore for Builtin {
    fn report(&mut self, err: Error) {
        unreachable!("builtin error: {:?}", err);
    }

    fn report_all(&mut self, err: Errors) {
        if err.is_empty() {
            return;
        }
        unreachable!("builtin error: {:?}", err);
    }

    fn take_errors(&mut self) -> Errors {
        Default::default()
    }
}

impl TypeStore for Builtin {
    fn store_private_type(&mut self, ctxt: ModuleId, id: Id, ty: Type, should_override: bool) {
        debug_assert_eq!(ctxt, ModuleId::builtin());
        debug_assert!(!should_override);

        self.types.entry(id.sym().clone()).or_default().push(ty);
    }

    fn store_private_var(&mut self, ctxt: ModuleId, id: Id, ty: Type) {
        debug_assert_eq!(ctxt, ModuleId::builtin());

        match self.vars.entry(id.sym().clone()) {
            Entry::Occupied(entry) => {
                let (id, prev_ty) = entry.remove_entry();
                self.vars.insert(id, Type::intersection(DUMMY_SP, vec![prev_ty, ty]));
            }
            Entry::Vacant(entry) => {
                entry.insert(ty);
            }
        }
    }

    fn export_var(&mut self, _: Span, _: ModuleId, _: Id, _: Id) {}

    fn export_type(&mut self, _: Span, _: ModuleId, _: Id) {}

    fn get_local_type(&self, _ctxt: ModuleId, id: Id) -> Option<Type> {
        let types = self.types.get(id.sym()).cloned()?;
        Some(Type::intersection(DUMMY_SP, types))
    }

    fn get_local_var(&self, _ctxt: ModuleId, id: Id) -> Option<Type> {
        self.vars.get(id.sym()).cloned()
    }

    fn take_info(&mut self, _ctxt: ModuleId) -> ModuleTypeData {
        unimplemented!("builtin.take_info")
    }

    fn reexport_type(&mut self, _: Span, _: ModuleId, _: JsWord, _: Type) {}

    fn reexport_var(&mut self, _: Span, _: ModuleId, _: JsWord, _: Type) {}
}

impl Mode for Builtin {
    fn module_id(&self, _stmt_index: usize) -> ModuleId {
        ModuleId::builtin()
    }

    fn path(&self, _: ModuleId) -> Arc<PathBuf> {
        unreachable!("builtin.path()")
    }

    fn subscope(&self) -> Storage {
        box Builtin::default()
    }
}

#[cfg(test)]
mod tests {
    use stc_ts_types::module_id;
    use stc_utils::path::intern::FileId;

    use super::*;

    #[test]
    fn group_01() {
        let gen = module_id::Generator::default();

        let path1 = FileId::from(PathBuf::from("1"));
        let file1 = File {
            id: gen.generate(path1).1,
            path: path1.clone(),
            stmt_count: 4,
        };
        let path2 = FileId::from(PathBuf::from("2"));
        let file2 = File {
            id: gen.generate(path2).1,
            path: path2.clone(),
            stmt_count: 5,
        };
        let group = Group {
            parent: None,
            files: Arc::new(vec![file1.clone(), file2.clone()]),
            info: Default::default(),
            errors: Default::default(),
        };

        assert_eq!(group.module_id(0), file1.id);
        assert_eq!(group.module_id(1), file1.id);
        assert_eq!(group.module_id(2), file1.id);
        assert_eq!(group.module_id(3), file1.id);

        assert_eq!(group.module_id(4), file2.id);
        assert_eq!(group.module_id(5), file2.id);
        assert_eq!(group.module_id(6), file2.id);
        assert_eq!(group.module_id(7), file2.id);
        assert_eq!(group.module_id(8), file2.id);
    }
}
