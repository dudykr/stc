use std::{
    collections::hash_map::Entry,
    mem::take,
    sync::{Arc, Mutex},
};

use auto_impl::auto_impl;
use fxhash::FxHashMap;
use stc_ts_errors::{Error, ErrorKind, Errors};
use stc_ts_types::{Id, ModuleId, ModuleTypeData, Type};
use stc_utils::cache::Freeze;
use swc_atoms::JsWord;
use swc_common::{iter::IdentifyLast, FileName, Span, SyntaxContext, TypeEq, DUMMY_SP};

#[derive(Debug, Default)]
pub struct Info {
    pub errors: Mutex<Errors>,
    pub exports: ModuleTypeData,
}

pub type Storage<'b> = Box<dyn 'b + Mode>;

#[auto_impl(&mut, Box)]
pub trait ErrorStore {
    fn report(&self, err: Error);
    fn report_all(&self, err: Errors);
    fn take_errors(&self) -> Errors;
}

#[auto_impl(&mut, Box)]
pub trait TypeStore: Send + Sync {
    fn get_local_type(&self, ctxt: ModuleId, id: Id) -> Option<Type>;
    fn get_local_var(&self, ctxt: ModuleId, id: Id) -> Option<Type>;

    fn store_private_type(&mut self, ctxt: ModuleId, id: Id, ty: Type, should_override: bool);
    fn store_private_var(&mut self, ctxt: ModuleId, id: Id, ty: Type);

    fn export_stored_type(&mut self, span: Span, ctxt: ModuleId, id: Id, orig_name: Id);
    fn export_stored_var(&mut self, span: Span, ctxt: ModuleId, id: Id, orig_name: Id);

    fn export_type(&mut self, span: Span, ctxt: ModuleId, id: JsWord, ty: Type);
    fn export_var(&mut self, span: Span, ctxt: ModuleId, id: JsWord, ty: Type);

    fn take_info(&mut self, ctxt: ModuleId) -> ModuleTypeData;
}

/// Metadata for the input provided to the analyzer.
///
/// The analyzer can work on multiple module at once, in case of circular
/// imports.
#[auto_impl(&mut, Box)]
pub trait Mode: TypeStore + ErrorStore {
    /// Returns the id of the module where statement #`stmt_index` came from.
    fn module_id(&self, stmt_index: usize) -> ModuleId;

    fn top_level_ctxt(&self, module_id: ModuleId) -> SyntaxContext;

    fn is_dts(&self) -> bool;

    fn path(&self, id: ModuleId) -> Arc<FileName>;

    fn subscope(&self) -> Storage;

    fn merge_back(&mut self, subscope: Storage) {
        let ss = subscope;
        let errors = ss.take_errors();
        self.report_all(errors);
    }
}

#[derive(Debug)]
pub struct Single<'a> {
    pub parent: Option<&'a Single<'a>>,
    pub id: ModuleId,
    pub top_level_ctxt: SyntaxContext,
    pub path: Arc<FileName>,
    pub is_dts: bool,
    pub info: Info,
}

impl ErrorStore for Single<'_> {
    fn report(&self, err: Error) {
        self.info.errors.lock().unwrap().push(err);
    }

    fn report_all(&self, err: Errors) {
        self.info.errors.lock().unwrap().extend(err);
    }

    fn take_errors(&self) -> Errors {
        take(&mut self.info.errors.lock().unwrap())
    }
}

impl TypeStore for Single<'_> {
    fn store_private_type(&mut self, ctxt: ModuleId, id: Id, ty: Type, should_override: bool) {
        debug_assert_eq!(ctxt, self.id);
        ty.assert_clone_cheap();

        if should_override {
            if self.info.exports.types.contains_key(id.sym()) {
                self.info.exports.types.insert(id.sym().clone(), vec![ty.clone()]);
            }
            self.info.exports.private_types.insert(id, vec![ty]);
        } else {
            self.info.exports.private_types.entry(id).or_default().push(ty);
        }
    }

    fn store_private_var(&mut self, ctxt: ModuleId, id: Id, ty: Type) {
        debug_assert_eq!(ctxt, self.id);
        ty.assert_clone_cheap();

        match self.info.exports.private_vars.entry(id) {
            Entry::Occupied(e) => {
                if e.get().type_eq(&ty) {
                    return;
                }

                let (id, prev_ty) = e.remove_entry();
                self.info
                    .exports
                    .private_vars
                    .insert(id, Type::new_union(DUMMY_SP, vec![prev_ty, ty]).freezed());
            }
            Entry::Vacant(e) => {
                e.insert(ty);
            }
        }
    }

    fn export_stored_var(&mut self, span: Span, ctxt: ModuleId, id: Id, orig_name: Id) {
        debug_assert_eq!(ctxt, self.id);

        match self.info.exports.private_vars.get(&orig_name).cloned() {
            Some(ty) => if self.info.exports.vars.insert(id.sym().clone(), ty).is_some() {},
            None => self.report(ErrorKind::NoSuchVar { span, name: id }.into()),
        }
    }

    fn export_stored_type(&mut self, span: Span, ctxt: ModuleId, id: Id, orig_name: Id) {
        debug_assert_eq!(ctxt, self.id);

        match self.info.exports.private_types.get(&orig_name).cloned() {
            Some(ty) => {
                *self.info.exports.types.entry(id.sym().clone()).or_default() = ty;
            }
            None => self.report(ErrorKind::NoSuchVar { span, name: id }.into()),
        }
    }

    fn get_local_type(&self, ctxt: ModuleId, id: Id) -> Option<Type> {
        debug_assert_eq!(ctxt, self.id);
        let ty = self
            .info
            .exports
            .private_types
            .get(&id)
            .map(|types| Type::new_intersection(DUMMY_SP, types.iter().cloned()).freezed());

        match ty {
            Some(ty) => Some(ty),
            None => self.parent?.get_local_type(ctxt, id),
        }
    }

    fn get_local_var(&self, ctxt: ModuleId, id: Id) -> Option<Type> {
        debug_assert_eq!(ctxt, self.id);

        match self.info.exports.private_vars.get(&id) {
            Some(v) => Some(v.clone()),
            None => self.parent?.get_local_var(ctxt, id),
        }
    }

    fn take_info(&mut self, ctxt: ModuleId) -> ModuleTypeData {
        debug_assert_eq!(ctxt, self.id);
        take(&mut self.info.exports)
    }

    fn export_type(&mut self, _span: Span, _ctxt: ModuleId, id: JsWord, ty: Type) {
        ty.assert_clone_cheap();

        self.info.exports.types.entry(id).or_default().push(ty);
    }

    fn export_var(&mut self, _span: Span, _ctxt: ModuleId, id: JsWord, ty: Type) {
        ty.assert_clone_cheap();

        // TODO(kdy1): error reporting for duplicate
        self.info.exports.vars.insert(id, ty);
    }
}

impl<'a> Mode for Single<'a> {
    fn module_id(&self, _stmt_index: usize) -> ModuleId {
        self.id
    }

    fn top_level_ctxt(&self, _: ModuleId) -> SyntaxContext {
        self.top_level_ctxt
    }

    fn is_dts(&self) -> bool {
        self.is_dts
    }

    fn path(&self, id: ModuleId) -> Arc<FileName> {
        debug_assert_eq!(id, self.id);
        self.path.clone()
    }

    fn subscope(&self) -> Storage {
        Box::new(Single {
            parent: Some(self),
            id: self.id,
            top_level_ctxt: self.top_level_ctxt,
            path: self.path.clone(),
            is_dts: self.is_dts,
            info: Default::default(),
        })
    }
}

#[derive(Debug, Clone)]
pub struct File {
    pub id: ModuleId,
    pub top_level_ctxt: SyntaxContext,
    pub path: Arc<FileName>,
    pub stmt_count: usize,
}

#[derive(Debug)]
pub struct Group<'a> {
    pub parent: Option<&'a Group<'a>>,
    pub files: Arc<Vec<File>>,
    pub errors: Mutex<Errors>,
    pub info: FxHashMap<ModuleId, ModuleTypeData>,
}

impl ErrorStore for Group<'_> {
    fn report(&self, err: Error) {
        self.errors.lock().unwrap().push(err);
    }

    fn report_all(&self, err: Errors) {
        self.errors.lock().unwrap().extend(err);
    }

    fn take_errors(&self) -> Errors {
        take(&mut self.errors.lock().unwrap())
    }
}

impl TypeStore for Group<'_> {
    fn store_private_type(&mut self, ctxt: ModuleId, id: Id, ty: Type, should_override: bool) {
        if should_override {
            if self.info.entry(ctxt).or_default().types.contains_key(id.sym()) {
                self.info.entry(ctxt).or_default().types.insert(id.sym().clone(), vec![ty.clone()]);
            }

            self.info.entry(ctxt).or_default().private_types.insert(id, vec![ty]);
        } else {
            self.info.entry(ctxt).or_default().private_types.entry(id).or_default().push(ty);
        }
    }

    fn store_private_var(&mut self, ctxt: ModuleId, id: Id, ty: Type) {
        let map = self.info.entry(ctxt).or_default();

        match map.private_vars.entry(id) {
            Entry::Occupied(e) => {
                let (id, prev_ty) = e.remove_entry();
                map.private_vars.insert(id, Type::new_union(DUMMY_SP, vec![prev_ty, ty]).freezed());
            }
            Entry::Vacant(e) => {
                e.insert(ty);
            }
        }
    }

    fn export_stored_var(&mut self, span: Span, ctxt: ModuleId, id: Id, orig_name: Id) {
        let e = self.info.entry(ctxt).or_default();
        match e.private_vars.get(&orig_name) {
            Some(v) => {
                e.vars.insert(id.sym().clone(), v.clone());
            }
            None => {
                dbg!();
                self.report(ErrorKind::NoSuchVar { span, name: id }.into())
            }
        }
    }

    fn export_stored_type(&mut self, span: Span, ctxt: ModuleId, id: Id, orig_name: Id) {
        let e = self.info.entry(ctxt).or_default();
        match e.private_types.get(&orig_name) {
            Some(v) => {
                e.types.insert(id.sym().clone(), v.clone());
            }
            None => self.report(ErrorKind::NoSuchType { span, name: id }.into()),
        }
    }

    fn get_local_type(&self, ctxt: ModuleId, id: Id) -> Option<Type> {
        let ty = self
            .info
            .get(&ctxt)?
            .private_types
            .get(&id)
            .map(|types| Type::new_intersection(DUMMY_SP, types.iter().cloned()));
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

    fn export_type(&mut self, _span: Span, ctxt: ModuleId, id: JsWord, ty: Type) {
        self.info.entry(ctxt).or_default().types.entry(id).or_default().push(ty);
    }

    fn export_var(&mut self, _span: Span, ctxt: ModuleId, id: JsWord, ty: Type) {
        // TODO(kdy1): Error reporting for duplicates
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

    fn top_level_ctxt(&self, stmt_index: ModuleId) -> SyntaxContext {
        for file in self.files.iter() {
            if file.id == stmt_index {
                return file.top_level_ctxt;
            }
        }

        unreachable!("failed to get top level ctxt")
    }

    fn is_dts(&self) -> bool {
        false
    }

    fn path(&self, id: ModuleId) -> Arc<FileName> {
        for file in self.files.iter() {
            if file.id == id {
                return file.path.clone();
            }
        }

        unreachable!("failed to get path by module id({:?}):  {:?}", id, self.files)
    }

    fn subscope(&self) -> Storage {
        Box::new(Group {
            parent: Some(self),
            files: self.files.clone(),
            errors: Default::default(),
            info: Default::default(),
        })
    }
}

#[derive(Debug, Default)]
pub struct Builtin {
    pub vars: FxHashMap<JsWord, Type>,
    pub types: FxHashMap<JsWord, Vec<Type>>,
}

impl ErrorStore for Builtin {
    fn report(&self, err: Error) {
        unreachable!("builtin error: {:?}", err);
    }

    fn report_all(&self, err: Errors) {
        if err.is_empty() {
            return;
        }
        unreachable!("builtin error: {:?}", err);
    }

    fn take_errors(&self) -> Errors {
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
                self.vars.insert(id, Type::new_intersection(DUMMY_SP, vec![prev_ty, ty]));
            }
            Entry::Vacant(entry) => {
                entry.insert(ty);
            }
        }
    }

    fn export_stored_var(&mut self, _: Span, _: ModuleId, _: Id, _: Id) {}

    fn export_stored_type(&mut self, _: Span, _: ModuleId, _: Id, _: Id) {}

    fn get_local_type(&self, _ctxt: ModuleId, id: Id) -> Option<Type> {
        let types = self.types.get(id.sym()).cloned()?;
        Some(Type::new_intersection(DUMMY_SP, types))
    }

    fn get_local_var(&self, _ctxt: ModuleId, id: Id) -> Option<Type> {
        self.vars.get(id.sym()).cloned()
    }

    fn take_info(&mut self, _ctxt: ModuleId) -> ModuleTypeData {
        unimplemented!("builtin.take_info")
    }

    fn export_type(&mut self, _: Span, _: ModuleId, _: JsWord, _: Type) {}

    fn export_var(&mut self, _: Span, _: ModuleId, _: JsWord, _: Type) {}
}

impl Mode for Builtin {
    fn module_id(&self, _stmt_index: usize) -> ModuleId {
        ModuleId::builtin()
    }

    fn top_level_ctxt(&self, _: ModuleId) -> SyntaxContext {
        SyntaxContext::empty()
    }

    fn is_dts(&self) -> bool {
        true
    }

    fn path(&self, _: ModuleId) -> Arc<FileName> {
        unreachable!("builtin.path()")
    }

    fn subscope(&self) -> Storage {
        Box::<Builtin>::default()
    }
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use stc_ts_types::module_id;

    use super::*;

    #[test]
    fn group_01() {
        testing::run_test(false, |_, _| {
            let gen = module_id::ModuleIdGenerator::default();

            let path1 = Arc::new(FileName::Real(PathBuf::from("1")));
            let file1 = File {
                id: gen.generate(&path1).0,
                top_level_ctxt: SyntaxContext::empty().apply_mark(gen.generate(&path1).1),
                path: path1.clone(),
                stmt_count: 4,
            };
            let path2 = Arc::new(FileName::Real(PathBuf::from("2")));
            let file2 = File {
                id: gen.generate(&path2).0,
                top_level_ctxt: SyntaxContext::empty().apply_mark(gen.generate(&path2).1),
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

            Ok(())
        })
        .unwrap();
    }
}
