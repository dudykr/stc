use once_cell::sync::Lazy;
use std::{
    collections::hash_map::Entry,
    fmt::{self, Debug, Display, Formatter},
    path::PathBuf,
    sync::{Arc, Mutex},
};
use swc_common::{collections::AHashMap, FileName};

#[derive(Default)]
struct Generator {
    last_id: u32,
    ids: AHashMap<Arc<FileName>, u32>,
    paths: AHashMap<u32, Arc<FileName>>,
}

fn with<T>(f: impl FnOnce(&mut Generator) -> T) -> T {
    static GENERATOR: Lazy<Mutex<Generator>> = Lazy::new(|| Default::default());

    let mut lock = GENERATOR.lock().unwrap();

    f(&mut *lock)
}

/// An interned path.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FileId(u32);

impl Debug for FileId {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(self, f)
    }
}

impl Display for FileId {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let id = self.0;
        let path = with(|g| g.paths.get(&id).cloned()).unwrap();

        write!(f, "{}", path)
    }
}

impl From<Arc<FileName>> for FileId {
    fn from(path: Arc<FileName>) -> Self {
        with(|g| {
            let cur_id = g.last_id + 1;
            //
            match g.ids.entry(path.clone()) {
                Entry::Occupied(e) => return FileId(*e.get()),
                Entry::Vacant(e) => {
                    e.insert(cur_id);
                }
            }

            g.paths.insert(cur_id, path);

            FileId(cur_id)
        })
    }
}

impl From<PathBuf> for FileId {
    fn from(path: PathBuf) -> Self {
        Self::from(FileName::Real(path))
    }
}

impl From<FileName> for FileId {
    fn from(path: FileName) -> Self {
        Self::from(Arc::new(path))
    }
}

impl FileId {
    pub fn path(&self) -> Arc<FileName> {
        with(|g| g.paths.get(&self.0).cloned()).unwrap()
    }
    pub fn get(path: impl Into<FileName>) -> Self {
        let path = path.into();

        Arc::new(path).into()
    }
}
