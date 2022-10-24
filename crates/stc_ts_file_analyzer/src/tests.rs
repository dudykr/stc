use std::sync::Arc;

use once_cell::sync::Lazy;
use stc_ts_env::Marks;
use swc_common::Globals;

pub(crate) static GLOBALS: Lazy<Arc<Globals>> = Lazy::new(Default::default);

pub(crate) static MARKS: Lazy<Marks> = Lazy::new(|| Marks::new(&*GLOBALS));
