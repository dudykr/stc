use stc_ts_types::Type;
use std::mem::size_of;

#[test]
fn size_of_type() {
    assert_eq!(size_of::<Type>(), 224);
}
