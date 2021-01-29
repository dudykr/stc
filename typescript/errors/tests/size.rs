use stc_ts_errors::Error;
use std::mem::size_of;

#[test]
fn size_of_type() {
    assert_eq!(size_of::<Error>(), 128);
}
