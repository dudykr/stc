// We need OUT_DIR

fn main() {
    println!("cargo:rerun-if-changed=build.rs");
}
