// No-op. We just need an environment variable OUT_DIR

fn main() {
    println!("cargo:rerun-if-changed=build.rs");
}
