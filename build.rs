fn main() {
    scx_rustland_core::RustLandBuilder::new()
        .unwrap()
        .build()
        .unwrap();

    // scx_rustland creates a file at src/bpf.rs by default.
    // we need to move it after creation -- to have it at another path
    let src = concat!(env!("CARGO_MANIFEST_DIR"), "/src/bpf.rs");
    let dst = concat!(env!("CARGO_MANIFEST_DIR"), "/src/bpf/bpf.rs");

    let buf = std::fs::read_to_string(&src)
        .unwrap()
        .replace("crate::bpf_intf", "crate::bpf::bpf_intf")
        .replace("crate::bpf_skel", "crate::bpf::bpf_skel");
    std::fs::remove_file(src).unwrap();
    std::fs::write(dst, buf).unwrap();
}
