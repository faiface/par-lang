#[cfg(not(target_family = "wasm"))]
pub(crate) fn create_runtime() -> std::io::Result<tokio::runtime::Runtime> {
    tokio::runtime::Builder::new_multi_thread()
        .enable_all()
        .build()
}

#[cfg(target_family = "wasm")]
pub(crate) fn create_runtime() -> std::io::Result<tokio::runtime::Runtime> {
    tokio::runtime::Builder::new_current_thread()
        // .enable_all()
        .build()
}
