#[cfg(not(target_arch = "wasm32"))]
pub(crate) fn create_runtime() -> std::io::Result<tokio::runtime::Runtime> {
    tokio::runtime::Builder::new_multi_thread()
        .enable_all()
        .build()
}

#[cfg(target_arch = "wasm32")]
pub(crate) fn create_runtime() -> std::io::Result<tokio::runtime::Runtime> {
    tokio::runtime::Builder::new_current_thread()
        // .enable_all()
        .build()
}
