use tracing::Level;
use super::server::LanguageServer;

pub fn main() {
    tracing_subscriber::fmt()
        //.with_max_level(Level::DEBUG)
        .with_writer(std::io::stderr)
        .with_ansi(false)
        .init();

    tracing::info!("Starting Language Server v0.0.1");

    let (connection, io_threads) = lsp_server::Connection::stdio();

    LanguageServer::new(&connection).run();

    drop(connection);
    io_threads.join().expect("IO thread panicked");

    tracing::info!("Language server exited");
}