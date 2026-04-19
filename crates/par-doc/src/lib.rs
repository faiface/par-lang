mod error;
mod html;
mod load;
mod model;
mod paths;
mod render;

use std::path::PathBuf;

pub use error::DocError;

#[derive(Debug, Clone)]
pub struct DocOptions {
    pub package_path: PathBuf,
    pub out_dir: Option<PathBuf>,
    pub only_exported: bool,
}

#[derive(Debug, Clone)]
pub struct GeneratedDocs {
    pub out_dir: PathBuf,
    pub index_file: PathBuf,
}

pub fn generate_docs(options: DocOptions) -> Result<GeneratedDocs, DocError> {
    let loaded = load::load_site(&options.package_path, options.only_exported)?;
    let out_dir = options.out_dir.unwrap_or(loaded.default_out_dir);
    let index_file = render::render_site(&loaded.model, &out_dir)?;
    Ok(GeneratedDocs {
        out_dir,
        index_file,
    })
}
