use std::collections::BTreeMap;
use std::path::PathBuf;

use par_core::frontend::Type;
use par_core::frontend::language::{GlobalName, PackageId, Universal};
use par_core::workspace::ModulePath;

#[derive(Debug, Clone)]
pub struct LoadedSite {
    pub model: SiteModel,
    pub default_out_dir: PathBuf,
}

#[derive(Debug, Clone)]
pub struct SiteModel {
    pub packages: BTreeMap<PackageId, PackageModel>,
    pub sections: SiteSections,
}

impl SiteModel {
    pub fn package(&self, id: &PackageId) -> Option<&PackageModel> {
        self.packages.get(id)
    }
}

#[derive(Debug, Clone, Default)]
pub struct SiteSections {
    pub root: Vec<PackageId>,
    pub built_in: Vec<PackageId>,
    pub direct_dependencies: Vec<PackageId>,
    pub indirect_dependencies: Vec<PackageId>,
}

#[derive(Debug, Clone)]
pub struct PackageModel {
    pub id: PackageId,
    pub name: String,
    pub identity: String,
    pub kind: PackageKind,
    pub modules: Vec<ModuleModel>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PackageKind {
    Root,
    BuiltIn,
    DirectDependency,
    IndirectDependency,
}

#[derive(Debug, Clone)]
pub struct ModuleModel {
    pub universal: Universal,
    pub path: ModulePath,
    pub doc_markdown: Option<String>,
    pub is_exported: bool,
    pub items: Vec<ItemModel>,
}

impl ModuleModel {
    pub fn key(&self) -> ModuleKey {
        ModuleKey {
            package: self.universal.package.clone(),
            path: self.path.clone(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ModuleKey {
    pub package: PackageId,
    pub path: ModulePath,
}

#[derive(Debug, Clone)]
pub struct ItemModel {
    pub kind: ItemKind,
    pub name: GlobalName<Universal>,
    pub params: Vec<String>,
    pub typ: Type<Universal>,
    pub doc_markdown: Option<String>,
    pub is_public: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ItemKind {
    Type,
    Declaration,
}
