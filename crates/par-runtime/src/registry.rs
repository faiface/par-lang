use crate::flat::runtime::ExternalFn;
use crate::linker::{Linked, PackageID, Unlinked};
use std::collections::HashMap;
use std::sync::LazyLock;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum PackageRef<'a> {
    Local,
    Package(&'a str),
}
#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub struct DefinitionRef<'a> {
    pub package: PackageRef<'a>,
    pub path: &'a [&'a str],
    pub module: &'a str,
    pub name: &'a str,
}
pub struct ExternalDef {
    pub path: DefinitionRef<'static>,
    pub f: ExternalFn,
}

inventory::collect!(ExternalDef);

static REGISTRY: LazyLock<HashMap<PackageID, HashMap<Unlinked, Linked>>> = LazyLock::new(|| {
    let mut map: HashMap<PackageID, HashMap<Unlinked, Linked>> = HashMap::new();
    for def in inventory::iter::<ExternalDef> {
        let package = def.path.package.clone().into();
        if !map.contains_key(&package) {
            map.insert(package.clone(), HashMap::new());
        }
        map.get_mut(&package)
            .unwrap()
            .insert(def.path.clone().into(), def.f);
    }
    map
});

pub fn get_external_fn(path: &Unlinked) -> Option<ExternalFn> {
    if let Some(map) = REGISTRY.get(&path.package) {
        map.get(path).copied()
    } else {
        None
    }
}
