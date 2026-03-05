use crate::flat::runtime::ExternalFn;
use std::collections::HashMap;
use std::sync::LazyLock;

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub struct DefinitionRef<'a> {
    pub package: &'a str,
    pub path: &'a [&'a str],
    pub module: &'a str,
    pub name: &'a str,
}
pub struct ExternalDef {
    pub path: DefinitionRef<'static>,
    pub f: ExternalFn,
}

inventory::collect!(ExternalDef);

static REGISTRY: LazyLock<HashMap<&'static str, HashMap<DefinitionRef, ExternalFn>>> =
    LazyLock::new(|| {
        let mut map = HashMap::new();
        for def in inventory::iter::<ExternalDef> {
            if !map.contains_key(def.path.package) {
                map.insert(def.path.package, HashMap::new());
            }
            map.get_mut(def.path.package)
                .unwrap()
                .insert(def.path.clone(), def.f);
        }
        map
    });

pub fn get_external_defs(package: &str) -> Vec<ExternalDef> {
    if let Some(map) = REGISTRY.get(package) {
        map.iter()
            .map(|(path, f)| ExternalDef {
                path: path.clone(),
                f: *f,
            })
            .collect()
    } else {
        Vec::new()
    }
}
