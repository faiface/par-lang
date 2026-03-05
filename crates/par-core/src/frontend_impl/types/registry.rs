use crate::frontend::Type;
use crate::frontend::language::Unresolved;
use par_runtime::registry::DefinitionRef;
use std::collections::HashMap;
use std::sync::LazyLock;
pub struct ExternalTypeDef {
    pub path: DefinitionRef<'static>,
    pub typ: Type<Unresolved>,
}

inventory::collect!(ExternalTypeDef);

static REGISTRY: LazyLock<HashMap<&'static str, HashMap<DefinitionRef, Type<Unresolved>>>> =
    LazyLock::new(|| {
        let mut map = HashMap::new();
        for def in inventory::iter::<ExternalTypeDef> {
            if !map.contains_key(def.path.package) {
                map.insert(def.path.package, HashMap::new());
            }
            map.get_mut(def.path.package)
                .unwrap()
                .insert(def.path.clone(), def.typ.clone());
        }
        map
    });

pub fn get_external_type_defs(package: &str) -> Vec<ExternalTypeDef> {
    if let Some(map) = REGISTRY.get(package) {
        map.iter()
            .map(|(path, typ)| ExternalTypeDef {
                path: path.clone(),
                typ: typ.clone(),
            })
            .collect()
    } else {
        Vec::new()
    }
}
