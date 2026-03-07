use crate::flat::arena::{Arena, Index, Indexable};
use crate::flat::runtime::{
    ExternalFn, Global, GlobalCont, GlobalValue, Package, PackageBody, PackagePtr,
};
use crate::registry::{DefinitionRef, PackageRef, get_external_fn};
use std::sync::OnceLock;

pub type Linked = ExternalFn;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum PackageID {
    Local,
    Package(String),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Unlinked {
    pub package: PackageID,
    pub path: Vec<String>,
    pub module: String,
    pub name: String,
}

impl<'a> From<PackageRef<'a>> for PackageID {
    fn from(package: PackageRef) -> Self {
        match package {
            PackageRef::Local => PackageID::Local,
            PackageRef::Package(name) => PackageID::Package(name.to_string()),
        }
    }
}

impl<'a> From<DefinitionRef<'a>> for Unlinked {
    fn from(value: DefinitionRef<'a>) -> Self {
        Unlinked {
            package: value.package.into(),
            path: value.path.iter().map(|s| s.to_string()).collect(),
            module: value.module.to_string(),
            name: value.name.to_string(),
        }
    }
}

pub fn link_arena(arena: &Arena<Unlinked>) -> Arena<Linked> {
    Arena {
        nodes: arena.nodes.iter().map(link_global).collect(),
        strings: arena.strings.clone(),
        string_to_location: arena
            .string_to_location
            .iter()
            .map(|(k, v)| (k.clone(), Index(v.0.clone())))
            .collect(),
        case_branches: arena
            .case_branches
            .iter()
            .map(|(index, package)| (Index(index.0.clone()), link_package_body(package)))
            .collect(),
        packages: arena
            .packages
            .iter()
            .map(|package| link_package(package))
            .collect(),
        redexes: arena
            .redexes
            .iter()
            .map(|(a, b)| (Index(a.0.clone()), Index(b.0.clone())))
            .collect(),
    }
}

fn link_package(package: &OnceLock<Package<Unlinked>>) -> OnceLock<Package<Linked>> {
    let package = package.get().unwrap();
    let lock = OnceLock::new();
    lock.set(Package {
        body: link_package_body(&package.body),
        num_vars: package.num_vars,
    })
    .unwrap();
    lock
}

fn link_package_body(body: &PackageBody<Unlinked>) -> PackageBody<Linked> {
    PackageBody {
        root: Index(body.root.0.clone()),
        captures: Index(body.captures.0.clone()),
        debug_name: body.debug_name.clone(),
        redexes: Index(body.redexes.0.clone()),
    }
}

fn link_global(node: &Global<Unlinked>) -> Global<Linked> {
    match node {
        Global::Variable(id) => Global::Variable(id.clone()),
        Global::Package(package_ptr, global_ptr, fab_behavior) => Global::Package(
            Index(package_ptr.0.clone()),
            Index(global_ptr.0.clone()),
            fab_behavior.clone(),
        ),
        Global::Destruct(cont) => Global::Destruct(link_global_cont(cont)),
        Global::Value(value) => Global::Value(link_global_value(value)),
        Global::Fanout(fanout) => Global::Fanout(Index(fanout.0.clone())),
    }
}

fn link_global_value(p0: &GlobalValue<Unlinked>) -> GlobalValue<Linked> {
    match p0 {
        GlobalValue::Break => GlobalValue::Break,
        GlobalValue::Pair(a, b) => GlobalValue::Pair(Index(a.0.clone()), Index(b.0.clone())),
        GlobalValue::Either(s, v) => GlobalValue::Either(Index(s.0.clone()), Index(v.0.clone())),
        GlobalValue::ExternalFn(unlinked) => {
            //TODO: make it an error
            GlobalValue::ExternalFn(
                get_external_fn(unlinked).expect("missing external {unlinked:?}"),
            )
        }
        GlobalValue::ExternalArc(e) => GlobalValue::ExternalArc(e.clone()),
        GlobalValue::Primitive(p) => GlobalValue::Primitive(p.clone()),
    }
}

fn link_global_cont(cont: &GlobalCont<Unlinked>) -> GlobalCont<Linked> {
    match cont {
        GlobalCont::Continue => GlobalCont::Continue,
        GlobalCont::Par(a, b) => GlobalCont::Par(Index(a.0.clone()), Index(b.0.clone())),
        GlobalCont::Choice(a, b) => GlobalCont::Choice(Index(a.0.clone()), Index(b.0.clone())),
    }
}

pub fn link_package_ptr(package: &PackagePtr<Unlinked>) -> PackagePtr<Linked> {
    Index(package.0.clone())
}
