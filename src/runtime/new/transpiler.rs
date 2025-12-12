use super::runtime::Runtime;
use std::collections::HashMap;
use std::fmt::Display;
use std::sync::Arc;

use super::readback::Handle;
use super::reducer::NetHandle;
use crate::par::types::TypeDefs;

use std::sync::OnceLock;

use crate::par::language::GlobalName;
use crate::runtime::new::arena::{Arena, Index};
use crate::runtime::new::reducer::Reducer;
use crate::runtime::new::runtime::{ExternalArc, GlobalCont, GlobalValue, Package, PackagePtr};
use crate::runtime::{new, old};
use old::compiler::IcCompiled;

use crate::runtime::old::Net;
use new::runtime::Global;

#[derive(Default)]
pub struct NetTranspiler {
    source: old::net::Net,
    dest: Arena,
    num_vars: usize,
    variable_map: HashMap<usize, usize>,
    package_map: HashMap<usize, PackagePtr>,
}

#[derive(Clone)]
pub struct Transpiled {
    pub arena: Arc<Arena>,
    pub name_to_package: HashMap<GlobalName, PackagePtr>,
    pub type_defs: TypeDefs,
}

impl Display for Transpiled {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.arena)
    }
}

impl Transpiled {
    pub fn transpile(ic_compiled: IcCompiled, type_defs: TypeDefs) -> Self {
        let mut this: NetTranspiler = Default::default();
        for i in ic_compiled.id_to_package.keys() {
            let slot = this.dest.alloc(OnceLock::new());
            this.package_map.insert(*i, slot);
        }
        for (id, body) in ic_compiled.id_to_package.as_ref().clone().drain(..) {
            this.transpile_package(id, body)
        }
        Self {
            arena: Arc::new(this.dest),
            type_defs,
            name_to_package: ic_compiled
                .name_to_id
                .iter()
                .map(|(a, b)| (a.clone(), this.package_map.get(b).unwrap().clone()))
                .collect(),
        }
    }

    pub fn compile_file(
        module: &crate::par::program::CheckedModule,
    ) -> Result<Self, crate::runtime::RuntimeCompilerError> {
        let type_defs = module.type_defs.clone();
        let ic_compiled = crate::runtime::old::compiler::IcCompiled::compile_file(module)?;
        let transpiled = Self::transpile(ic_compiled, type_defs);
        Ok(transpiled)
    }

    pub fn get_with_name(&self, name: &GlobalName) -> Option<PackagePtr> {
        Some(self.name_to_package.get(name).cloned()?)
    }


    pub fn new_runtime(&self) -> Runtime {
        Runtime::from(self.arena.clone())
    }
    pub fn new_reducer(&self) -> Reducer {
        Reducer::from(Runtime::from(self.arena.clone()))
    }
    pub async fn instantiate(&self, handle: NetHandle, name: &GlobalName) -> Option<Handle> {
        let package = self.get_with_name(name)?;
        Handle::from_package(self.arena.clone(), handle, package)
            .await
            .ok()
    }
}

impl NetTranspiler {
    fn transpile_tree_and_alloc(&mut self, source: old::net::Tree) -> Index<Global> {
        let tree = self.transpile_tree(source);
        self.dest.alloc(tree)
    }
    fn map_variable(&mut self, id: usize) -> usize {
        match self.variable_map.entry(id) {
            std::collections::hash_map::Entry::Occupied(occupied_entry) => occupied_entry.remove(),
            std::collections::hash_map::Entry::Vacant(vacant_entry) => {
                let id = self.num_vars;
                vacant_entry.insert(id);
                self.num_vars += 1;
                id
            }
        }
    }
    fn transpile_package(&mut self, id: usize, mut body: Net) {
        // First, allocate the package
        assert!(body.ports.len() == 2);
        let captures = body.ports.pop_back().unwrap();
        let root = body.ports.pop_back().unwrap();
        let mut redexes = Vec::from(body.redexes.clone());
        redexes.append(&mut body.waiting_for_reducer.clone());
        let debug_name = body.debug_name.clone();
        self.source = body;
        self.variable_map.clear();
        self.num_vars = 0;
        let root = self.transpile_tree(root);
        let captures = self.transpile_tree(captures);
        let redexes: Vec<_> = redexes
            .into_iter()
            .map(|(a, b)| (self.transpile_tree(a), self.transpile_tree(b)))
            .collect();
        let package = Package {
            root: root,
            captures: captures,
            num_vars: self.num_vars,
            debug_name,
            redexes,
        };
        let package_slot = self.package_map.get(&id).unwrap();
        self.dest.get(package_slot.clone()).set(package).unwrap();
    }
    fn map_package(&mut self, id: usize) -> PackagePtr {
        self.package_map.get(&id).unwrap().clone()
    }
    fn transpile_tree(&mut self, source: old::net::Tree) -> Global {
        use old::net::Tree;
        match source {
            Tree::Var(id) => match self.source.variables.remove_linked(id) {
                Ok(contents) => self.transpile_tree(contents),
                Err(_e) => {
                    // this is a "real" aux aux link.
                    Global::Variable(self.map_variable(id))
                }
            },
            Tree::Break => Global::Value(GlobalValue::Break),
            Tree::Continue => Global::Destruct(GlobalCont::Continue),
            Tree::Era => Global::Fanout(self.dest.alloc_clone(&[])),
            Tree::Par(a, b) => {
                let a: Index<Global> = self.transpile_tree_and_alloc(*a);
                let b: Index<Global> = self.transpile_tree_and_alloc(*b);
                Global::Destruct(GlobalCont::Par(a, b))
            }
            Tree::Times(a, b) => {
                let a: Index<Global> = self.transpile_tree_and_alloc(*a);
                let b: Index<Global> = self.transpile_tree_and_alloc(*b);
                Global::Value(GlobalValue::Pair(a, b))
            }
            Tree::Dup(a, b) => {
                let s = [self.transpile_tree(*a), self.transpile_tree(*b)];
                Global::Fanout(self.dest.alloc_clone(&s))
            }
            Tree::Signal(arc_str, tree) => Global::Value(GlobalValue::Either(
                arc_str.clone(),
                self.transpile_tree_and_alloc(*tree),
            )),
            Tree::Choice(tree, hash_map, els) => {
                let tree = self.transpile_tree_and_alloc(*tree);
                Global::Destruct(GlobalCont::Choice(
                    tree,
                    Arc::new(
                        hash_map
                            .iter()
                            .map(|(k, v)| (k.clone(), self.package_map.get(v).unwrap().clone()))
                            .collect(),
                    ),
                    els.map(|x| self.package_map.get(&x).unwrap().clone()),
                ))
            }
            Tree::Package(id, context, behavior) => Global::Package(
                self.map_package(id),
                self.transpile_tree_and_alloc(*context),
                behavior,
            ),
            Tree::SignalRequest(_sender) => todo!(),
            Tree::Primitive(primitive) => Global::Value(GlobalValue::Primitive(primitive)),
            Tree::IntRequest(_sender) => todo!(),
            Tree::StringRequest(_sender) => todo!(),
            Tree::BytesRequest(_sender) => todo!(),
            Tree::External(e) => Global::Value(GlobalValue::ExternalFn(e)),
            Tree::ExternalBox(e) => Global::Value(GlobalValue::ExternalArc(ExternalArc(e))),
        }
    }
}
