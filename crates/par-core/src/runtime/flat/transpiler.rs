use super::runtime::Runtime;
use std::collections::HashMap;
use std::fmt::Display;
use std::sync::Arc;

use super::readback::Handle;
use super::reducer::NetHandle;
use crate::par::types::{visit, Type, TypeDefs, TypeError};

use std::sync::OnceLock;

use crate::par::language::GlobalName;
use crate::runtime_impl::flat::arena::{Arena, Index};
use crate::runtime_impl::flat::reducer::Reducer;
use crate::runtime_impl::flat::runtime::{
    ExternalArc, GlobalCont, GlobalValue, Package, PackageBody, PackagePtr,
};
use crate::runtime_impl::{flat, tree};
use arcstr::ArcStr;
use indexmap::IndexMap;
use tree::compiler::IcCompiled;

use crate::runtime_impl::tree::Net;
use flat::runtime::Global;

#[derive(Default)]
pub(crate) struct NetTranspiler {
    source: tree::net::Net,
    num_vars: usize,
    variable_map: HashMap<usize, usize>,
}

#[derive(Default)]
pub(crate) struct ProgramTranspiler {
    stack: Vec<NetTranspiler>,
    dest: Arena,
    packages_in_nodes: HashMap<usize, Index<OnceLock<Package>>>,
    id_to_package: Arc<IndexMap<usize, Net>>,
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
        let this: ProgramTranspiler = ProgramTranspiler::transpile_program(&ic_compiled);
        let mut arena = this.dest;
        let mut closure = |ty: &Type| {
            fn helper(ty: &Type, defs: &TypeDefs, arena: &mut Arena) -> Result<(), TypeError> {
                match ty {
                    Type::Either(_, variants) | Type::Choice(_, variants) => {
                        for k in variants.keys() {
                            arena.intern(k.string.as_str());
                        }
                    }
                    _ => {}
                }
                visit::continue_deref(&ty, defs, |ty: &Type| helper(ty, &defs, arena))
            }
            helper(ty, &type_defs, &mut arena)
        };
        for (_, _, ty) in type_defs.clone().globals.values() {
            closure(ty).unwrap();
        }

        Self {
            arena: Arc::new(arena),
            type_defs,
            name_to_package: ic_compiled
                .name_to_id
                .iter()
                .map(|(a, b)| (a.clone(), this.packages_in_nodes.get(b).unwrap().clone()))
                .collect(),
        }
    }

    pub fn compile_file(
        module: &crate::par::program::CheckedModule,
    ) -> Result<Self, crate::runtime_impl::RuntimeCompilerError> {
        let type_defs = module.type_defs.clone();
        let ic_compiled = crate::runtime_impl::tree::compiler::IcCompiled::compile_file(module)?;
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
    pub fn instantiate(&self, handle: NetHandle, name: &GlobalName) -> Option<Handle> {
        let package = self.get_with_name(name)?;
        Handle::from_package(self.arena.clone(), handle, package).ok()
    }
}

impl NetTranspiler {
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
}

impl ProgramTranspiler {
    fn transpile_tree_and_alloc(&mut self, source: tree::net::Tree) -> Index<Global> {
        let tree = self.transpile_tree(source);
        self.dest.alloc(tree)
    }
    fn current_net(&self) -> &NetTranspiler {
        self.stack.last().unwrap()
    }
    fn current_net_mut(&mut self) -> &mut NetTranspiler {
        self.stack.last_mut().unwrap()
    }
    fn transpile_package_body(&mut self) -> PackageBody {
        let body = &mut self.current_net_mut().source;
        assert!(body.ports.len() == 2);
        let captures = body.ports.pop_back().unwrap();
        let root = body.ports.pop_back().unwrap();
        let mut redexes = Vec::from(body.redexes.clone());
        redexes.append(&mut body.waiting_for_reducer.clone());
        let debug_name = body.debug_name.clone();

        let root = self.transpile_tree_and_alloc(root);
        let captures = self.transpile_tree_and_alloc(captures);
        let redexes: Vec<_> = redexes
            .into_iter()
            .map(|(a, b)| {
                (
                    self.transpile_tree_and_alloc(a),
                    self.transpile_tree_and_alloc(b),
                )
            })
            .collect();
        let redexes: Index<_> = self.dest.alloc_clone(redexes.as_ref());
        PackageBody {
            root: root,
            captures: captures,
            debug_name,
            redexes,
        }
    }
    fn transpile_package_go(&mut self, body: Net) -> Package {
        let mut current_net = NetTranspiler::default();
        current_net.source = body;
        self.stack.push(current_net);
        let body = self.transpile_package_body();
        let package = Package {
            body,
            num_vars: self.current_net().num_vars,
        };
        self.stack.pop();
        package
    }
    fn transpile_definition_package(&mut self, id: usize, body: Net) -> Index<OnceLock<Package>> {
        let package_index = self.dest.alloc(OnceLock::new());
        self.packages_in_nodes.insert(id, package_index.clone());
        let package = self.transpile_package_go(body);
        self.dest.get(package_index.clone()).set(package).unwrap();
        package_index
    }
    fn transpile_casebranch_package(&mut self, body: Net) -> (PackageBody, usize) {
        let mut current_net = NetTranspiler::default();
        current_net.source = body;
        current_net.num_vars = self.current_net().num_vars;
        self.stack.push(current_net);
        let body = self.transpile_package_body();
        let num_vars = self.stack.pop().unwrap().num_vars;
        (body, num_vars)
    }
    fn transpile_program(compiled: &IcCompiled) -> Self {
        let mut this = Self::default();
        this.id_to_package = compiled.id_to_package.clone();
        let keys: Vec<_> = this.id_to_package.keys().cloned().collect();
        for id in keys.into_iter() {
            if compiled.get_case_branch_name(id).is_none() {
                let package = this.id_to_package.get(&id).cloned().unwrap();
                this.transpile_definition_package(id, package);
            }
        }
        this
    }
    fn map_package(&mut self, id: usize) -> PackagePtr {
        if let Some(p) = self.packages_in_nodes.get(&id) {
            return p.clone();
        }
        let net = self.id_to_package.get(&id).unwrap().clone();
        self.transpile_definition_package(id, net)
    }
    fn transpile_tree(&mut self, source: tree::net::Tree) -> Global {
        use tree::net::Tree;
        match source {
            Tree::Var(id) => match self.current_net_mut().source.variables.remove_linked(id) {
                Ok(contents) => self.transpile_tree(contents),
                Err(_e) => {
                    // this is a "real" aux aux link.
                    Global::Variable(self.current_net_mut().map_variable(id))
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
                self.dest.intern(arc_str.as_str()),
                self.transpile_tree_and_alloc(*tree),
            )),
            Tree::Choice(captures, hash_map, els) => {
                let mut maximum_casebranch_length = 0;
                let mut table: Vec<_> = hash_map
                    .iter()
                    .map(|(k, v)| (k.clone(), v.clone()))
                    .chain(els.map(|id| (ArcStr::from(""), id)))
                    .map(|(signal, id)| {
                        let signal = self.dest.intern(signal.as_str());
                        let package = self.id_to_package.get(&id).unwrap().clone();
                        let (package, length) = self.transpile_casebranch_package(package);
                        maximum_casebranch_length = maximum_casebranch_length.max(length);
                        (signal, package)
                    })
                    .collect();
                self.current_net_mut().num_vars = maximum_casebranch_length;
                table.sort_by_key(|x| x.0 .0);
                let tree = self.transpile_tree_and_alloc(*captures);
                Global::Destruct(GlobalCont::Choice(
                    tree,
                    self.dest.alloc_clone(table.as_ref()),
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
