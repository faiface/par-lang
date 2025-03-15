use std::{
    fmt::{Debug, Display},
    sync::Arc,
};

use crate::{par::parse::Loc, playground::CheckedProgram};
use indexmap::{IndexMap, IndexSet};
use std::hash::Hash;

use super::net::{Net, Tree};
use crate::par::parse::Loc::External;
use crate::par::{
    language::Internal,
    parse::Program,
    process::{Captures, Command, Expression, Process},
    types::Type,
};

use super::Name;

type Prog = Program<Name, Arc<Expression<Loc, Name, Type<Loc, Name>>>>;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
enum VariableKind {
    // Can only be used once.
    Linear,
    // Replicable, but needs no dereliction
    Replicable,
    // Replicable, and needs dereliction
    Boxed,
}

#[derive(Debug, Clone)]
pub struct TypedTree {
    pub tree: Tree,
    pub ty: Type<Loc, Name>,
}

impl Default for TypedTree {
    fn default() -> Self {
        Self {
            tree: Tree::e(),
            ty: Type::Break(Loc::default()),
        }
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Var {
    Name(Name),
    Loop(Option<Name>),
}

pub struct Compiler<'program> {
    net: Net,
    vars: IndexMap<Var, (TypedTree, VariableKind)>,
    program: &'program CheckedProgram,
    global_name_to_id: IndexMap<Name, usize>,
    type_variables: IndexSet<Name>,
    id_to_ty: Vec<Type<Loc, Name>>,
    id_to_package: Vec<Tree>,
}

impl Tree {
    pub(crate) fn with_type(self, ty: Type<Loc, Name>) -> TypedTree {
        TypedTree { tree: self, ty }
    }
}

fn multiplex_trees(mut trees: Vec<Tree>) -> Tree {
    if trees.len() == 0 {
        Tree::e()
    } else if trees.len() == 1 {
        trees.pop().unwrap()
    } else {
        let new_trees = trees.split_off(trees.len() / 2);
        Tree::c(multiplex_trees(trees), multiplex_trees(new_trees))
    }
}

impl<'program> Compiler<'program> {
    fn compile_global(&mut self, name: &Name) -> Option<TypedTree> {
        if let Some(id) = self.global_name_to_id.get(name) {
            let ty = self.id_to_ty.get(*id).unwrap().clone();
            return Some(TypedTree {
                tree: Tree::Package(*id),
                ty,
            });
        };
        let global = self.program.definitions.get(name)?;
        let typ = match self.program.declarations.get(name) {
            Some(Some(typ)) => Some(typ),
            _ => None,
        };
        let debug = false;
        if debug {
            println!("{global:#?}");
        }

        let (id, typ) = self.in_package(
            |this| {
                let mut s = String::new();
                global.pretty(&mut s, 0).unwrap();
                this.compile_expression(global.as_ref())
            },
            typ,
            debug,
        );
        self.global_name_to_id.insert(name.clone(), id);

        Some(Tree::Package(id).with_type(typ))
    }

    fn in_package(
        &mut self,
        f: impl FnOnce(&mut Self) -> TypedTree,
        typ: Option<&Type<Loc, Name>>,
        debug: bool,
    ) -> (usize, Type<Loc, Name>) {
        let old_net = core::mem::take(&mut self.net);
        let tree = self.with_captures(&Captures::default(), |this| f(this));
        self.net.ports.push_back(tree.tree);

        let id = self.id_to_package.len();
        self.net.packages = Arc::new(self.id_to_package.clone().into_iter().enumerate().collect());
        if debug {
            println!("{}", self.net.show());
        }
        self.net.normal();
        if debug {
            println!("{}", self.net.show());
        }
        let package_contents = self.net.ports.pop_back().unwrap();
        self.id_to_ty.push(tree.ty.clone());
        self.id_to_package.push(package_contents);
        self.net = old_net;

        (id, tree.ty)
    }

    fn with_captures<T>(
        &mut self,
        captures: &Captures<Loc, Name>,
        f: impl FnOnce(&mut Self) -> T,
    ) -> T {
        let mut vars = IndexMap::new();
        for (name, _) in captures.names.iter() {
            let (tree, kind) = self.use_name(name);
            vars.insert(Var::Name(name.clone()), (tree, kind));
        }
        let var_keys: Vec<_> = self.vars.keys().cloned().collect();
        for var in var_keys {
            if let Var::Loop(label) = var {
                let (tree, kind) = self.use_var(&Var::Loop(label.clone()));
                vars.insert(Var::Loop(label.clone()), (tree, kind));
            }
        }
        core::mem::swap(&mut vars, &mut self.vars);
        let t = f(self);
        core::mem::swap(&mut vars, &mut self.vars);
        // drop all replicables
        for (name, (value, kind)) in vars.into_iter() {
            if kind == VariableKind::Linear {
                panic!("some variables were not closed: {:?}", name);
            } else {
                self.net.link(value.tree, Tree::e());
            }
        }
        t
    }
    fn bind_variable(&mut self, name: &Name, tree: TypedTree) {
        assert!(self
            .vars
            .insert(Var::Name(name.clone()), (tree, VariableKind::Linear))
            .is_none())
    }
    fn instantiate_variable(&mut self, name: &Name) -> TypedTree {
        let (value, kind) = self.use_name(name);
        if kind == VariableKind::Boxed {
            todo!()
        }
        value
    }

    fn use_var(&mut self, var: &Var) -> (TypedTree, VariableKind) {
        if let Some((tree, kind)) = self.vars.swap_remove(var) {
            match kind {
                VariableKind::Linear => (tree, kind),
                kind => {
                    let (w0, w1) = self.net.create_wire();
                    let (v0, v1) = self.net.create_wire();
                    self.net.link(Tree::d(v0, w0), tree.tree);
                    self.vars.insert(
                        var.clone(),
                        (
                            TypedTree {
                                tree: w1,
                                ty: tree.ty.clone(),
                            },
                            kind,
                        ),
                    );
                    (
                        TypedTree {
                            tree: v1,
                            ty: tree.ty.clone(),
                        },
                        kind,
                    )
                }
            }
        } else {
            panic!("unknown variable {:?}", var)
        }
    }

    fn use_name(&mut self, name: &Name) -> (TypedTree, VariableKind) {
        if self.vars.contains_key(&Var::Name(name.clone())) {
            self.use_var(&Var::Name(name.clone()))
        } else if self.compile_global(name).is_some() {
            let value = self.compile_global(name).unwrap();
            let v = (value.clone(), VariableKind::Replicable);
            self.vars.insert(Var::Name(name.clone()), v.clone());
            v
        } else {
            panic!("unknown variable {}", name)
        }
    }
    fn create_typed_wire(&mut self, t: Type<Loc, Name>) -> (TypedTree, TypedTree) {
        let (v0, v1) = self.net.create_wire();
        (
            TypedTree {
                tree: v0,
                ty: t.clone(),
            },
            TypedTree {
                tree: v1,
                ty: t.dual(&self.program.type_defs).unwrap(),
            },
        )
    }
    fn show_state(&mut self) {
        println!("Variables:");
        for (name, (value, kind)) in &self.vars {
            println!(
                "    {:?}: {:?} = {:?}",
                name,
                value.ty,
                self.net.show_tree(&value.tree)
            )
        }
        println!("Net:");
        println!("{}", self.net.show_indent(1));
    }
    fn link_typed(&mut self, a: TypedTree, b: TypedTree) {
        self.net.link(a.tree, b.tree);
    }
    fn either_instance(&mut self, tree: Tree, index: usize, out_of: usize) -> Tree {
        let (w0, w1) = self.net.create_wire();
        let mut trees: Vec<_> = std::iter::repeat(Tree::e()).take(out_of).collect();
        *trees.get_mut(index).unwrap() = Tree::c(w1, tree);
        Tree::c(w0, multiplex_trees(trees))
    }
    /// cases is a list of (context, payload).
    fn choice_instance(&mut self, ctx_out: Tree, cases: Vec<(Tree, Tree)>) -> Tree {
        Tree::c(
            ctx_out,
            multiplex_trees(cases.into_iter().map(|(a, b)| Tree::c(a, b)).collect()),
        )
    }
    fn normalize_type(&mut self, ty: Type<Loc, Name>) -> Type<Loc, Name> {
        match ty {
            Type::Name(loc, name, args) => {
                if self.type_variables.contains(&name) {
                    return Type::Name(loc, name, args);
                } else {
                    let ty = self.program.type_defs.get(&loc, &name, &args).unwrap();
                    self.normalize_type(ty)
                }
            }
            Type::Either(loc, mut index_map) => {
                index_map.sort_keys();
                Type::Either(loc, index_map)
            }
            Type::Choice(loc, mut index_map) => {
                index_map.sort_keys();
                Type::Choice(loc, index_map)
            }
            Type::Recursive(_, name, body) => self.normalize_type(
                Type::expand_recursive(&name, &body, &self.program.type_defs).unwrap(),
            ),
            Type::Iterative(_, name, body) => self.normalize_type(
                Type::expand_iterative(&name, &body, &self.program.type_defs).unwrap(),
            ),
            a => a,
        }
    }

    fn compile_expression(&mut self, expr: &Expression<Loc, Name, Type<Loc, Name>>) -> TypedTree {
        match expr {
            Expression::Reference(_, name, ty) => self.instantiate_variable(name),
            Expression::Fork(_, captures, name, _, typ, proc) => {
                self.with_captures(captures, |this| {
                    let (v0, v1) = this.create_typed_wire(typ.clone());
                    this.bind_variable(name, v0);
                    this.compile_process(proc);
                    v1
                })
            }
        }
    }
    fn compile_process(&mut self, proc: &Process<Loc, Name, Type<Loc, Name>>) {
        match proc {
            Process::Let(_, key, _, _, value, rest) => {
                let value = self.compile_expression(value);
                self.vars
                    .insert(Var::Name(key.clone()), (value, VariableKind::Linear));
                self.compile_process(rest);
            }

            Process::Do(_, name, target_ty, command) => {
                self.compile_command(name.clone(), target_ty.clone(), command)
            }
            _ => todo!(),
        }
    }
    fn compile_command(
        &mut self,
        name: Name,
        ty: Type<Loc, Name>,
        cmd: &Command<Loc, Name, Type<Loc, Name>>,
    ) {
        match cmd {
            Command::Link(a) => {
                let a = self.compile_expression(a);
                let b = self.instantiate_variable(&name);
                self.link_typed(a, b);
            }
            // types get erased.
            Command::SendType(target_type, process) => {
                let a = self.instantiate_variable(&name);
                let Type::ReceiveType(_, src_name, ret_type) = self.normalize_type(a.ty.clone())
                else {
                    panic!("Unexpected type for SendType: {:?}", a.ty);
                };
                let ret_type = ret_type.substitute(&src_name, target_type).unwrap();
                self.bind_variable(&name, a.tree.with_type(ret_type));
                self.compile_process(process);
            }
            Command::ReceiveType(_, process) => {
                let a = self.instantiate_variable(&name);
                let Type::SendType(_, dest_name, ret_type) = self.normalize_type(a.ty.clone())
                else {
                    panic!("Unexpected type for ReceiveType: {:?}", a.ty);
                };
                self.bind_variable(&name, a.tree.with_type(*ret_type));

                let was_empty_before = self.type_variables.insert(dest_name.clone());
                self.compile_process(process);
                if was_empty_before {
                    self.type_variables.swap_remove(&dest_name);
                }
            }
            Command::Send(expr, process) => {
                // < name(expr) process >
                // ==
                // name = free
                // free = (name < expr >)
                // < process >
                let a = self.instantiate_variable(&name);
                let Type::Receive(_, arg_type, ret_type) = self.normalize_type(a.ty.clone()) else {
                    panic!("Unexpected type for Receive: {:?}", a.ty);
                };
                let expr = self.compile_expression(expr);

                let (v0, v1) = self.create_typed_wire(*ret_type);
                self.bind_variable(&name, v0);
                //
                self.net.link(Tree::c(v1.tree, expr.tree), a.tree);
                self.compile_process(process);
            }
            Command::Receive(target, _, process) => {
                // < name[target] process >
                // ==
                // name = free
                // free = (name target)
                // < process >
                let a = self.instantiate_variable(&name);
                let Type::Send(_, arg_type, ret_type) = self.normalize_type(a.ty.clone()) else {
                    panic!("Unexpected type for Receive: {:?}", a.ty);
                };
                let (v0, v1) = self.create_typed_wire(*arg_type);
                let (w0, w1) = self.create_typed_wire(*ret_type);
                self.bind_variable(&name, w0);
                self.bind_variable(&target, v0);
                //
                self.net.link(Tree::c(w1.tree, v1.tree), a.tree);
                self.compile_process(process);
            }
            Command::Choose(chosen, process) => {
                let a = self.instantiate_variable(&name);

                let Type::Choice(_, branches) = self.normalize_type(a.ty.clone()) else {
                    panic!("Unexpected type for Choose: {:?}", a.ty);
                };
                let Some(branch_type) = branches.get(chosen) else {
                    unreachable!()
                };
                let branch_index = branches.get_index_of(chosen).unwrap();
                let (v0, v1) = self.create_typed_wire(branch_type.clone());
                let choosing_tree = self.either_instance(v1.tree, branch_index, branches.len());
                self.net.link(choosing_tree, a.tree);
                self.bind_variable(&name, v0);
                self.compile_process(process);
            }
            Command::Match(names, processes) => {
                let old_tree = self.instantiate_variable(&name);
                // Multiplex all other variables in the context.
                let mut m_trees = vec![];
                let mut m_tys = vec![];
                let mut m_vars = vec![];
                let mut m_kind = vec![];
                for (k, (v, kind)) in core::mem::take(&mut self.vars) {
                    m_vars.push(k);
                    m_trees.push(v.tree);
                    m_tys.push(v.ty);
                    m_kind.push(kind);
                }
                let context_in = multiplex_trees(m_trees);

                let mut branches = vec![];
                let Type::Either(_, required_branches) = self.normalize_type(ty.clone()) else {
                    panic!("Unexpected type for Match: {:?}", ty);
                };
                let mut choice_and_process: Vec<_> = names.iter().zip(processes.iter()).collect();
                choice_and_process.sort_by_key(|k| k.0);

                for ((choice_here, process), branch) in choice_and_process
                    .into_iter()
                    .zip(required_branches.values())
                {
                    let (w0, w1) = self.create_typed_wire(branch.clone());
                    self.bind_variable(&name, w0);

                    // multiplex the conetxt frmo the inside now
                    let mut m_trees = vec![];
                    for (name, (ty, kind)) in m_vars.iter().zip(m_tys.iter().zip(m_kind.iter())) {
                        let (v0, v1) = self.net.create_wire();
                        self.vars
                            .insert(name.clone(), (v0.with_type(ty.clone()), kind.clone()));

                        m_trees.push(v1);
                    }
                    let context_out = multiplex_trees(m_trees);

                    self.compile_process(process);
                    // drop all replicables
                    for (name, (value, kind)) in core::mem::take(&mut self.vars).into_iter() {
                        if kind == VariableKind::Linear {
                            panic!("some variables were not closed: {:?}", name);
                        } else {
                            self.net.link(value.tree, Tree::e());
                        }
                    }
                    branches.push((context_out, w1.tree))
                }
                let t = self.choice_instance(context_in, branches);

                self.net.link(old_tree.tree, t);
            }
            Command::Break => {
                // < name ! >
                // ==
                // name = *
                let a = self.instantiate_variable(&name).tree;
                self.net.link(a, Tree::e());
            }
            Command::Continue(process) => {
                // < name ? process >
                // ==
                // name = *
                // < process >
                let a = self.instantiate_variable(&name).tree;
                self.net.link(a, Tree::e());
                self.compile_process(process);
            }
            Command::Begin(label, process) => {
                let (def0, def1) = self.net.create_wire();
                let prev = self.vars.insert(
                    Var::Loop(label.clone()),
                    (
                        def0.with_type(Type::Break(External)),
                        VariableKind::Replicable,
                    ),
                );
                if let Some((prev_tree, _)) = prev {
                    self.net.link(prev_tree.tree, Tree::Era);
                }
                self.vars.sort_keys();
                let mut m_trees = vec![];
                let mut m_tys = vec![];
                let mut m_vars = vec![];
                let mut m_kind = vec![];
                for (k, (v, kind)) in core::mem::take(&mut self.vars) {
                    m_vars.push(k);
                    m_trees.push(v.tree);
                    m_tys.push(v.ty);
                    m_kind.push(kind);
                }
                let context_in = multiplex_trees(m_trees);

                let (id, _) = self.in_package(
                    |this| {
                        let mut m_trees = vec![];
                        for (name, (ty, kind)) in m_vars.iter().zip(m_tys.iter().zip(m_kind.iter()))
                        {
                            let (v0, v1) = this.net.create_wire();
                            this.vars
                                .insert(name.clone(), (v0.with_type(ty.clone()), kind.clone()));
                            m_trees.push(v1);
                        }
                        this.compile_process(process);
                        multiplex_trees(m_trees).with_type(Type::Break(External))
                    },
                    None,
                    true,
                );
                self.net.link(def1, Tree::Package(id));
                self.net.link(context_in, Tree::Package(id));
            }
            Command::Loop(label) => {
                let (tree, _) = self.use_var(&Var::Loop(label.clone()));
                self.vars.sort_keys();
                let mut m_trees = vec![];
                let mut m_tys = vec![];
                let mut m_vars = vec![];
                let mut m_kind = vec![];
                for (k, (v, kind)) in core::mem::take(&mut self.vars) {
                    m_vars.push(k);
                    m_trees.push(v.tree);
                    m_tys.push(v.ty);
                    m_kind.push(kind);
                }
                let context_in = multiplex_trees(m_trees);
                self.net.link(tree.tree, context_in);
            }
        }
    }
}

pub fn compile_file(program: &CheckedProgram) -> IcCompiled {
    let mut compiler = Compiler {
        net: Net::default(),
        vars: IndexMap::default(),
        global_name_to_id: Default::default(),
        id_to_package: Default::default(),
        id_to_ty: Default::default(),
        type_variables: Default::default(),
        program,
    };
    for k in compiler.program.definitions.clone().keys() {
        compiler.compile_global(k);
    }
    IcCompiled {
        id_to_package: Arc::new(compiler.id_to_package.into_iter().enumerate().collect()),
        name_to_id: compiler.global_name_to_id,
    }
}

#[derive(Clone, Default)]
pub struct IcCompiled {
    pub(crate) id_to_package: Arc<IndexMap<usize, Tree>>,
    pub(crate) name_to_id: IndexMap<Name, usize>,
}

impl Display for IcCompiled {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (k, v) in self.id_to_package.iter() {
            // check if it has a name
            for (name, id) in self.name_to_id.iter() {
                if id == k {
                    f.write_fmt(format_args!("// {} \n", name))?;
                }
            }
            f.write_fmt(format_args!("@{} = {}\n", k, v.show()))?;
        }
        Ok(())
    }
}

impl IcCompiled {
    pub fn get_with_name(&self, name: &Name) -> Option<Tree> {
        let id = self.name_to_id.get(name)?;
        self.id_to_package.get(id).cloned()
    }
    pub fn create_net(&self) -> Net {
        let mut net = Net::default();
        net.packages = self.id_to_package.clone();
        net
    }
}
