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
    parse::Program,
    process::{Captures, Command, Expression, Process},
    types::Type,
};

use super::Name;

type Prog = Program<Loc, Name, Arc<Expression<Loc, Name, Type<Loc, Name>>>>;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
enum VariableKind {
    // Can only be used once.
    Linear,
    // Replicable, but needs no dereliction
    Replicable,
    // Replicable, and needs dereliction
    Boxed,
    // Global, should not be stored in the context.
    Global,
}

#[derive(Clone)]
pub enum Error {
    /// Error that is emitted when a variable that was never bound/captured is used
    UnboundVar(Loc),
    /// Error that is emitted when a linear variable is not used
    UnusedVar(Loc),
    UnexpectedType(Loc, Type<Loc, Name>),
    GlobalNotFound(Name),
    DependencyCycle {
        global: Name,
        dependents: IndexSet<Name>,
    },
}

impl Debug for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::UnboundVar(loc) => write!(f, "Unbound variable at {:?}", loc),
            Error::UnusedVar(loc) => write!(f, "Unused variable at {:?}", loc),
            Error::UnexpectedType(loc, ty) => write!(f, "Unexpected type at {:?}: {:?}", loc, ty),
            Error::GlobalNotFound(name) => write!(f, "Global not found: {:?}", name),
            Error::DependencyCycle { global, dependents } => write!(
                f,
                "Dependency cycle detected for global {:?} with dependents {:?}",
                global, dependents
            ),
        }
    }
}

type Result<T> = std::result::Result<T, Error>;

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

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct LoopLabel(Option<Name>);

pub struct Context {
    vars: IndexMap<Var, (TypedTree, VariableKind)>,
    loop_points: IndexMap<LoopLabel, Vec<LoopLabel>>,
}

pub struct PackData {
    names: Vec<Var>,
    types: Vec<Type<Loc, Name>>,
    kinds: Vec<VariableKind>,
    loop_points: IndexMap<LoopLabel, Vec<LoopLabel>>,
}

impl Context {
    pub fn pack(
        &mut self,
        labels_in_scope: Option<&Vec<LoopLabel>>,
        net: &mut Net,
    ) -> (Tree, PackData) {
        let mut m_trees = vec![];
        let mut m_tys = vec![];
        let mut m_vars = vec![];
        let mut m_kind = vec![];
        for (name, (tree, kind)) in core::mem::take(&mut self.vars) {
            if let Some(labels_in_scope) = labels_in_scope {
                if let Var::Loop(label) = &name {
                    if !labels_in_scope.contains(&LoopLabel(label.clone())) {
                        net.link(tree.tree, Tree::e());
                        continue;
                    }
                }
            }
            m_vars.push(name);
            m_trees.push(tree.tree);
            m_tys.push(tree.ty);
            m_kind.push(kind);
        }
        let context_in = multiplex_trees(m_trees);
        (
            context_in,
            PackData {
                names: m_vars,
                types: m_tys,
                kinds: m_kind,
                loop_points: core::mem::take(&mut self.loop_points),
            },
        )
    }

    pub fn unpack(&mut self, packed: &PackData, net: &mut Net) -> Tree {
        let mut m_trees = vec![];
        for (name, (ty, kind)) in packed
            .names
            .iter()
            .zip(packed.types.iter().zip(packed.kinds.iter()))
        {
            let (v0, v1) = net.create_wire();
            self.bind_variable_with_kind(&name, v0.with_type(ty.clone()), kind.clone());
            m_trees.push(v1);
        }
        self.loop_points = packed.loop_points.clone();
        let context_out = multiplex_trees(m_trees);
        context_out
    }

    fn bind_variable_with_kind(&mut self, var: &Var, tree: TypedTree, kind: VariableKind) {
        assert_ne!(kind, VariableKind::Global);
        assert!(self.vars.insert(var.clone(), (tree, kind)).is_none())
    }
}

pub struct Compiler<'program> {
    net: Net,
    context: Context,
    program: &'program CheckedProgram,
    global_name_to_id: IndexMap<Name, usize>,
    type_variables: IndexSet<Name>,
    id_to_ty: Vec<Type<Loc, Name>>,
    id_to_package: Vec<Tree>,
    compile_global_stack: IndexSet<Name>,
}

impl Tree {
    pub(crate) fn with_type(self, ty: Type<Loc, Name>) -> TypedTree {
        TypedTree { tree: self, ty }
    }
}

pub(crate) fn multiplex_trees(mut trees: Vec<Tree>) -> Tree {
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
    fn compile_global(&mut self, name: &Name) -> Result<TypedTree> {
        if let Some(id) = self.global_name_to_id.get(name) {
            let ty = self.id_to_ty.get(*id).unwrap().clone();
            return Ok(TypedTree {
                tree: Tree::Package(*id),
                ty,
            });
        };
        if !self.compile_global_stack.insert(name.clone()) {
            return Err(Error::DependencyCycle {
                global: name.clone(),
                dependents: self.compile_global_stack.clone(),
            });
        }
        let global = self.program.definitions.get(name);
        let global = match global {
            Some(a) => &a.1,
            _ => return Err(Error::GlobalNotFound(name.clone())),
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
            debug,
        )?;
        self.global_name_to_id.insert(name.clone(), id);
        self.compile_global_stack.shift_remove(name);
        Ok(Tree::Package(id).with_type(typ))
    }

    fn in_package(
        &mut self,
        f: impl FnOnce(&mut Self) -> Result<TypedTree>,
        debug: bool,
    ) -> Result<(usize, Type<Loc, Name>)> {
        let old_net = core::mem::take(&mut self.net);
        let tree = self.with_captures(&Captures::default(), |this| f(this))?;
        self.net.ports.push_back(tree.tree);

        let id = self.id_to_package.len();
        self.net.packages = Arc::new(self.id_to_package.clone().into_iter().enumerate().collect());
        if debug {
            println!("{}", self.net.show());
        }
        self.net.assert_valid();
        self.net.normal();
        self.net.assert_valid();
        if debug {
            println!("{}", self.net.show());
        }
        let package_contents = self.net.ports.pop_back().unwrap();
        self.id_to_ty.push(tree.ty.clone());
        self.id_to_package.push(package_contents);
        self.net = old_net;

        Ok((id, tree.ty))
    }

    fn with_captures<T>(
        &mut self,
        captures: &Captures<Loc, Name>,
        f: impl FnOnce(&mut Self) -> Result<T>,
    ) -> Result<T> {
        let mut vars = IndexMap::new();
        for (name, _) in captures.names.iter() {
            let (tree, kind) = self.use_name(name)?;
            if kind != VariableKind::Global {
                vars.insert(Var::Name(name.clone()), (tree, kind));
            }
        }
        for (label, _) in self.context.loop_points.clone().iter() {
            let (tree, kind) = self.use_var(&Var::Loop(label.0.clone()))?;
            vars.insert(Var::Loop(label.0.clone()), (tree, kind));
        }
        let mut loop_points_before = self.context.loop_points.clone();
        core::mem::swap(&mut vars, &mut self.context.vars);
        let t = f(self);
        core::mem::swap(&mut vars, &mut self.context.vars);
        core::mem::swap(&mut loop_points_before, &mut self.context.loop_points);
        t
    }
    fn bind_variable(&mut self, name: &Name, tree: TypedTree) -> Result<()> {
        if self
            .context
            .vars
            .insert(Var::Name(name.clone()), (tree, VariableKind::Linear))
            .is_some()
        {
            return Err(Error::UnusedVar(Loc::default()));
        } else {
            Ok(())
        }
    }
    fn instantiate_variable(&mut self, name: &Name) -> Result<TypedTree> {
        let (value, kind) = self.use_name(name)?;
        if kind == VariableKind::Boxed {
            todo!()
        }
        Ok(value)
    }

    fn use_var(&mut self, var: &Var) -> Result<(TypedTree, VariableKind)> {
        if let Some((tree, kind)) = self.context.vars.swap_remove(var) {
            match kind {
                VariableKind::Linear => Ok((tree, kind)),
                kind => {
                    let (w0, w1) = self.net.create_wire();
                    let (v0, v1) = self.net.create_wire();
                    self.net.link(Tree::d(v0, w0), tree.tree);
                    self.context.vars.insert(
                        var.clone(),
                        (
                            TypedTree {
                                tree: w1,
                                ty: tree.ty.clone(),
                            },
                            kind,
                        ),
                    );
                    Ok((
                        TypedTree {
                            tree: v1,
                            ty: tree.ty.clone(),
                        },
                        kind,
                    ))
                }
            }
        } else {
            Err(Error::UnboundVar(Default::default()))
        }
    }

    fn use_name(&mut self, name: &Name) -> Result<(TypedTree, VariableKind)> {
        if self.context.vars.contains_key(&Var::Name(name.clone())) {
            return self.use_var(&Var::Name(name.clone()));
        }
        match self.compile_global(name) {
            Ok(value) => Ok((value, VariableKind::Global)),
            Err(Error::GlobalNotFound(_)) => Err(Error::UnboundVar(Default::default())),
            Err(e) => Err(e),
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
        for (name, (value, kind)) in &self.context.vars {
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
            Type::Recursive(_, asc, name, body) => self.normalize_type(
                Type::expand_recursive(&asc, &name, &body, &self.program.type_defs).unwrap(),
            ),
            Type::Iterative(_, asc, name, body) => self.normalize_type(
                Type::expand_iterative(&asc, &name, &body, &self.program.type_defs).unwrap(),
            ),
            a => a,
        }
    }

    fn compile_expression(
        &mut self,
        expr: &Expression<Loc, Name, Type<Loc, Name>>,
    ) -> Result<TypedTree> {
        match expr {
            Expression::Reference(_, name, ty) => self.instantiate_variable(name),
            Expression::Fork(_, captures, name, _, typ, proc) => {
                self.with_captures(captures, |this| {
                    let (v0, v1) = this.create_typed_wire(typ.clone());
                    this.bind_variable(name, v0)?;
                    this.compile_process(proc)?;
                    Ok(v1)
                })
            }
        }
    }
    fn compile_process(&mut self, proc: &Process<Loc, Name, Type<Loc, Name>>) -> Result<()> {
        match proc {
            Process::Let(_, key, _, _, value, rest) => {
                let value = self.compile_expression(value)?;
                self.context
                    .vars
                    .insert(Var::Name(key.clone()), (value, VariableKind::Linear));
                self.compile_process(rest)
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
    ) -> Result<()> {
        match cmd {
            Command::Link(a) => {
                let a = self.compile_expression(a)?;
                let b = self.instantiate_variable(&name)?;
                self.link_typed(a, b);
                self.end_context()?;
            }
            // types get erased.
            Command::SendType(target_type, process) => {
                let a = self.instantiate_variable(&name)?;
                let Type::ReceiveType(_, src_name, ret_type) = self.normalize_type(a.ty.clone())
                else {
                    panic!("Unexpected type for SendType: {:?}", a.ty);
                };
                let ret_type = ret_type.substitute(&src_name, target_type).unwrap();
                self.bind_variable(&name, a.tree.with_type(ret_type))?;
                self.compile_process(process)?;
            }
            Command::ReceiveType(_, process) => {
                let a = self.instantiate_variable(&name)?;
                let Type::SendType(_, dest_name, ret_type) = self.normalize_type(a.ty.clone())
                else {
                    panic!("Unexpected type for ReceiveType: {:?}", a.ty);
                };
                self.bind_variable(&name, a.tree.with_type(*ret_type))?;

                let was_empty_before = self.type_variables.insert(dest_name.clone());
                self.compile_process(process)?;
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
                let a = self.instantiate_variable(&name)?;
                let Type::Receive(_, arg_type, ret_type) = self.normalize_type(a.ty.clone()) else {
                    panic!("Unexpected type for Receive: {:?}", a.ty);
                };
                let expr = self.compile_expression(expr)?;

                let (v0, v1) = self.create_typed_wire(*ret_type);
                self.bind_variable(&name, v0)?;
                //
                self.net.link(Tree::c(v1.tree, expr.tree), a.tree);
                self.compile_process(process)?;
            }
            Command::Receive(target, _, process) => {
                // < name[target] process >
                // ==
                // name = free
                // free = (name target)
                // < process >
                let a = self.instantiate_variable(&name)?;
                let Type::Send(_, arg_type, ret_type) = self.normalize_type(a.ty.clone()) else {
                    panic!("Unexpected type for Receive: {:?}", a.ty);
                };
                let (v0, v1) = self.create_typed_wire(*arg_type);
                let (w0, w1) = self.create_typed_wire(*ret_type);
                self.bind_variable(&name, w0)?;
                self.bind_variable(&target, v0)?;
                //
                self.net.link(Tree::c(w1.tree, v1.tree), a.tree);
                self.compile_process(process)?;
            }
            Command::Choose(chosen, process) => {
                let a = self.instantiate_variable(&name)?;

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
                self.bind_variable(&name, v0)?;
                self.compile_process(process)?;
            }
            Command::Match(names, processes) => {
                let old_tree = self.instantiate_variable(&name)?;
                // Multiplex all other variables in the context.
                let (context_in, pack_data) = self.context.pack(None, &mut self.net);

                let mut branches = vec![];
                let Type::Either(_, required_branches) = self.normalize_type(ty.clone()) else {
                    panic!("Unexpected type for Match: {:?}", ty);
                };
                let mut choice_and_process: Vec<_> = names.iter().zip(processes.iter()).collect();
                choice_and_process.sort_by_key(|k| k.0);

                let loop_points = self.context.loop_points.clone();

                for ((choice_here, process), branch) in choice_and_process
                    .into_iter()
                    .zip(required_branches.values())
                {
                    let (w0, w1) = self.create_typed_wire(branch.clone());
                    self.bind_variable(&name, w0)?;

                    let context_out = self.context.unpack(&pack_data, &mut self.net);
                    self.compile_process(process)?;
                    branches.push((context_out, w1.tree))
                }
                let t = self.choice_instance(context_in, branches);

                self.net.link(old_tree.tree, t);
            }
            Command::Break => {
                // < name ! >
                // ==
                // name = *
                let a = self.instantiate_variable(&name)?.tree;
                self.net.link(a, Tree::e());
                self.end_context()?;
            }
            Command::Continue(process) => {
                // < name ? process >
                // ==
                // name = *
                // < process >
                let a = self.instantiate_variable(&name)?.tree;
                self.net.link(a, Tree::e());
                self.compile_process(process)?;
            }
            Command::Begin(_, label, process) => {
                let label = LoopLabel(label.clone());
                let (def0, def1) = self.net.create_wire();
                let prev = self.context.vars.insert(
                    Var::Loop(label.0.clone()),
                    (
                        def0.with_type(Type::Break(External)),
                        VariableKind::Replicable,
                    ),
                );
                if let Some((prev_tree, _)) = prev {
                    self.net.link(prev_tree.tree, Tree::Era);
                }
                self.context.vars.sort_keys();
                let mut labels_in_scope: Vec<_> =
                    self.context.loop_points.keys().cloned().collect();
                labels_in_scope.push(label.clone());
                self.context
                    .loop_points
                    .insert(label.clone(), labels_in_scope);

                let (context_in, pack_data) = self.context.pack(None, &mut self.net);
                let (id, _) = self.in_package(
                    |this| {
                        let context_out = this.context.unpack(&pack_data, &mut this.net);
                        this.compile_process(process)?;
                        Ok(context_out.with_type(Type::Break(External)))
                    },
                    true,
                )?;
                self.net.link(def1, Tree::Package(id));
                self.net.link(context_in, Tree::Package(id));
            }
            Command::Loop(label) => {
                let label = LoopLabel(label.clone());
                let (tree, _) = self.use_var(&Var::Loop(label.0.clone()))?;
                let labels_in_scope = self.context.loop_points.get(&label).unwrap().clone();
                self.context.vars.sort_keys();
                let (context_in, pack_data) =
                    self.context.pack(Some(&labels_in_scope), &mut self.net);
                self.net.link(tree.tree, context_in);
            }
        };
        Ok(())
    }

    fn end_context(&mut self) -> Result<()> {
        // drop all replicables
        for (name, (value, kind)) in core::mem::take(&mut self.context.vars).into_iter() {
            if kind == VariableKind::Linear {
                return Err(Error::UnusedVar(Default::default()));
            } else {
                self.net.link(value.tree, Tree::e());
            }
        }
        self.context.loop_points = Default::default();
        Ok(())
    }
}

pub fn compile_file(program: &CheckedProgram) -> Result<IcCompiled> {
    let mut compiler = Compiler {
        net: Net::default(),
        context: Context {
            vars: IndexMap::default(),
            loop_points: IndexMap::default(),
        },
        global_name_to_id: Default::default(),
        id_to_package: Default::default(),
        id_to_ty: Default::default(),
        type_variables: Default::default(),
        compile_global_stack: Default::default(),
        program,
    };
    for k in compiler.program.definitions.clone().keys() {
        compiler.compile_global(k)?;
    }
    Ok(IcCompiled {
        id_to_package: Arc::new(compiler.id_to_package.into_iter().enumerate().collect()),
        name_to_id: compiler.global_name_to_id,
    })
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
