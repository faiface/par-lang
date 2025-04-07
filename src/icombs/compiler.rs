use std::{
    fmt::{Debug, Display},
    sync::Arc,
};

use crate::{
    par::{parse::Loc, types::TypeDefs},
    playground::CheckedProgram,
};
use indexmap::{IndexMap, IndexSet};
use std::hash::Hash;

use super::net::{Net, Tree};
use crate::par::parse::Loc::External;
use crate::par::{
    process::{Captures, Command, Expression, Process},
    types::Type,
};

use super::Name;

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

#[derive(Clone, Debug)]
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
    UnguardedLoop(Loc, Option<Name>),
}

impl Error {
    pub fn display(&self, code: &str) -> String {
        match self {
            Error::UnboundVar(loc) => format!("Unbound variable\n{}", loc.display(code)),
            Error::UnusedVar(loc) => format!("Unused variable\n{}", loc.display(code)),
            Error::UnexpectedType(loc, ty) => {
                format!("Unexpected type: {:?}\n{}", ty, loc.display(code),)
            }
            Error::GlobalNotFound(name) => format!("Global not found: {:?}", name),
            Error::DependencyCycle { global, dependents } => format!(
                "Dependency cycle detected for global {:?} with dependents {:?}",
                global, dependents
            ),
            Error::UnguardedLoop(loc, name) => format!(
                "Unguarded loop with label {:?} at\n{}",
                name,
                loc.display(code)
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

impl From<Name> for Var {
    fn from(value: Name) -> Self {
        Var::Name(value)
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct LoopLabel(Option<Name>);

pub struct Context {
    vars: IndexMap<Var, (TypedTree, VariableKind)>,
    loop_points: IndexMap<LoopLabel, Vec<LoopLabel>>,
    unguarded_loop_labels: Vec<LoopLabel>,
}

pub struct PackData {
    names: Vec<Var>,
    types: Vec<Type<Loc, Name>>,
    kinds: Vec<VariableKind>,
    loop_points: IndexMap<LoopLabel, Vec<LoopLabel>>,
    unguarded_loop_labels: Vec<LoopLabel>,
}

impl Context {
    pub fn pack(
        &mut self,
        captures: Option<&Captures<Loc, Name>>,
        labels_in_scope: Option<&Vec<LoopLabel>>,
        net: &mut Net,
    ) -> (Tree, PackData) {
        let mut m_trees = vec![];
        let mut m_tys = vec![];
        let mut m_vars = vec![];
        let mut m_kind = vec![];
        for (name, (tree, kind)) in core::mem::take(&mut self.vars) {
            if let Some(captures) = captures {
                if let Var::Name(name) = &name {
                    if !captures.names.contains_key(name) {
                        net.link(tree.tree, Tree::e());
                        continue;
                    }
                }
            }
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
                unguarded_loop_labels: core::mem::take(&mut self.unguarded_loop_labels),
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
            self.bind_variable_with_kind(name.clone(), v0.with_type(ty.clone()), kind.clone());
            m_trees.push(v1);
        }
        self.loop_points = packed.loop_points.clone();
        self.unguarded_loop_labels = packed.unguarded_loop_labels.clone();
        let context_out = multiplex_trees(m_trees);
        context_out
    }

    fn bind_variable_with_kind(&mut self, var: Var, tree: TypedTree, kind: VariableKind) {
        assert_ne!(kind, VariableKind::Global);
        match self.vars.insert(var.clone(), (tree, kind)) {
            Some(x) => panic!("{:?}", x),
            None => (),
        }
    }
}

pub struct Compiler {
    net: Net,
    context: Context,
    type_defs: TypeDefs<Loc, Name>,
    definitions: IndexMap<Name, (Loc, Arc<Expression<Loc, Name, Type<Loc, Name>>>)>,
    global_name_to_id: IndexMap<Name, usize>,
    id_to_ty: Vec<Type<Loc, Name>>,
    id_to_package: Vec<Net>,
    lazy_redexes: Vec<(Tree, Tree)>,
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

impl Compiler {
    fn get_kind(&self, tree: &TypedTree) -> VariableKind {
        match tree.ty.is_linear(&self.type_defs).unwrap() {
            true => VariableKind::Linear,
            false => VariableKind::Replicable,
        }
    }

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
        let global = match self.definitions.get(name).cloned() {
            Some((_, expr)) => expr,
            _ => return Err(Error::GlobalNotFound(name.clone())),
        };
        /*let debug = false;
        if debug {
            println!("{global:#?}");
        }*/

        let (id, typ) = self.in_package(|this, _| {
            let mut s = String::new();
            global.pretty(&mut s, 0).unwrap();
            this.compile_expression(global.as_ref())
        })?;
        self.global_name_to_id.insert(name.clone(), id);
        self.compile_global_stack.shift_remove(name);
        Ok(Tree::Package(id).with_type(typ))
    }

    fn in_package(
        &mut self,
        f: impl FnOnce(&mut Self, usize) -> Result<TypedTree>,
        //debug: bool,
    ) -> Result<(usize, Type<Loc, Name>)> {
        let id = self.id_to_package.len();
        let old_net = core::mem::take(&mut self.net);
        // Allocate package
        self.id_to_ty.push(Type::Break(Loc::External));
        self.id_to_package.push(Default::default());
        let tree = self.with_captures(&Captures::default(), |this| f(this, id))?;
        self.net.ports.push_back(tree.tree);

        self.net.packages = Arc::new(self.id_to_package.clone().into_iter().enumerate().collect());
        /*if debug {
            println!("{}", self.net.show());
        }*/
        self.net.assert_valid_with(
            self.lazy_redexes
                .iter()
                .map(|(a, b)| [a, b].into_iter())
                .flatten(),
        );
        self.net.normal();
        self.net
            .redexes
            .append(&mut core::mem::take(&mut self.lazy_redexes).into());
        self.net.assert_valid();
        /*if debug {
            println!("{}", self.net.show());
        }*/
        *self.id_to_ty.get_mut(id).unwrap() = tree.ty.clone();
        *self.id_to_package.get_mut(id).unwrap() = core::mem::take(&mut self.net);
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
            let (tree, kind) = self.use_name(name, false)?;
            if kind != VariableKind::Global {
                vars.insert(Var::Name(name.clone()), (tree, kind));
            }
        }
        for (label, _) in self.context.loop_points.clone().iter() {
            let (tree, kind) = self.use_var(&Var::Loop(label.0.clone()), false)?;
            vars.insert(Var::Loop(label.0.clone()), (tree, kind));
        }
        let loop_points_before = self.context.loop_points.clone();
        core::mem::swap(&mut vars, &mut self.context.vars);
        let t = f(self);
        self.context.vars = vars;
        self.context.loop_points = loop_points_before;
        t
    }

    fn bind_variable(&mut self, var: impl Into<Var>, tree: TypedTree) -> Result<()> {
        let kind = self.get_kind(&tree);
        let prev = self.context.vars.insert(var.into(), (tree, kind));
        match prev {
            Some((prev_tree, prev_kind)) => {
                if prev_kind == VariableKind::Linear {
                    return Err(Error::UnusedVar(Loc::default()));
                }
                self.net.link(prev_tree.tree, Tree::e());
                Ok(())
            }
            None => Ok(()),
        }
    }

    fn instantiate_name(&mut self, name: &Name, in_command: bool) -> Result<TypedTree> {
        let (value, kind) = self.use_name(name, in_command)?;
        if kind == VariableKind::Boxed {
            todo!()
        }
        Ok(value)
    }

    fn use_var(&mut self, var: &Var, in_command: bool) -> Result<(TypedTree, VariableKind)> {
        if let Some((tree, kind)) = self.context.vars.swap_remove(var) {
            if in_command {
                return Ok((tree, kind));
            }
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

    fn use_name(&mut self, name: &Name, in_command: bool) -> Result<(TypedTree, VariableKind)> {
        if self.context.vars.contains_key(&Var::Name(name.clone())) {
            return self.use_var(&Var::Name(name.clone()), in_command);
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
                ty: t.dual(&self.type_defs).unwrap(),
            },
        )
    }

    fn show_state(&mut self) {
        println!("Variables:");
        for (name, (value, _)) in &self.context.vars {
            println!(
                "    {}: {} = {:?}",
                if let Var::Name(name) = name {
                    format!("{}", name)
                } else {
                    format!("{:?}", name)
                },
                {
                    let mut s = String::new();
                    value.ty.pretty(&mut s, 4);
                    s
                },
                self.net.show_tree(&value.tree)
            )
        }
        println!("Net:");
        println!("{}", self.net.show_indent(1));
        println!("{}", self.net.ports.len())
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
                if self.type_defs.vars.contains(&name) {
                    return Type::Name(loc, name, args);
                } else {
                    let ty = self.type_defs.get(&loc, &name, &args).unwrap();
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
                Type::expand_recursive(&asc, &name, &body, &self.type_defs).unwrap(),
            ),
            Type::Iterative(_, asc, name, body) => self.normalize_type(
                Type::expand_iterative(&asc, &name, &body, &self.type_defs).unwrap(),
            ),
            Type::Chan(_, body) => {
                let dual = body.dual(&self.type_defs).unwrap();
                if matches!(dual, Type::Chan(..)) {
                    dual
                } else {
                    self.normalize_type(dual)
                }
            }
            a => a,
        }
    }

    fn compile_expression(
        &mut self,
        expr: &Expression<Loc, Name, Type<Loc, Name>>,
    ) -> Result<TypedTree> {
        match expr {
            Expression::Reference(_, name, _) => self.instantiate_name(name, false),
            Expression::Fork(_, captures, name, _, typ, proc) => {
                self.with_captures(captures, |this| {
                    let (v0, v1) = this.create_typed_wire(typ.clone());
                    this.bind_variable(name.clone(), v0)?;
                    this.compile_process(proc)?;
                    Ok(v1)
                })
            }
        }
    }

    fn compile_process(&mut self, proc: &Process<Loc, Name, Type<Loc, Name>>) -> Result<()> {
        /*let debug = false;
        if debug {
            let mut s = String::new();
            proc.pretty(&mut s, 0).unwrap();
            println!("{s}");
            self.show_state();
        }*/
        match proc {
            Process::Let(_, name, _, _, expr, rest) => {
                let value = self.compile_expression(expr)?;
                self.bind_variable(name.clone(), value)?;
                self.compile_process(rest)
            }

            Process::Do(loc, name, target_ty, command) => {
                self.compile_command(loc, name.clone(), target_ty.clone(), command)
            }

            Process::Telltypes(_, _) => unreachable!(),
        }
    }

    fn compile_command(
        &mut self,
        loc: &Loc,
        name: Name,
        ty: Type<Loc, Name>,
        cmd: &Command<Loc, Name, Type<Loc, Name>>,
    ) -> Result<()> {
        //println!("{:?}", loc);
        match cmd {
            Command::Link(expr) => {
                let subject = self.instantiate_name(&name, true)?;
                let value = self.compile_expression(expr)?;
                self.link_typed(subject, value);
                self.end_context()?;
            }
            // types get erased.
            Command::SendType(argument, process) => {
                let subject = self.instantiate_name(&name, true)?;
                let Type::ReceiveType(_, type_name, ret_type) =
                    self.normalize_type(subject.ty.clone())
                else {
                    panic!("Unexpected type for SendType: {:?}", subject.ty);
                };
                let ret_type = ret_type.substitute(&type_name, argument).unwrap();
                self.bind_variable(name, subject.tree.with_type(ret_type))?;
                self.compile_process(process)?;
            }
            Command::ReceiveType(parameter, process) => {
                let subject = self.instantiate_name(&name, true)?;
                let Type::SendType(_, type_name, ret_type) =
                    self.normalize_type(subject.ty.clone())
                else {
                    panic!("Unexpected type for ReceiveType: {:?}", subject.ty);
                };
                let ret_type = ret_type
                    .clone()
                    .substitute(&type_name, &Type::Var(loc.clone(), parameter.clone()))
                    .unwrap();
                let was_empty_before = self.type_defs.vars.insert(parameter.clone());
                self.bind_variable(name, subject.tree.with_type(ret_type))?;
                self.compile_process(process)?;
                if was_empty_before {
                    self.type_defs.vars.shift_remove(parameter);
                }
            }
            Command::Send(expr, process) => {
                // < name(expr) process >
                // ==
                // name = free
                // free = (name < expr >)
                // < process >
                let subject = self.instantiate_name(&name, true)?;
                let Type::Receive(_, _, ret_type) = self.normalize_type(subject.ty.clone()) else {
                    panic!("Unexpected type for Receive: {:?}", subject.ty);
                };
                let expr = self.compile_expression(expr)?;
                let (v0, v1) = self.create_typed_wire(*ret_type);
                self.bind_variable(name, v0)?;
                self.net.link(Tree::c(v1.tree, expr.tree), subject.tree);
                self.compile_process(process)?;
            }
            Command::Receive(target, _, process) => {
                // < name[target] process >
                // ==
                // name = free
                // free = (name target)
                // < process >
                let subject = self.instantiate_name(&name, true)?;
                let Type::Send(_, arg_type, ret_type) = self.normalize_type(subject.ty.clone())
                else {
                    panic!("Unexpected type for Receive: {:?}", subject.ty);
                };
                let (v0, v1) = self.create_typed_wire(*arg_type);
                let (w0, w1) = self.create_typed_wire(*ret_type);
                self.bind_variable(name, w0)?;
                self.bind_variable(target.clone(), v0)?;
                self.net.link(Tree::c(w1.tree, v1.tree), subject.tree);
                self.compile_process(process)?;
            }
            Command::Choose(chosen, process) => {
                let subject = self.instantiate_name(&name, true)?;
                let Type::Choice(_, branches) = self.normalize_type(subject.ty.clone()) else {
                    panic!("Unexpected type for Choose: {:?}", subject.ty);
                };
                let Some(branch_type) = branches.get(chosen) else {
                    unreachable!()
                };
                let branch_index = branches.get_index_of(chosen).unwrap();
                let (v0, v1) = self.create_typed_wire(branch_type.clone());
                let choosing_tree = self.either_instance(v1.tree, branch_index, branches.len());
                self.net.link(choosing_tree, subject.tree);
                self.bind_variable(name, v0)?;
                self.compile_process(process)?;
            }
            Command::Match(names, processes) => {
                self.context.unguarded_loop_labels.clear();
                let old_tree = self.instantiate_name(&name, true)?;
                // Multiplex all other variables in the context.
                let (context_in, pack_data) = self.context.pack(None, None, &mut self.net);

                let mut branches = vec![];
                let Type::Either(_, required_branches) = self.normalize_type(ty.clone()) else {
                    panic!("Unexpected type for Match: {:?}", ty);
                };
                let mut choice_and_process: Vec<_> = names.iter().zip(processes.iter()).collect();
                choice_and_process.sort_by_key(|k| k.0);

                for ((_, process), branch) in choice_and_process
                    .into_iter()
                    .zip(required_branches.values())
                {
                    let (w0, w1) = self.create_typed_wire(branch.clone());
                    self.bind_variable(name.clone(), w0)?;

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
                let a = self.instantiate_name(&name, true)?.tree;
                self.net.link(a, Tree::e());
                self.end_context()?;
            }
            Command::Continue(process) => {
                // < name ? process >
                // ==
                // name = *
                // < process >
                let a = self.instantiate_name(&name, true)?.tree;
                self.net.link(a, Tree::e());
                self.compile_process(process)?;
            }
            Command::Begin(_, label, captures, process) => {
                let label = LoopLabel(label.clone());
                self.context.vars.sort_keys();

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

                let mut labels_in_scope: Vec<_> =
                    self.context.loop_points.keys().cloned().collect();
                labels_in_scope.push(label.clone());
                self.context
                    .loop_points
                    .insert(label.clone(), labels_in_scope);

                self.context.unguarded_loop_labels.push(label.clone());

                let (context_in, pack_data) =
                    self.context.pack(Some(captures), None, &mut self.net);
                let (id, _) = self.in_package(
                    |this, _| {
                        let context_out = this.context.unpack(&pack_data, &mut this.net);
                        this.compile_process(process)?;
                        Ok(context_out.with_type(Type::Break(External)))
                    },
                    //true,
                )?;
                self.net.link(def1, Tree::Package(id));
                self.net.link(context_in, Tree::Package(id));
            }
            Command::Loop(label, captures) => {
                let label = LoopLabel(label.clone());
                if self.context.unguarded_loop_labels.contains(&label) {
                    return Err(Error::UnguardedLoop(loc.clone(), label.clone().0));
                }
                let (tree, _) = self.use_var(&Var::Loop(label.0.clone()), false)?;
                let labels_in_scope = self.context.loop_points.get(&label).unwrap().clone();
                self.context.vars.sort_keys();
                let (context_in, _) =
                    self.context
                        .pack(Some(captures), Some(&labels_in_scope), &mut self.net);
                self.lazy_redexes.push((tree.tree, context_in));
            }
        };
        Ok(())
    }

    fn end_context(&mut self) -> Result<()> {
        // drop all replicables
        for (_, (value, kind)) in core::mem::take(&mut self.context.vars).into_iter() {
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
            unguarded_loop_labels: Default::default(),
        },
        type_defs: program.type_defs.clone(),
        definitions: program.definitions.clone(),
        global_name_to_id: Default::default(),
        id_to_package: Default::default(),
        id_to_ty: Default::default(),
        compile_global_stack: Default::default(),
        lazy_redexes: vec![],
    };

    for name in compiler.definitions.keys().cloned().collect::<Vec<_>>() {
        compiler.compile_global(&name)?;
    }

    Ok(IcCompiled {
        id_to_package: Arc::new(compiler.id_to_package.into_iter().enumerate().collect()),
        name_to_id: compiler.global_name_to_id,
        id_to_ty: compiler.id_to_ty.into_iter().enumerate().collect(),
    })
}

#[derive(Clone, Default)]
pub struct IcCompiled {
    pub(crate) id_to_package: Arc<IndexMap<usize, Net>>,
    pub(crate) name_to_id: IndexMap<Name, usize>,
    pub(crate) id_to_ty: IndexMap<usize, Type<Loc, Name>>,
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
    pub fn get_with_name(&self, name: &Name) -> Option<Net> {
        let id = self.name_to_id.get(name)?;
        self.id_to_package.get(id).cloned()
    }

    pub fn get_type_of(&self, name: &Name) -> Option<Type<Loc, Name>> {
        let id = self.name_to_id.get(name)?;
        self.id_to_ty.get(id).cloned()
    }

    pub fn create_net(&self) -> Net {
        let mut net = Net::default();
        net.packages = self.id_to_package.clone();
        net
    }
}
