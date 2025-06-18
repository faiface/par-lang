use std::{
    collections::{BTreeMap, BTreeSet},
    fmt::{Debug, Display},
    sync::Arc,
};

use super::net::{Net, Tree};
use crate::par::{
    language::{GlobalName, LocalName},
    process::{Captures, Command, Expression, Process},
    types::Type,
};
use crate::{
    location::{Span, Spanning},
    par::{
        program::{CheckedModule, Definition},
        types::TypeDefs,
    },
};
use indexmap::{IndexMap, IndexSet};
use std::hash::Hash;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
enum VariableKind {
    // Can only be used once.
    Linear,
    // Replicable, but needs no dereliction
    Replicable,
}

#[derive(Clone, Debug)]
pub enum Error {
    /// Error that is emitted when a variable that was never bound/captured is used
    UnboundVar(Span, #[allow(unused)] Var),
    /// Error that is emitted when a linear variable is not used
    UnusedVar(Span),
    GlobalNotFound(GlobalName),
    DependencyCycle {
        global: GlobalName,
        dependents: IndexSet<GlobalName>,
    },
    UnguardedLoop(Span, #[allow(unused)] Option<LocalName>),
}

impl Error {
    pub fn display(&self, _code: &str) -> String {
        "inet compilation error".to_string()
        //TODO: fix error messages
        /*match self {
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
        }*/
    }

    pub fn spans(&self) -> (Span, Vec<Span>) {
        match self {
            Error::UnboundVar(span, _) | Error::UnusedVar(span) | Error::UnguardedLoop(span, _) => {
                (span.clone(), vec![])
            }

            Error::GlobalNotFound(name) => (name.span(), vec![]),

            Error::DependencyCycle { global, dependents } => (
                global.span(),
                dependents.iter().map(|name| name.span()).collect(),
            ),
        }
    }
}

type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, Clone)]
pub struct TypedTree {
    pub tree: Tree,
    pub ty: Type,
}

impl Default for TypedTree {
    fn default() -> Self {
        Self {
            tree: Tree::Era,
            ty: Type::Break(Span::default()),
        }
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Var {
    Name(LocalName),
    Loop(Option<LocalName>),
}

impl From<LocalName> for Var {
    fn from(value: LocalName) -> Self {
        Var::Name(value)
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct LoopLabel(Option<LocalName>);

#[derive(Debug)]
pub struct Context {
    vars: BTreeMap<Var, (TypedTree, VariableKind)>,
    loop_points: BTreeMap<LoopLabel, BTreeSet<LoopLabel>>,
    unguarded_loop_labels: Vec<LoopLabel>,
}

pub struct PackData {
    names: Vec<Var>,
    types: Vec<Type>,
    kinds: Vec<VariableKind>,
    loop_points: BTreeMap<LoopLabel, BTreeSet<LoopLabel>>,
    unguarded_loop_labels: Vec<LoopLabel>,
}

impl Context {
    pub fn pack(
        &mut self,
        driver: Option<&LocalName>,
        captures: Option<&Captures>,
        labels_in_scope: Option<&BTreeSet<LoopLabel>>,
        net: &mut Net,
    ) -> (Tree, PackData) {
        let mut m_trees = vec![];
        let mut m_tys = vec![];
        let mut m_vars = vec![];
        let mut m_kind = vec![];
        for (name, (tree, kind)) in core::mem::take(&mut self.vars) {
            if let Some(captures) = captures {
                if let Var::Name(name) = &name {
                    if !captures.names.contains_key(name) && Some(name) != driver {
                        net.link(tree.tree, Tree::Era);
                        continue;
                    }
                }
            }
            if let Some(labels_in_scope) = labels_in_scope {
                if let Var::Loop(label) = &name {
                    if !labels_in_scope.contains(&LoopLabel(label.clone())) {
                        net.link(tree.tree, Tree::Era);
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
        match self.vars.insert(var.clone(), (tree, kind)) {
            Some(x) => panic!("{:?}", x),
            None => (),
        }
    }
}

pub struct Compiler {
    net: Net,
    context: Context,
    type_defs: TypeDefs,
    definitions: IndexMap<GlobalName, Definition<Arc<Expression<Type>>>>,
    global_name_to_id: IndexMap<GlobalName, usize>,
    id_to_ty: Vec<Type>,
    id_to_package: Vec<Net>,
    lazy_redexes: Vec<(Tree, Tree)>,
    compile_global_stack: IndexSet<GlobalName>,
}

impl Tree {
    pub(crate) fn with_type(self, ty: Type) -> TypedTree {
        TypedTree { tree: self, ty }
    }
}

pub(crate) fn multiplex_trees(mut trees: Vec<Tree>) -> Tree {
    if trees.len() == 0 {
        Tree::Era
    } else if trees.len() == 1 {
        trees.pop().unwrap()
    } else {
        let new_trees = trees.split_off(trees.len() / 2);
        Tree::Con(
            Box::new(multiplex_trees(trees)),
            Box::new(multiplex_trees(new_trees)),
        )
    }
}

impl Compiler {
    fn get_kind(&self, tree: &TypedTree) -> VariableKind {
        match tree.ty.is_linear(&self.type_defs).unwrap() {
            true => VariableKind::Linear,
            false => VariableKind::Replicable,
        }
    }

    fn compile_global(&mut self, name: &GlobalName) -> Result<TypedTree> {
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
            Some(def) => def.expression,
            _ => return Err(Error::GlobalNotFound(name.clone())),
        };

        let (id, typ) = self.in_package(|this, _| {
            let mut s = String::new();
            global.pretty(&mut s, 0).unwrap();
            this.compile_expression(global.as_ref())
        })?;
        self.global_name_to_id.insert(name.clone(), id);
        self.compile_global_stack.shift_remove(name);
        Ok(Tree::Package(id).with_type(typ))
    }

    /// Optimize away erasure underneath auxiliary ports of dup and con nodes where it is safe to do so.
    ///
    /// Expects vars to be already have been substituted.
    fn apply_safe_rules(&mut self, tree: Tree) -> Tree {
        match tree {
            Tree::Dup(a, b) => {
                let a = self.apply_safe_rules(*a);
                let b = self.apply_safe_rules(*b);
                match (a, b) {
                    // This is unconditionally valid on the initial net because no "sup" nodes (dups with opposite polarity) are created in an initial net.
                    (Tree::Era, x) | (x, Tree::Era) => x,
                    (a, b) => Tree::Dup(Box::new(a), Box::new(b)),
                }
            }
            Tree::Con(a, b) => {
                let a = self.apply_safe_rules(*a);
                let b = self.apply_safe_rules(*b);
                match (a, b) {
                    (Tree::Era, Tree::Era) => {
                        // Eta reduction is always correct
                        Tree::Era
                    }
                    (a, b) => {
                        // TODO optimize `!` and `?`
                        Tree::Con(Box::new(a), Box::new(b))
                    }
                }
            }
            tree => tree,
        }
    }

    /// Reduces the tree in ways that aren't regular interactions. This might be invalid after the net has been reduced with regular interactions such as after calling [`Self::normal()`].
    fn non_principal_interactions(&mut self, mut tree: Tree) -> Tree {
        self.net.substitute_tree(&mut tree);
        self.apply_safe_rules(tree)
    }

    fn in_package(
        &mut self,
        f: impl FnOnce(&mut Self, usize) -> Result<TypedTree>,
    ) -> Result<(usize, Type)> {
        let id = self.id_to_package.len();
        let old_net = core::mem::take(&mut self.net);
        let old_lazy_redexes = core::mem::take(&mut self.lazy_redexes);
        // Allocate package
        self.id_to_ty.push(Type::Break(Span::default()));
        self.id_to_package.push(Default::default());
        let mut tree = self.with_captures(&Captures::default(), |this| f(this, id))?;

        // Non-principal interaction optimization pass
        tree.tree = self.non_principal_interactions(tree.tree);
        self.lazy_redexes = core::mem::take(&mut self.lazy_redexes)
            .into_iter()
            .map(|(tree, tree1)| {
                (
                    self.non_principal_interactions(tree),
                    self.non_principal_interactions(tree1),
                )
            })
            .collect();
        self.net.redexes = core::mem::take(&mut self.net.redexes)
            .into_iter()
            .map(|(tree, tree1)| {
                (
                    self.non_principal_interactions(tree),
                    self.non_principal_interactions(tree1),
                )
            })
            .collect();

        self.net.ports.push_back(tree.tree);

        self.net.packages = Arc::new(self.id_to_package.clone().into_iter().enumerate().collect());
        // self.net.assert_valid_with(
        //     self.lazy_redexes
        //         .iter()
        //         .map(|(a, b)| [a, b].into_iter())
        //         .flatten(),
        // );
        // This is less efficient, but at least panics will now contain the lazy redexes
        let mut net2 = self.net.clone();
        net2.redexes.append(&mut self.lazy_redexes.clone().into());
        net2.assert_valid();

        self.net.normal();
        self.net
            .redexes
            .append(&mut core::mem::take(&mut self.lazy_redexes).into());
        self.net.assert_valid();
        *self.id_to_ty.get_mut(id).unwrap() = tree.ty.clone();
        *self.id_to_package.get_mut(id).unwrap() = core::mem::take(&mut self.net);
        self.lazy_redexes = old_lazy_redexes;
        self.net = old_net;

        Ok((id, tree.ty))
    }

    fn with_captures<T>(
        &mut self,
        captures: &Captures,
        f: impl FnOnce(&mut Self) -> Result<T>,
    ) -> Result<T> {
        let mut vars = BTreeMap::new();
        for (name, _) in captures.names.iter() {
            let (tree, kind) = self.use_variable(name, false)?;
            vars.insert(Var::Name(name.clone()), (tree, kind));
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
                    return Err(Error::UnusedVar(Span::default()));
                }
                self.net.link(prev_tree.tree, Tree::Era);
                Ok(())
            }
            None => Ok(()),
        }
    }

    fn use_var(&mut self, var: &Var, in_command: bool) -> Result<(TypedTree, VariableKind)> {
        if let Some((tree, kind)) = self.context.vars.remove(var) {
            if in_command {
                return Ok((tree, kind));
            }
            match kind {
                VariableKind::Linear => Ok((tree, kind)),
                kind => {
                    let (w0, w1) = self.net.create_wire();
                    let (v0, v1) = self.net.create_wire();
                    self.net
                        .link(Tree::Dup(Box::new(v0), Box::new(w0)), tree.tree);
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
            Err(Error::UnboundVar(Default::default(), var.clone()))
        }
    }

    fn use_global(&mut self, name: &GlobalName) -> Result<TypedTree> {
        match self.compile_global(name) {
            Ok(value) => Ok(value),
            Err(Error::GlobalNotFound(_)) => Err(Error::GlobalNotFound(name.clone())),
            Err(e) => Err(e),
        }
    }

    fn use_variable(
        &mut self,
        name: &LocalName,
        in_command: bool,
    ) -> Result<(TypedTree, VariableKind)> {
        return self.use_var(&Var::Name(name.clone()), in_command);
    }

    fn create_typed_wire(&mut self, t: Type) -> (TypedTree, TypedTree) {
        let (v0, v1) = self.net.create_wire();
        (
            TypedTree {
                tree: v0,
                ty: t.clone(),
            },
            TypedTree {
                tree: v1,
                ty: t.dual(Span::None),
            },
        )
    }

    fn link_typed(&mut self, a: TypedTree, b: TypedTree) {
        self.net.link(a.tree, b.tree);
    }

    fn either_instance(&mut self, tree: Tree, index: usize, out_of: usize) -> Tree {
        Tree::Signal(index as u16, out_of as u16, Box::new(tree))
    }

    fn choice_instance(&mut self, ctx_out: Tree, branches: Vec<usize>) -> Tree {
        Tree::Choice(Box::new(ctx_out), Arc::from(branches))
    }

    fn normalize_type(&mut self, ty: Type) -> Type {
        match ty {
            Type::Name(loc, name, args) => {
                let ty = self.type_defs.get(&loc, &name, &args).unwrap();
                self.normalize_type(ty)
            }
            Type::DualName(loc, name, args) => {
                let ty = self.type_defs.get_dual(&loc, &name, &args).unwrap();
                self.normalize_type(ty)
            }
            Type::Recursive {
                asc, label, body, ..
            } => self.normalize_type(
                Type::expand_recursive(&asc, &label, &body, &self.type_defs).unwrap(),
            ),
            Type::Iterative {
                asc, label, body, ..
            } => self.normalize_type(
                Type::expand_iterative(&asc, &label, &body, &self.type_defs).unwrap(),
            ),
            ty => ty,
        }
    }

    fn compile_expression(&mut self, expr: &Expression<Type>) -> Result<TypedTree> {
        match expr {
            Expression::Global(_, name, _) => self.use_global(name),
            Expression::Variable(_, name, _) => Ok(self.use_variable(name, false)?.0),

            Expression::Fork {
                captures,
                chan_name,
                chan_type,
                process,
                ..
            } => self.with_captures(captures, |this| {
                let (v0, v1) = this.create_typed_wire(chan_type.clone());
                this.bind_variable(chan_name.clone(), v0)?;
                this.compile_process(process)?;
                Ok(v1)
            }),

            Expression::Primitive(_, value, _) => Ok(TypedTree {
                tree: Tree::Primitive(value.clone()),
                ty: value.get_type(),
            }),

            Expression::External(_, f, typ) => Ok(TypedTree {
                tree: Tree::External(*f),
                ty: typ.clone(),
            }),
        }
    }

    fn compile_process(&mut self, proc: &Process<Type>) -> Result<()> {
        match proc {
            Process::Let {
                name, value, then, ..
            } => {
                let value = self.compile_expression(value)?;
                self.bind_variable(name.clone(), value)?;
                self.compile_process(then)
            }

            Process::Do {
                span,
                name,
                typ,
                command,
            } => self.compile_command(span, name.clone(), typ.clone(), command),

            Process::Telltypes(_, _) => unreachable!(),
        }
    }

    fn compile_command(
        &mut self,
        span: &Span,
        name: LocalName,
        ty: Type,
        cmd: &Command<Type>,
    ) -> Result<()> {
        match cmd {
            Command::Link(expr) => {
                let subject = self.use_variable(&name, true)?.0;
                let value = self.compile_expression(expr)?;
                self.link_typed(subject, value);
                self.end_context()?;
            }
            // types get erased.
            Command::SendType(argument, process) => {
                let subject = self.use_variable(&name, true)?.0;
                let Type::Forall(_, type_name, ret_type) = self.normalize_type(subject.ty.clone())
                else {
                    panic!("Unexpected type for SendType: {:?}", subject.ty);
                };
                let ret_type = ret_type
                    .substitute(BTreeMap::from([(&type_name, argument)]))
                    .unwrap();
                self.bind_variable(name, subject.tree.with_type(ret_type))?;
                self.compile_process(process)?;
            }
            Command::ReceiveType(parameter, process) => {
                let subject = self.use_variable(&name, true)?.0;
                let Type::Exists(_, type_name, ret_type) = self.normalize_type(subject.ty.clone())
                else {
                    panic!("Unexpected type for ReceiveType: {:?}", subject.ty);
                };
                let ret_type = ret_type
                    .clone()
                    .substitute(BTreeMap::from([(
                        &type_name,
                        &Type::Var(span.clone(), parameter.clone()),
                    )]))
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
                let subject = self.use_variable(&name, true)?.0;
                let Type::Function(_, _, ret_type) = self.normalize_type(subject.ty.clone()) else {
                    panic!("Unexpected type for Receive: {:?}", subject.ty);
                };
                let expr = self.compile_expression(expr)?;
                let (v0, v1) = self.create_typed_wire(*ret_type);
                self.bind_variable(name, v0)?;
                self.net.link(
                    Tree::Con(Box::new(v1.tree), Box::new(expr.tree)),
                    subject.tree,
                );
                self.compile_process(process)?;
            }
            Command::Receive(target, _, _, process) => {
                // < name[target] process >
                // ==
                // name = free
                // free = (name target)
                // < process >
                let subject = self.use_variable(&name, true)?.0;
                let Type::Pair(_, arg_type, ret_type) = self.normalize_type(subject.ty.clone())
                else {
                    panic!("Unexpected type for Receive: {:?}", subject.ty);
                };
                let (v0, v1) = self.create_typed_wire(*arg_type);
                let (w0, w1) = self.create_typed_wire(*ret_type);
                self.bind_variable(name, w0)?;
                self.bind_variable(target.clone(), v0)?;
                self.net.link(
                    Tree::Con(Box::new(w1.tree), Box::new(v1.tree)),
                    subject.tree,
                );
                self.compile_process(process)?;
            }
            Command::Signal(chosen, process) => {
                let subject = self.use_variable(&name, true)?.0;
                let Type::Choice(_, branches) = self.normalize_type(subject.ty.clone()) else {
                    panic!("Unexpected type for Signal: {:?}", subject.ty);
                };
                let Some(branch_type) = branches.get(chosen) else {
                    unreachable!()
                };
                let branch_index = branches.keys().position(|k| k == chosen).unwrap();
                let (v0, v1) = self.create_typed_wire(branch_type.clone());
                let choosing_tree = self.either_instance(v1.tree, branch_index, branches.len());
                self.net.link(choosing_tree, subject.tree);
                self.bind_variable(name, v0)?;
                self.compile_process(process)?;
            }
            Command::Case(names, processes) => {
                self.context.unguarded_loop_labels.clear();
                let old_tree = self.use_variable(&name, true)?.0;
                // Multiplex all other variables in the context.
                let (context_in, pack_data) = self.context.pack(None, None, None, &mut self.net);

                let mut branches = vec![];
                let Type::Either(_, required_branches) = self.normalize_type(ty.clone()) else {
                    panic!("Unexpected type for Case: {:?}", ty);
                };
                let mut choice_and_process: Vec<_> = names.iter().zip(processes.iter()).collect();
                choice_and_process.sort_by_key(|k| k.0);

                for ((_, process), branch) in choice_and_process
                    .into_iter()
                    .zip(required_branches.values())
                {
                    let (package_id, _) = self.in_package(|this, _| {
                        let (w0, w1) = this.create_typed_wire(branch.clone());
                        this.bind_variable(name.clone(), w0)?;

                        let context_out = this.context.unpack(&pack_data, &mut this.net);
                        this.compile_process(process)?;
                        Ok(Tree::Con(Box::new(context_out), Box::new(w1.tree))
                            .with_type(Type::Break(Default::default())))
                    })?;
                    branches.push(package_id)
                }
                let t = self.choice_instance(context_in, branches);

                self.net.link(old_tree.tree, t);
            }
            Command::Break => {
                // < name ! >
                // ==
                // name = *
                let a = self.use_variable(&name, true)?.0.tree;
                self.net.link(a, Tree::Era);
                self.end_context()?;
            }
            Command::Continue(process) => {
                // < name ? process >
                // ==
                // name = *
                // < process >
                let a = self.use_variable(&name, true)?.0.tree;
                self.net.link(a, Tree::Era);
                self.compile_process(process)?;
            }
            Command::Begin {
                label,
                captures,
                body,
                ..
            } => {
                let label = LoopLabel(label.clone());

                let (def0, def1) = self.net.create_wire();
                let prev = self.context.vars.insert(
                    Var::Loop(label.0.clone()),
                    (
                        def0.with_type(Type::Break(Span::default())),
                        VariableKind::Replicable,
                    ),
                );

                if let Some((prev_tree, _)) = prev {
                    self.net.link(prev_tree.tree, Tree::Era);
                }

                let mut labels_in_scope: BTreeSet<_> =
                    self.context.loop_points.keys().cloned().collect();
                labels_in_scope.insert(label.clone());
                self.context
                    .loop_points
                    .insert(label.clone(), labels_in_scope);

                self.context.unguarded_loop_labels.push(label.clone());

                let (context_in, pack_data) =
                    self.context
                        .pack(Some(&name), Some(captures), None, &mut self.net);
                let (id, _) = self.in_package(
                    |this, _| {
                        let context_out = this.context.unpack(&pack_data, &mut this.net);
                        this.compile_process(body)?;
                        Ok(context_out.with_type(Type::Break(Span::default())))
                    },
                    //true,
                )?;
                self.net.link(def1, Tree::Package(id));
                self.net.link(context_in, Tree::Package(id));
            }
            Command::Loop(label, driver, captures) => {
                let label = LoopLabel(label.clone());
                if self.context.unguarded_loop_labels.contains(&label) {
                    return Err(Error::UnguardedLoop(span.clone(), label.clone().0));
                }
                let (tree, _) = self.use_var(&Var::Loop(label.0.clone()), false)?;
                let driver_tree = self.use_variable(&name, true)?.0;
                self.bind_variable(driver.clone(), driver_tree)?;
                let labels_in_scope = self.context.loop_points.get(&label).unwrap().clone();
                let (context_in, _) = self.context.pack(
                    Some(driver),
                    Some(captures),
                    Some(&labels_in_scope),
                    &mut self.net,
                );
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
                self.net.link(value.tree, Tree::Era);
            }
        }
        self.context.loop_points = Default::default();
        Ok(())
    }
}

pub fn compile_file(program: &CheckedModule) -> Result<IcCompiled> {
    let mut compiler = Compiler {
        net: Net::default(),
        context: Context {
            vars: BTreeMap::default(),
            loop_points: BTreeMap::default(),
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
    pub(crate) name_to_id: IndexMap<GlobalName, usize>,
    pub(crate) id_to_ty: IndexMap<usize, Type>,
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
    pub fn get_with_name(&self, name: &GlobalName) -> Option<Net> {
        let id = self.name_to_id.get(name)?;
        self.id_to_package.get(id).cloned()
    }

    pub fn get_type_of(&self, name: &GlobalName) -> Option<Type> {
        let id = self.name_to_id.get(name)?;
        self.id_to_ty.get(id).cloned()
    }

    pub fn create_net(&self) -> Net {
        let mut net = Net::default();
        net.packages = self.id_to_package.clone();
        net
    }
}
