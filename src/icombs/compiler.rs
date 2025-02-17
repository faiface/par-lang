use std::{
    collections::BTreeMap,
    fmt::{Debug, Display},
    sync::Arc,
};

use indexmap::IndexMap;
use std::hash::Hash;

use crate::par::{
    language::Internal,
    parse::{Name, Program},
    process::{Captures, Command, Expression, Process},
};

use super::net::{Net, Tree};

type Prog<Loc, Name, Typ> = Program<Name, Arc<Expression<Loc, Name, Typ>>>;

pub struct Compiler<'program, Loc, Name: Debug + Clone + Eq + Hash, Typ> {
    net: Net,
    vars: IndexMap<Name, Tree>,
    types: IndexMap<Name, Typ>,
    // Vars that can be replicated with dup and era
    replicable_vars: IndexMap<Name, Tree>,
    program: &'program Prog<Loc, Name, Typ>,
    global_name_to_id: IndexMap<Name, usize>,
    id_to_package: Vec<Tree>,
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

impl<'program, Loc: Debug, Name: Debug + Clone + Eq + Hash + Display, Typ>
    Compiler<'program, Loc, Name, Typ>
{
    fn compile_global(&mut self, name: &Name) -> Option<Tree> {
        if let Some(tree) = self.global_name_to_id.get(name) {
            return Some(Tree::Package(*tree));
        };
        let global = self.program.definitions.get(name)?;
        let old_net = core::mem::take(&mut self.net);

        let id = self.id_to_package.len();
        self.global_name_to_id.insert(name.clone(), id);
        let tree = self.with_captures(&Captures::default(), |this| {
            this.compile_expression(global.as_ref())
        });
        self.net.ports.push_back(tree);
        self.net.packages = Arc::new(self.id_to_package.clone().into_iter().enumerate().collect());
        println!("{}", self.net.show());
        self.net.normal();
        let tree = self.net.ports.pop_back().unwrap();
        self.id_to_package.push(tree);
        self.net = old_net;

        Some(Tree::Package(id))
    }
    fn with_captures<T>(
        &mut self,
        captures: &Captures<Loc, Name>,
        f: impl FnOnce(&mut Self) -> T,
    ) -> T {
        println!("{:?}", captures);
        let mut replicables = IndexMap::new();
        let mut vars = IndexMap::new();
        for (name, _) in captures.names.iter() {
            let (tree, is_replicable) = self.use_variable(name);
            if is_replicable {
                replicables.insert(name.clone(), tree);
            } else {
                vars.insert(name.clone(), tree);
            }
        }
        core::mem::swap(&mut replicables, &mut self.replicable_vars);
        core::mem::swap(&mut vars, &mut self.vars);
        let t = f(self);
        core::mem::swap(&mut replicables, &mut self.replicable_vars);
        core::mem::swap(&mut vars, &mut self.vars);
        // drop all replicables
        for (_, v) in replicables.into_iter() {
            self.net.link(v, Tree::e());
        }
        // assert no variables were unclosed
        if !vars.is_empty() {
            let s: Vec<String> = vars.keys().map(|x| format!("{}", x)).collect();
            panic!("some variablse were not closed: {}", s.join(", "));
        }
        t
    }
    fn bind_variable(&mut self, name: &Name, tree: Tree) {
        assert!(self.vars.insert(name.clone(), tree).is_none())
    }
    // TODO: This should do dereliction, when necessary.
    fn instantiate_variable(&mut self, name: &Name) -> Tree {
        self.use_variable(name).0
    }
    fn use_variable(&mut self, name: &Name) -> (Tree, bool) {
        if let Some(value) = self.vars.swap_remove(name) {
            (value, false)
        } else if let Some(old_value) = self.replicable_vars.swap_remove(name) {
            let (w0, w1) = self.net.create_wire();
            let (v0, v1) = self.net.create_wire();
            self.net.link(Tree::d(v0, w0), old_value);
            self.replicable_vars.insert(name.clone(), w1);
            (v1, true)
        } else if self.compile_global(name).is_some() {
            let value = self.compile_global(name).unwrap();
            self.replicable_vars.insert(name.clone(), value.clone());
            (value, true)
        } else {
            panic!("unknown variable {}", name)
        }
    }
    fn compile_expression(&mut self, expr: &Expression<Loc, Name, Typ>) -> Tree {
        match expr {
            Expression::Reference(_, name, _) => self.use_variable(name).0,
            Expression::Fork(_, captures, name, _, _, proc) => {
                self.with_captures(captures, |this| {
                    let (v0, v1) = this.net.create_wire();
                    this.bind_variable(name, v0);
                    this.compile_process(proc);
                    v1
                })
            }
        }
    }
    fn compile_process(&mut self, proc: &Process<Loc, Name, Typ>) {
        match proc {
            Process::Let(_, key, _, _, value, rest) => {
                let value = self.compile_expression(value);
                self.vars.insert(key.clone(), value);
                self.compile_process(rest);
            }

            Process::Do(_, name, _, command) => self.compile_command(name.clone(), command),
            _ => todo!(),
        }
    }
    fn link_var(&mut self, name: Name, a: Tree) {
        if let Some(b) = self.vars.swap_remove(&name) {
            self.net.link(a, b)
        } else {
            self.vars.insert(name, a);
        }
    }
    fn show_state(&mut self) {
        println!("Vars:");
        for (k, v) in &self.vars {
            println!(" {}: {}", k, self.net.show_tree(&v))
        }
        println!("Replicable vars:");
        for (k, v) in &self.replicable_vars {
            println!(" {}: {}", k, self.net.show_tree(&v))
        }
        println!("Net:");
        println!(" {}", self.net.show());
        println!("");
    }
    fn compile_command(&mut self, name: Name, cmd: &Command<Loc, Name, Typ>) {
        match cmd {
            Command::Link(a) => {
                let a = self.compile_expression(a);
                let b = self.instantiate_variable(&name);
                self.net.link(a, b);
            }
            // types get erased.
            Command::SendType(_, process) => self.compile_process(process),
            Command::ReceiveType(_, process) => self.compile_process(process),
            Command::Send(expr, process) => {
                let expr = self.compile_expression(expr);
                let (v0, v1) = self.net.create_wire();
                let old_tree = self.vars.insert(name, v1).unwrap();
                self.net.link(Tree::c(v0, expr), old_tree);
                self.compile_process(process);
            }
            Command::Receive(target, _, process) => {
                let (v0, v1) = self.net.create_wire();
                let (w0, w1) = self.net.create_wire();
                let old_tree = self
                    .vars
                    .insert(name.clone(), v1)
                    .unwrap_or_else(|| panic!("could not find var {}", name));
                self.vars.insert(target.clone(), w1);
                self.net.link(Tree::c(v0, w0), old_tree);
                self.compile_process(process);
            }
            Command::Choose(signal, process) => {
                // TODO this is really terrible, but it's just a proof of concept solution.
                let is_right = format!("{}", signal) == "right";
                let is_left = format!("{}", signal) == "left";
                assert!(is_right ^ is_left);

                let (v0, v1) = self.net.create_wire();
                let (w0, w1) = self.net.create_wire();
                let old_tree = self.vars.insert(name, v1).unwrap();
                self.net.link(
                    if is_left {
                        Tree::c(w0, Tree::c(Tree::c(w1, v0), Tree::e()))
                    } else {
                        Tree::c(w0, Tree::c(Tree::e(), Tree::c(w1, v0)))
                    },
                    old_tree,
                );
                self.compile_process(process);
            }
            Command::Match(names, processes) => {
                let old_tree = self.instantiate_variable(&name);
                // Multiplex all other variables in the context.
                let mut m_trees = vec![];
                let mut m_vars = vec![];
                let mut m_is_replicable = vec![];
                for (k, v) in core::mem::take(&mut self.vars) {
                    m_vars.push(k);
                    m_trees.push(v);
                    m_is_replicable.push(false);
                }
                for k in self.replicable_vars.clone().keys() {
                    let v = self.use_variable(&k).0;
                    m_vars.push(k.clone());
                    m_trees.push(v);
                    m_is_replicable.push(true);
                }
                let context_in = multiplex_trees(m_trees);

                let mut case_left: Option<Tree> = None;
                let mut case_right: Option<Tree> = None;
                for (name_here, process) in names.iter().zip(processes.iter()) {
                    let (w0, w1) = self.net.create_wire();
                    self.vars.insert(name.clone(), w0);

                    // multiplex the conetxt frmo the inside now
                    let mut m_trees = vec![];
                    for (name, is_replicable) in m_vars.iter().zip(m_is_replicable.iter()) {
                        let (v0, v1) = self.net.create_wire();
                        if *is_replicable {
                            self.replicable_vars.insert(name.clone(), v0);
                        } else {
                            self.vars.insert(name.clone(), v0);
                        }

                        m_trees.push(v1);
                    }
                    let context_out = multiplex_trees(m_trees);

                    self.compile_process(process);
                    if format!("{}", name_here) == "left" {
                        case_left = Some(Tree::c(context_out, w1));
                    } else if format!("{}", name_here) == "right" {
                        case_right = Some(Tree::c(context_out, w1));
                    } else {
                        todo!()
                    }
                }
                self.net.link(
                    old_tree,
                    Tree::c(context_in, Tree::c(case_left.unwrap(), case_right.unwrap())),
                );
            }
            Command::Break => {
                self.link_var(name, Tree::e());
            }
            Command::Continue(process) => {
                self.link_var(name, Tree::e());
                self.compile_process(process);
            }
            Command::Begin(name, rest) => {
                unreachable!()
            }
            Command::Loop(name) => {
                unreachable!()
            }
        }
    }
}

pub fn compile_file<Loc: Debug, Typ>(program: &Prog<Loc, Internal<Name>, Typ>) -> IcCompiled {
    let mut compiler = Compiler {
        net: Net::default(),
        vars: IndexMap::default(),
        replicable_vars: IndexMap::default(),
        global_name_to_id: Default::default(),
        id_to_package: Default::default(),
        types: IndexMap::new(),
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
    id_to_package: Arc<IndexMap<usize, Tree>>,
    name_to_id: IndexMap<Internal<Name>, usize>,
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
