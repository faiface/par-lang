use std::{
    fmt::{Debug, Display},
    sync::Arc,
};

use indexmap::IndexMap;
use std::hash::Hash;

use crate::process::{Command, Expression, Process};

use super::net::{Net, Tree};

type Globals<Loc, Name> = Arc<IndexMap<Name, Arc<Expression<Loc, Name>>>>;

pub struct Compiler<Loc, Name: Debug + Clone + Eq + Hash> {
    net: Net,
    vars: IndexMap<Name, Tree>,
    globals: Globals<Loc, Name>,
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

impl<Loc: Debug, Name: Debug + Clone + Eq + Hash + Display> Compiler<Loc, Name> {
    fn compile_expression(&mut self, expr: &Expression<Loc, Name>) -> Tree {
        match expr {
            Expression::Reference(_, name) => self
                .vars
                .swap_remove(name)
                .or({
                    let global = self.globals.get(name).map(|x| Arc::clone(x));
                    global.map(|x| {
                        let old_vars = core::mem::take(&mut self.vars);
                        let e = self.compile_expression(&x);
                        self.vars = old_vars;
                        e
                    })
                })
                .unwrap_or_else(|| panic!("could not find name {}", name)),
            Expression::Fork(_, _, name, proc) => {
                // TODO: We don't handle captures yet (I don't know if they have an effect)
                let (v0, v1) = self.net.create_wire();
                self.link_var(name.clone(), v0);
                self.compile_process(proc);
                v1
            }
        }
    }
    fn compile_process(&mut self, proc: &Process<Loc, Name>) {
        match proc {
            Process::Let(_, key, value, rest) => {
                let value = self.compile_expression(value);
                self.vars.insert(key.clone(), value);
                self.compile_process(rest);
            }

            Process::Do(_, name, command) => self.compile_command(name.clone(), command),
        }
    }
    fn link_var(&mut self, name: Name, a: Tree) {
        if let Some(b) = self.vars.swap_remove(&name) {
            self.net.link(a, b)
        } else {
            self.vars.insert(name, a);
        }
    }
    fn compile_command(&mut self, name: Name, cmd: &Command<Loc, Name>) {
        match cmd {
            Command::Link(a) => {
                let a = self.compile_expression(a);
                self.link_var(name, a);
            }
            Command::Send(expr, process) => {
                let expr = self.compile_expression(expr);
                let (v0, v1) = self.net.create_wire();
                let old_tree = self.vars.insert(name, v1).unwrap();
                self.net.link(Tree::c(v0, expr), old_tree);
                self.compile_process(process);
            }
            Command::Receive(target, process) => {
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
            Command::Either(names, processes) => {
                let old_tree = self.vars.swap_remove(&name).unwrap();
                // now, multiplex all other variables.
                let mut m_trees = vec![];
                let mut m_vars = vec![];
                for (k, v) in core::mem::take(&mut self.vars) {
                    m_vars.push(k);
                    m_trees.push(v);
                }
                let context_in = multiplex_trees(m_trees);

                let mut case_left: Option<Tree> = None;
                let mut case_right: Option<Tree> = None;
                for (name_here, process) in names.iter().zip(processes.iter()) {
                    let (w0, w1) = self.net.create_wire();
                    self.vars.insert(name.clone(), w0);
                    let mut m_trees = vec![];
                    for i in m_vars.iter() {
                        let (v0, v1) = self.net.create_wire();
                        self.vars.insert(i.clone(), v0);
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
            Command::Begin(_, _) => todo!(),
            Command::Loop(_) => todo!(),
        }
    }
}

pub fn compile_expression<Loc: Debug, Name: Hash + Eq + Display + Debug + Clone>(
    expr: &Expression<Loc, Name>,
    globals: Globals<Loc, Name>,
) -> Net {
    let mut compiler = Compiler {
        net: Net::default(),
        vars: IndexMap::default(),
        globals,
    };
    let tree = compiler.compile_expression(expr);
    compiler.net.ports.push_back(tree);
    compiler.net
}
