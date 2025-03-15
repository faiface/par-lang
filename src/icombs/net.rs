//! This module contains a simple implementation for Interaction Combinators.
//! It is not performant; it's mainly here to act as a storage and interchange format

use std::any::Any;
use std::collections::{BTreeMap, VecDeque};
use std::sync::Arc;

use indexmap::IndexMap;

pub type VarId = usize;

pub fn number_to_string(mut number: usize) -> String {
    let mut result = String::new();
    number += 1;
    while number > 0 {
        let remainder = (number - 1) % 26;
        let character = (b'a' + remainder as u8) as char;
        result.insert(0, character);
        number = (number - 1) / 26;
    }
    result
}

pub enum Tree {
    Con(Box<Tree>, Box<Tree>),
    Dup(Box<Tree>, Box<Tree>),
    Era,
    Var(usize),
    Package(usize),
    Ext(
        Box<
            dyn FnOnce(
                    &mut Net,
                    Result<Tree, Box<dyn Any + Send + Sync>>,
                    Box<dyn Any + Send + Sync>,
                ) + Send
                + Sync,
        >,
        Box<dyn Any + Send + Sync>,
    ),
}

impl Tree {
    pub fn c(a: Tree, b: Tree) -> Tree {
        Tree::Con(Box::new(a), Box::new(b))
    }
    pub fn d(a: Tree, b: Tree) -> Tree {
        Tree::Dup(Box::new(a), Box::new(b))
    }
    pub fn e() -> Tree {
        Tree::Era
    }
    pub fn ext(
        f: impl FnOnce(&mut Net, Result<Tree, Box<dyn Any + Send + Sync>>, Box<dyn Any + Send + Sync>)
            + 'static
            + Send
            + Sync,
        a: impl Any + Send + Sync,
    ) -> Tree {
        Tree::Ext(Box::new(f), Box::new(a))
    }
    pub fn map_vars(&mut self, m: &mut impl FnMut(VarId) -> VarId) {
        use Tree::*;
        match self {
            Var(x) => *x = m(*x),
            Con(a, b) => {
                a.map_vars(m);
                b.map_vars(m);
            }
            Dup(a, b) => {
                a.map_vars(m);
                b.map_vars(m);
            }
            _ => {}
        }
    }
    fn map_vars_tree(&mut self, m: &mut impl FnMut(VarId) -> Tree) {
        use Tree::*;
        match self {
            t @ Var(..) => {
                let Var(id) = &t else { unreachable!() };
                *t = m(id.clone())
            }
            Con(a, b) => {
                a.map_vars_tree(m);
                b.map_vars_tree(m);
            }
            Dup(a, b) => {
                a.map_vars_tree(m);
                b.map_vars_tree(m);
            }
            _ => {}
        }
    }
    pub fn show(&self) -> String {
        self.show_with_context(&mut (BTreeMap::new(), 0))
    }
    pub fn show_with_context(&self, ctx: &mut (BTreeMap<usize, String>, usize)) -> String {
        use Tree::*;
        match self {
            Var(id) => {
                if let Some(name) = ctx.0.get(id) {
                    name.clone()
                } else {
                    ctx.1 += 1;
                    let free_var = ctx.1 - 1;
                    ctx.0.insert(*id, number_to_string(free_var));
                    number_to_string(free_var)
                }
            }
            Con(a, b) => format!(
                "({} {})",
                a.show_with_context(ctx),
                b.show_with_context(ctx)
            ),
            Dup(a, b) => format!(
                "[{} {}]",
                a.show_with_context(ctx),
                b.show_with_context(ctx)
            ),
            Era => format!("*"),
            Package(id) => format!("@{}", id),
            Ext(f, _) => format!("<ext>"),
        }
    }
}

impl core::fmt::Debug for Tree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Tree::Con(a, b) => f.debug_tuple("Con").field(a).field(b).finish(),
            Tree::Dup(a, b) => f.debug_tuple("Dup").field(a).field(b).finish(),
            Tree::Era => f.debug_tuple("Era").finish(),
            Tree::Var(id) => f.debug_tuple("Var").field(id).finish(),
            Tree::Package(id) => f.debug_tuple("Package").field(id).finish(),
            Tree::Ext(_, _) => f.debug_tuple("Ext").finish_non_exhaustive(),
        }
    }
}

impl Clone for Tree {
    fn clone(&self) -> Self {
        match self {
            Tree::Con(a, b) => Tree::Con(a.clone(), b.clone()),
            Tree::Dup(a, b) => Tree::Dup(a.clone(), b.clone()),
            Tree::Era => Tree::Era,
            Tree::Var(id) => Tree::Var(id.clone()),
            Tree::Package(id) => Tree::Package(id.clone()),
            Tree::Ext(_, _) => panic!("Can't clone `Ext` tree!"),
        }
    }
}

#[derive(Debug, Default)]
pub struct Net {
    pub ports: VecDeque<Tree>,
    pub redexes: VecDeque<(Tree, Tree)>,
    pub vars: BTreeMap<usize, Option<Tree>>,
    pub packages: Arc<IndexMap<usize, Tree>>,
}
impl Net {
    fn interact(&mut self, a: Tree, b: Tree) {
        use Tree::*;
        match (a, b) {
            (Var(..), _) | (_, Var(..)) => unreachable!(),
            (Era, Era) => (),
            (Con(a0, a1), Era) | (Dup(a0, a1), Era) | (Era, Con(a0, a1)) | (Era, Dup(a0, a1)) => {
                self.link(*a0, Era);
                self.link(*a1, Era);
            }
            (Con(a0, a1), Con(b0, b1)) | (Dup(a0, a1), Dup(b0, b1)) => {
                self.link(*a0, *b0);
                self.link(*a1, *b1);
            }
            (Con(a0, a1), Dup(b0, b1)) | (Dup(b0, b1), Con(a0, a1)) => {
                let (a00, b00) = self.create_wire();
                let (a01, b01) = self.create_wire();
                let (a10, b10) = self.create_wire();
                let (a11, b11) = self.create_wire();
                self.link(*a0, Tree::Dup(Box::new(a00), Box::new(a01)));
                self.link(*a1, Tree::Dup(Box::new(a10), Box::new(a11)));
                self.link(*b0, Tree::Con(Box::new(b00), Box::new(b10)));
                self.link(*b1, Tree::Con(Box::new(b01), Box::new(b11)));
            }
            (Ext(f, a), b) | (b, Ext(f, a)) => {
                f(self, Ok(b), a);
            }
            (Package(_), Era) | (Era, Package(_)) => {}
            (Package(id), Dup(a, b)) | (Dup(a, b), Package(id)) => {
                self.link(*a, Package(id));
                self.link(*b, Package(id));
            }
            (Package(id), a) | (a, Package(id)) => {
                let b = self.dereference_package(id);
                self.interact(a, b);
            }
        }
    }
    pub fn freshen_variables(&mut self, tree: &mut Tree) {
        let mut package_to_net: BTreeMap<usize, usize> = BTreeMap::new();
        tree.map_vars(&mut |var_id| {
            if let Some(id) = package_to_net.get(&var_id) {
                id.clone()
            } else {
                let id = self.allocate_var_id();
                self.vars.insert(id, None);
                package_to_net.insert(var_id, id);
                id
            }
        });
    }
    pub fn dereference_package(&mut self, package: usize) -> Tree {
        let mut tree = self
            .packages
            .get(&package)
            .unwrap_or_else(|| panic!("Unknown package with ID {}", package))
            .clone();
        // Now, we have to freshen all variables in the tree
        self.freshen_variables(&mut tree);
        tree
    }
    /// Returns whether a reduction was carried out
    pub fn reduce_one(&mut self) -> bool {
        if let Some((a, b)) = self.redexes.pop_front() {
            self.interact(a, b);
            true
        } else {
            false
        }
    }
    pub fn substitute_tree(&mut self, tree: &mut Tree) {
        match tree {
            Tree::Con(a, b) | Tree::Dup(a, b) => {
                self.substitute_tree(a);
                self.substitute_tree(b);
            }
            t @ Tree::Var(_) => {
                let Tree::Var(id) = t else { unreachable!() };
                if let Some(Some(mut a)) = self.vars.remove(id) {
                    self.substitute_tree(&mut a);
                    *t = a;
                } else {
                    self.vars.insert(*id, None);
                }
            }
            _ => {}
        }
    }
    pub fn substitute_ref(&self, tree: &Tree) -> Tree {
        match tree {
            Tree::Con(a, b) => Tree::c(self.substitute_ref(a), self.substitute_ref(b)),
            Tree::Dup(a, b) => Tree::d(self.substitute_ref(a), self.substitute_ref(b)),
            Tree::Var(id) => {
                if let Some(Some(a)) = self.vars.get(id) {
                    self.substitute_ref(&a)
                } else {
                    tree.clone()
                }
            }
            _ => tree.clone(),
        }
    }
    pub fn normal(&mut self) {
        while self.reduce_one() {}
        // dereference all variables
        let mut ports = core::mem::take(&mut self.ports);
        ports.iter_mut().for_each(|x| self.substitute_tree(x));
        self.ports = ports;
    }
    pub fn link(&mut self, a: Tree, b: Tree) {
        if let Tree::Var(id) = a {
            match self.vars.remove(&id).unwrap_or_else(||{
                panic!("A variable, with id {id}, was linked to more than twice\n\
                    This is an internal Par bug, which can be caused by many things, such as incorrectly cloning `Tree`s\n\
                    This happened while linking\n\
                    \t{a:?}\nand\n\t{b:?}",
                )

            }) {
                Some(a) => {
                    self.link(a, b);
                }
                None => {
                    self.vars.insert(id, Some(b));
                }
            }
        } else if let Tree::Var(id) = b {
            self.link(Tree::Var(id), a)
        } else {
            self.redexes.push_back((a, b))
        }
    }
    pub fn allocate_var_id(&mut self) -> VarId {
        for i in 0.. {
            if self.vars.get(&i).is_none() {
                return i;
            }
        }
        unreachable!();
    }
    pub fn create_wire(&mut self) -> (Tree, Tree) {
        let id = self.allocate_var_id();
        self.vars.insert(id, None);
        (Tree::Var(id), Tree::Var(id))
    }
    pub fn map_vars(&mut self, m: &mut impl FnMut(VarId) -> VarId) {
        self.ports.iter_mut().for_each(|x| x.map_vars(m));
        self.redexes.iter_mut().for_each(|(a, b)| {
            a.map_vars(m);
            b.map_vars(m)
        });
        let vars = core::mem::take(&mut self.vars);
        self.vars = vars
            .into_iter()
            .map(|(mut k, mut v)| {
                k = m(k);
                v.as_mut().map(|x| x.map_vars(m));
                (k, v)
            })
            .collect();
    }
    pub fn map_vars_tree(&mut self, m: &mut impl FnMut(VarId) -> Tree) {
        self.ports.iter_mut().for_each(|x| x.map_vars_tree(m));
        self.redexes.iter_mut().for_each(|(a, b)| {
            a.map_vars_tree(m);
            b.map_vars_tree(m)
        });
    }
    pub fn show_tree(&self, t: &Tree) -> String {
        use Tree::*;
        match t {
            Var(id) => {
                if let Some(Some(b)) = self.vars.get(id) {
                    self.show_tree(b)
                } else {
                    number_to_string(*id)
                }
            }
            Con(a, b) => format!("({} {})", self.show_tree(a), self.show_tree(b)),
            Dup(a, b) => format!("[{} {}]", self.show_tree(a), self.show_tree(b)),
            Era => format!("*"),
            Package(id) => format!("@{}", id),
            Ext(..) => format!("<ext>"),
        }
    }
    pub fn show(&self) -> String {
        self.show_indent(0)
    }
    pub fn show_indent(&self, indent: usize) -> String {
        use core::fmt::Write;
        let indent_string = "    ".repeat(indent);
        let mut s = String::new();
        for i in &self.ports {
            write!(&mut s, "{}{}\n", indent_string, self.show_tree(i)).unwrap();
        }
        for (a, b) in &self.redexes {
            write!(
                &mut s,
                "{}{} ~ {}\n",
                indent_string,
                self.show_tree(a),
                self.show_tree(b)
            )
            .unwrap();
        }
        s
    }
    pub fn is_active(&self, tree: &Tree) -> bool {
        if let Tree::Var(_) = self.substitute_ref(tree) {
            false
        } else {
            true
        }
    }
}
