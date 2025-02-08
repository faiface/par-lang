//! This module contains a simple implementation for Interaction Combinators.
//! It is not performant; it's mainly here to act as a storage and interchange format

use std::collections::{BTreeMap, VecDeque};

type VarId = usize;

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
#[derive(Debug)]
pub enum Tree {
    Con(Box<Tree>, Box<Tree>),
    Dup(Box<Tree>, Box<Tree>),
    Era,
    Var(usize),
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
}
impl Tree {
    fn map_vars(&mut self, m: &mut impl FnMut(VarId) -> VarId) {
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
            Era => {}
        }
    }
}

#[derive(Default, Debug)]
pub struct Net {
    pub ports: VecDeque<Tree>,
    pub redexes: VecDeque<(Tree, Tree)>,
    pub vars: BTreeMap<usize, Option<Tree>>,
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
        }
    }
    pub fn reduce_one(&mut self) -> bool {
        if let Some((a, b)) = self.redexes.pop_front() {
            self.interact(a, b);
            true
        } else {
            false
        }
    }
    pub fn normal(&mut self) {
        while self.reduce_one() {}
    }
    pub fn link(&mut self, a: Tree, b: Tree) {
        if let Tree::Var(id) = a {
            match self.vars.remove(&id).unwrap() {
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
        }
    }
    pub fn show(&self) -> String {
        use core::fmt::Write;
        let mut s = String::new();
        for i in &self.ports {
            write!(&mut s, "{}\n", self.show_tree(i)).unwrap();
        }
        for (a, b) in &self.redexes {
            write!(&mut s, "{} ~ {}\n", self.show_tree(a), self.show_tree(b)).unwrap();
        }
        s
    }
}
