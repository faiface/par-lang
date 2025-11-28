use crate::runtime::new::arena::{Arena, Index};
use crate::runtime::new::runtime::{GlobalCont, GlobalValue};
use crate::runtime::{new, old};
use old::compiler::IcCompiled;

use new::runtime::Global;

pub struct NetTranspiler {
    source: old::net::Net,
    dest: Arena,
}

impl NetTranspiler {
    fn transpile_tree_and_alloc(&mut self, source: &old::net::Tree) -> Index<Global> {
        let tree = self.transpile_tree(source);
        self.dest.alloc(tree)
    }
    fn transpile_tree(&mut self, source: &old::net::Tree) -> Global {
        use old::net::Tree;
        match source {
            Tree::Break => Global::Value(GlobalValue::Break),
            Tree::Continue => Global::Destruct(GlobalCont::Continue),
            Tree::Era => Global::Fanout(self.dest.alloc_clone(&[])),
            Tree::Par(a, b) => {
                let a: Index<Global> = self.transpile_tree_and_alloc(a);
                let b: Index<Global> = self.transpile_tree_and_alloc(b);
                Global::Destruct(GlobalCont::Par(a, b))
            }
            Tree::Times(a, b) => {
                let a: Index<Global> = self.transpile_tree_and_alloc(a);
                let b: Index<Global> = self.transpile_tree_and_alloc(b);
                Global::Value(GlobalValue::Pair(a, b))
            }
            Tree::Dup(a, b) => {
                let s = [self.transpile_tree(a), self.transpile_tree(b)];
                Global::Fanout(self.dest.alloc_clone(&s))
            }
            Tree::Signal(arc_str, tree) => Global::Value(GlobalValue::Either(
                arc_str.clone(),
                self.transpile_tree_and_alloc(tree),
            )),
            Tree::Choice(tree, hash_map, _) => todo!(),
            Tree::Box_(tree, _) => todo!(),
            Tree::Var(_) => todo!(),
            Tree::Package(_) => todo!(),
            Tree::SignalRequest(sender) => todo!(),
            Tree::Primitive(primitive) => todo!(),
            Tree::IntRequest(sender) => todo!(),
            Tree::StringRequest(sender) => todo!(),
            Tree::BytesRequest(sender) => todo!(),
            Tree::External(_) => todo!(),
            Tree::ExternalBox(_) => todo!(),
        }
    }
}
/*
pub struct Transpiler<'s> {
    old: IcCompiled,
    dest: &'s mut IndexedArena<'s>,
}

impl<'s> Transpiler<'s> {
    fn transpile(&mut self) {}
}

*/
