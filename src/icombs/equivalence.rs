//! This module contains a function which checks whether two disconnected trees are equivalent.
use std::collections::{BTreeMap, BTreeSet};

use super::{Net, Tree, VarId};
fn uniqueify_vars(mut tree: Tree, new_id: &mut usize) -> (Tree, BTreeMap<usize, usize>) {
    let mut original_to_new: BTreeMap<usize, usize> = BTreeMap::new();
    let mut connection_map: BTreeMap<usize, usize> = BTreeMap::new();
    tree.map_vars(&mut |id: VarId| -> VarId {
        if let Some(other) = original_to_new.get(&id) {
            connection_map.insert(new_id.clone(), other.clone());
            connection_map.insert(other.clone(), *new_id);
        } else {
            original_to_new.insert(id, *new_id);
        }
        *new_id += 1;
        *new_id - 1
    });
    (tree, connection_map)
}

pub fn are_equivalent(net: &mut Net, left: Tree, right: Tree) -> bool {
    // we first remove all variables from the left and right trees,

    let mut new_id = 0;
    let (left, left_map) = uniqueify_vars(left, &mut new_id);
    let (right, right_map) = uniqueify_vars(right, &mut new_id);

    for id in 0..new_id {
        net.ports.push_back(Tree::Var(id));
        net.vars.insert(id, None);
    }
    net.redexes.push_back((left, right));
    net.normal();
    // now, inspect the resulting ports
    // the condition for equivalence is:
    // var A is connected to var B iff left_map[A] is connected to right_map[B]
    let mut is_connected_to = BTreeSet::new();
    for (a, tree) in net.ports.iter().enumerate() {
        let Tree::Var(b) = tree else { return false };
        if a != *b {
            is_connected_to.insert((a, *b));
            is_connected_to.insert((*b, a));
        }
    }

    for (a, b) in is_connected_to.iter() {
        let (a, b) = if left_map.contains_key(&a) {
            (a, b)
        } else {
            (b, a)
        };
        let Some(a_other) = left_map.get(&a) else {
            return false;
        };
        let Some(b_other) = right_map.get(&b) else {
            return false;
        };
        if !(is_connected_to.contains(&(*a_other, *b_other))) {
            return false;
        }
    }

    true
}
