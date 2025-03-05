use std::collections::BTreeMap;

use super::{Net, Tree};

use pest::Parser;

use pest::iterators::Pairs;

use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "icombs/syntax.pest"]
pub struct Icombs;

#[derive(Default)]
struct State {
    ctx: BTreeMap<String, usize>,
    next_id: usize,
}

impl State {
    fn create_var(&mut self, s: &str) -> Tree {
        if let Some(i) = self.ctx.remove(s) {
            Tree::Var(i)
        } else {
            self.ctx.insert(s.to_string(), self.next_id);
            let tree = Tree::Var(self.next_id);
            self.next_id += 1;
            tree
        }
    }
    fn parse_tree(&mut self, pairs: &mut Pairs<'_, Rule>) -> Tree {
        let pair = pairs.next().unwrap().into_inner().next().unwrap();
        match pair.as_rule() {
            Rule::con_tree => {
                let mut pairs = pair.into_inner();
                let a = self.parse_tree(&mut pairs);
                let b = self.parse_tree(&mut pairs);
                Tree::c(a, b)
            }
            Rule::dup_tree => {
                let mut pairs = pair.into_inner();
                let a = self.parse_tree(&mut pairs);
                let b = self.parse_tree(&mut pairs);
                Tree::c(a, b)
            }
            Rule::era_tree => Tree::e(),
            Rule::var_tree => {
                let pair = pair.into_inner().next().unwrap();
                self.create_var(pair.as_str())
            }
            _ => unreachable!(),
        }
    }
    pub fn parse_net(&mut self, pairs: &mut Pairs<'_, Rule>) -> Net {
        let mut net = Net::default();
        while let Some(pair) = pairs.next() {
            match pair.as_rule() {
                Rule::redex => {
                    let mut pairs = pair.into_inner();
                    let a = self.parse_tree(&mut pairs);
                    let b = self.parse_tree(&mut pairs);
                    net.redexes.push_back((a, b));
                }
                Rule::tree => {
                    let a = self.parse_tree(&mut Pairs::single(pair));
                    net.ports.push_back(a)
                }
                Rule::EOI => {
                    break;
                }
                _ => unreachable!(),
            }
        }
        for i in 0..self.next_id {
            net.vars.insert(i, None);
        }
        net
    }
}

pub fn parse_net(source: &str) -> Result<Net, String> {
    let mut pairs = match Icombs::parse(Rule::program, source) {
        Ok(mut pairs) => pairs.next().unwrap().into_inner(),
        Err(error) => return Err(error.to_string()),
    };
    let mut s = State::default();
    Ok(s.parse_net(&mut pairs))
}
