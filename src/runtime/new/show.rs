use std::fmt::Display;
use std::ops::Deref;

use crate::runtime::new::arena::Arena;
use crate::runtime::new::runtime::{
    Global, GlobalPtr, Instance, Linear, Node, Shared, SyncShared, Value,
};

pub struct Shower<'a> {
    pub arena: &'a Arena,
    pub deref_globals: bool,
}

impl<'a> Shower<'a> {
    pub fn from_arena(arena: &'a Arena) -> Self {
        Self {
            arena,
            deref_globals: true,
        }
    }
}

pub struct Showable<'a, 'b, P>(pub P, pub &'b Shower<'a>);
//pub struct ShowableGlobal<'a, 'b>(&'a Instance, &'a Global, &'b mut Shower<'a>);

impl<'a, 'b> std::fmt::Display for Showable<'a, 'b, &'a Node> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.0 {
            Node::Linear(linear) => write!(f, "-{}", Showable(linear, self.1)),
            Node::Shared(shared) => write!(f, "&{}", Showable(shared, self.1)),
            Node::Global(instance, global) => write!(f, "'{}", Showable(global, self.1)),
        }
    }
}

impl<'a, 'b> std::fmt::Display for Showable<'a, 'b, &'a Box<Node>> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Showable(self.0.as_ref(), self.1).fmt(f)
    }
}

impl<'a, 'b> std::fmt::Display for Showable<'a, 'b, &'a Linear> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.0 {
            Linear::Value(value) => {
                write!(f, "{}", Showable(value, self.1))?;
            }
            Linear::Request(sender) => {
                write!(f, "<external request>")?;
            }
            Linear::ShareHole(mutex) => {
                if let Ok(mut lock) = mutex.try_lock() {
                    match &*lock {
                        crate::runtime::new::runtime::SharedHole::Filled(sync_shared) => {
                            write!(f, "<unexpected filled share hole>")?;
                        }
                        crate::runtime::new::runtime::SharedHole::Unfilled(nodes) => {
                            write!(f, "<unfilled hole>")?;
                        }
                    }
                } else {
                    write!(f, "<locked>")?;
                }
            }
        }
        Ok(())
    }
}

impl<'a, 'b> std::fmt::Display for Showable<'a, 'b, &'a Shared> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.0 {
            Shared::Async(mutex) => {
                if let Ok(mut lock) = mutex.try_lock() {
                    match &*lock {
                        crate::runtime::new::runtime::SharedHole::Filled(sync_shared) => {
                            write!(f, "{}", Showable(sync_shared, self.1))?;
                        }
                        crate::runtime::new::runtime::SharedHole::Unfilled(nodes) => {
                            write!(f, "<waiting>")?;
                        }
                    }
                } else {
                    write!(f, "<locked>")?;
                }
            }
            Shared::Sync(sync_shared) => {
                write!(f, "{}", Showable(sync_shared.as_ref(), self.1))?;
            }
        };
        Ok(())
    }
}
impl<'a, 'b> std::fmt::Display for Showable<'a, 'b, &'a SyncShared> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.0 {
            SyncShared::Package(index, shared) => {
                write!(f, "@{}${}", index.0, Showable(shared, self.1))?;
            }
            SyncShared::Value(value) => {
                write!(f, "{}", Showable(value, self.1))?;
            }
        };
        Ok(())
    }
}

impl<'a, 'b> std::fmt::Display for Showable<'a, 'b, &'a Global> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.0 {
            Global::Variable(id) => {
                write!(f, "{}", id)?;
            }
            Global::GlobalPackage(index, captures) => {
                write!(f, "@{}${}", index.0, Showable(captures, self.1))?;
            }
            Global::Fanout(index) => {
                write!(f, "{{")?;
                for i in self.1.arena.get(index.clone()) {
                    write!(f, "{} ", Showable(i, self.1))?;
                }
                write!(f, "}}")?;
            }
            Global::Destruct(global_cont) => {
                use crate::runtime::new::runtime::GlobalCont::*;
                match global_cont {
                    Continue => write!(f, "?")?,
                    Par(a, b) => {
                        write!(f, "[{}] {}", Showable(b, self.1), Showable(a, self.1))?;
                    }
                    Choice(captures, hash_map, els) => {
                        write!(f, ".{{")?;
                        for (k, v) in hash_map.iter() {
                            write!(f, "{} @{} ", k, v.0)?;
                        }
                        if let Some(els) = els {
                            write!(f, "else @{} ", els.0)?;
                        }
                        write!(f, "}}${}", Showable(captures, self.1))?;
                    }
                }
            }
            Global::Value(value) => {
                write!(f, "{}", Showable(value, self.1))?;
            }
        };
        Ok(())
    }
}

impl<'a, 'b> std::fmt::Display for Showable<'a, 'b, &'a GlobalPtr> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.1.deref_globals {
            write!(f, "{}", Showable(self.1.arena.get(self.0.clone()), self.1))?;
        } else {
            write!(f, "{}", self.0 .0)?;
        }
        Ok(())
    }
}

impl<'a, 'b, P> std::fmt::Display for Showable<'a, 'b, &'a Value<P>>
where
    Showable<'a, 'b, &'a P>: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use crate::runtime::new::runtime::Value::*;
        match self.0 {
            Break => write!(f, "!")?,
            Pair(a, b) => {
                write!(f, "({}) {}", Showable(b, self.1), Showable(a, self.1))?;
            }
            Either(arc_str, payload) => {
                write!(f, ".{} {}", arc_str, Showable(payload, self.1))?;
            }
            ExternalFn(_) => {
                write!(f, "<external fn>")?;
            }
            ExternalArc(_) => {
                write!(f, "<external arc>")?;
            }
            Primitive(primitive) => {
                write!(f, "#{:?}", primitive)?;
            }
        };
        Ok(())
    }
}
