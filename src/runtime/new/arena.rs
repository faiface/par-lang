use std::fmt::Debug;
use std::sync::OnceLock;

use crate::runtime::new::runtime::{GlobalCont, Value};

use super::runtime::{Global, Package};

#[derive(Default)]
/// The `Arena` is a store for values from a finite set of types,
/// and returns indices into the arena. Allocation is done using [`Arena::alloc`],
/// and values can be accessed later with [`Arena::get`]
pub struct Arena {
    nodes: Vec<Global>,
    strings: String,
    packages: Vec<OnceLock<Package>>,
}

impl Arena {
    /// Get a reference
    pub fn get<T: Indexable + ?Sized>(&self, index: Index<T>) -> &T {
        T::get(self, index)
    }
    pub fn alloc<T: Indexable>(&mut self, data: T) -> Index<T> {
        T::alloc(self, data)
    }
    pub fn alloc_clone<T: Indexable + ?Sized>(&mut self, data: &T) -> Index<T> {
        T::alloc_clone(self, data)
    }
}

impl std::fmt::Display for Arena {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fn fmt_captures(
            f: &mut std::fmt::Formatter<'_>,
            this: &Arena,
            global: &Global,
        ) -> std::fmt::Result {
            if !matches!(global, Global::Value(Value::Break)) {
                write!(f, "$")?;
                fmt_global(f, this, &global)?;
            }
            Ok(())
        }
        fn fmt_global(
            f: &mut std::fmt::Formatter<'_>,
            this: &Arena,
            global: &Global,
        ) -> std::fmt::Result {
            match global {
                Global::Variable(id) => {
                    write!(f, "{}", id)?;
                }
                Global::GlobalPackage(index, captures) => {
                    write!(f, "@{}", index.0)?;
                    fmt_captures(f, this, this.get(captures.clone()))?;
                }
                Global::Fanout(index) => {
                    write!(f, "{{")?;
                    for i in this.get(index.clone()) {
                        fmt_global(f, this, i)?;
                        write!(f, " ")?;
                    }
                    write!(f, "}}")?;
                }
                Global::Destruct(global_cont) => {
                    use crate::runtime::new::runtime::GlobalCont::*;
                    match global_cont {
                        Continue => write!(f, "?")?,
                        Par(a, b) => {
                            write!(f, "[")?;
                            fmt_global(f, this, this.get(b.clone()))?;
                            write!(f, "] ")?;
                            fmt_global(f, this, this.get(a.clone()))?;
                        }
                        Choice(captures, hash_map, els) => {
                            write!(f, ".{{")?;
                            for (k, v) in hash_map.iter() {
                                write!(f, "{} @{} ", k, v.0)?;
                            }
                            if let Some(els) = els {
                                write!(f, "else @{} ", els.0)?;
                            }
                            write!(f, "}}")?;
                            fmt_captures(f, this, this.get(captures.clone()))?;
                        }
                    }
                }
                Global::Value(value) => {
                    use crate::runtime::new::runtime::Value::*;
                    match value {
                        Break => write!(f, "!")?,
                        Pair(a, b) => {
                            write!(f, "(")?;
                            fmt_global(f, this, this.get(b.clone()))?;
                            write!(f, ") ")?;
                            fmt_global(f, this, this.get(a.clone()))?;
                        }
                        Either(arc_str, payload) => {
                            write!(f, ".{} ", arc_str)?;
                            fmt_global(f, this, this.get(payload.clone()))?;
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
                    }
                }
            };
            Ok(())
        }
        for (idx, package) in self.packages.iter().enumerate() {
            let Some(lock) = package.get() else {
                write!(f, "@{} = <unfilled>\n", idx)?;
                continue;
            };
            if lock.debug_name.len() > 0 {
                write!(f, "// {}\n", lock.debug_name)?;
            }
            write!(f, "@{} = ", idx)?;
            fmt_global(f, self, &lock.root)?;
            if !matches!(lock.captures, Global::Destruct(GlobalCont::Continue)) {
                write!(f, "\n    $ ")?;
                fmt_global(f, self, &lock.captures)?;
            }
            for (a, b) in &lock.redexes {
                write!(f, "\n     ")?;
                fmt_global(f, self, &a)?;
                write!(f, " ~ ")?;
                fmt_global(f, self, &b)?;
            }
            write!(f, "\n")?;
        }
        Ok(())
    }
}

pub struct Index<T: Indexable + ?Sized>(T::Store);

/// The `Indexable` trait is implemented by all types that are contained by an `Arena`.
/// It defines a [`Indexable::Store`] associated type, which determines what is needed to
/// index into a value of this type. For example, sized values usually require a `usize` to index them,
/// which represents the offset into the array that contains it
/// but a slice type requires a pair of offset and length.
pub trait Indexable {
    type Store: Clone;
    fn get<'s>(store: &'s Arena, index: Index<Self>) -> &'s Self;
    fn alloc<'s>(store: &'s mut Arena, data: Self) -> Index<Self>
    where
        Self: Sized;
    fn alloc_clone<'s>(_store: &'s mut Arena, _data: &Self) -> Index<Self> {
        todo!()
    }
}

impl Indexable for [Global] {
    type Store = (usize, usize);
    fn get<'s>(store: &'s Arena, index: Index<Self>) -> &'s Self {
        &store.nodes[index.0 .0..index.0 .0 + index.0 .1]
    }
    fn alloc_clone<'s>(store: &'s mut Arena, data: &Self) -> Index<Self> {
        let start = store.nodes.len();
        store.nodes.extend_from_slice(data);
        Index((start, data.len()))
    }
}
impl Indexable for str {
    type Store = (usize, usize);
    fn get<'s>(store: &'s Arena, index: Index<Self>) -> &'s Self {
        &store.strings[index.0 .0..index.0 .0 + index.0 .1]
    }
    fn alloc_clone<'s>(store: &'s mut Arena, data: &Self) -> Index<Self> {
        let start = store.strings.len();
        store.strings.push_str(data);
        Index((start, data.len()))
    }
}
impl Indexable for Global {
    type Store = usize;
    fn get<'s>(store: &'s Arena, index: Index<Self>) -> &'s Self {
        &store.nodes[index.0]
    }
    fn alloc<'s>(store: &'s mut Arena, data: Self) -> Index<Self> {
        let start = store.nodes.len();
        store.nodes.push(data);
        Index(start)
    }
}
impl Indexable for OnceLock<Package> {
    type Store = usize;
    fn get<'s>(store: &'s Arena, index: Index<Self>) -> &'s Self {
        &store.packages[index.0]
    }
    fn alloc<'s>(store: &'s mut Arena, data: Self) -> Index<Self> {
        let start = store.packages.len();
        store.packages.push(data);
        Index(start)
    }
}

impl<T: Indexable + ?Sized> Clone for Index<T>
where
    T::Store: Clone,
{
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}
impl<T: Indexable + ?Sized> Debug for Index<T>
where
    T::Store: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}
