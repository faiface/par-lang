use std::collections::BTreeMap;
use std::fmt::Debug;
use std::sync::OnceLock;

use crate::runtime::new::show::{Showable, Shower};

use super::runtime::{Global, Package};

#[derive(Default)]
/// The `Arena` is a store for values from a finite set of types,
/// and returns indices into the arena. Allocation is done using [`Arena::alloc`],
/// and values can be accessed later with [`Arena::get`]
pub struct Arena {
    nodes: Vec<Global>,
    strings: String,
    packages: Vec<OnceLock<Package>>,
    redexes: Vec<(Index<Global>, Index<Global>)>,
    case_branches: Vec<(Index<str>, OnceLock<Package>)>,
    string_to_location: BTreeMap<String, Index<str>>,
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
    pub fn memory_size(&self) -> usize {
        self.nodes.len() * size_of::<Global>()
            + self.strings.len()
            + self.packages.len() * size_of::<OnceLock<Package>>()
            + self.redexes.len() * size_of::<(Global, Global)>()
            + self.case_branches.len() * size_of::<(Index<str>, OnceLock<Package>)>()
    }
    pub fn empty_string(&self) -> Index<str> {
        Index((0, 0))
    }
    pub fn intern(&mut self, s: &str) -> Index<str> {
        if s.is_empty() {
            self.empty_string()
        } else if let Some(s) = self.string_to_location.get(s) {
            s.clone()
        } else {
            let i = self.alloc_clone(s);
            self.string_to_location.insert(s.to_string(), i.clone());
            i
        }
    }
    pub fn interned(&self, s: &str) -> Option<Index<str>> {
        if s.is_empty() {
            Some(self.empty_string())
        } else {
            self.string_to_location.get(s).cloned()
        }
    }
}

impl std::fmt::Display for Arena {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let shower = Shower::from_arena(self);
        for (idx, package) in self.packages.iter().enumerate() {
            let Some(lock) = package.get() else {
                write!(f, "@{} = <unfilled>\n", idx)?;
                continue;
            };
            if lock.debug_name.len() > 0 {
                write!(f, "// {}\n", lock.debug_name)?;
            }
            write!(f, "@{} = {}", idx, Showable(&lock.root, &shower))?;
            write!(f, "\n    $ {}", Showable(&lock.captures, &shower))?;
            for (a, b) in self.get(lock.redexes.clone()) {
                write!(
                    f,
                    "\n     {} ~ {}",
                    Showable(a, &shower),
                    Showable(b, &shower)
                )?;
            }
            write!(f, "\n")?;
        }
        Ok(())
    }
}

pub struct Index<T: Indexable + ?Sized>(pub T::Store);

impl<T: Indexable + ?Sized> Copy for Index<T> {}

/// The `Indexable` trait is implemented by all types that are contained by an `Arena`.
/// It defines a [`Indexable::Store`] associated type, which determines what is needed to
/// index into a value of this type. For example, sized values usually require a `usize` to index them,
/// which represents the offset into the array that contains it
/// but a slice type requires a pair of offset and length.
pub trait Indexable {
    type Store: Copy + PartialEq + Eq + PartialOrd + Ord;
    fn get<'s>(store: &'s Arena, index: Index<Self>) -> &'s Self;
    fn alloc<'s>(store: &'s mut Arena, data: Self) -> Index<Self>
    where
        Self: Sized;
    fn alloc_clone<'s>(_store: &'s mut Arena, _data: &Self) -> Index<Self> {
        todo!()
    }
}

macro_rules! slice_indexable {
    ($field:ident, $element:ty) => {
        impl Indexable for [$element] {
            type Store = (usize, usize);
            fn get<'s>(store: &'s Arena, index: Index<Self>) -> &'s Self {
                &store.$field[index.0 .0..index.0 .0 + index.0 .1]
            }
            fn alloc_clone<'s>(store: &'s mut Arena, data: &Self) -> Index<Self> {
                let start = store.$field.len();
                store.$field.extend_from_slice(data);
                Index((start, data.len()))
            }
        }
    };
}
macro_rules! sized_indexable {
    ($field:ident, $element:ty) => {
        impl Indexable for $element {
            type Store = usize;
            fn get<'s>(store: &'s Arena, index: Index<Self>) -> &'s Self {
                &store.$field[index.0]
            }
            fn alloc<'s>(store: &'s mut Arena, data: Self) -> Index<Self> {
                let start = store.$field.len();
                store.$field.push(data);
                Index(start)
            }
        }
    };
}
slice_indexable!(case_branches, (Index<str>, OnceLock<Package>));
slice_indexable!(nodes, Global);
slice_indexable!(redexes, (Index<Global>, Index<Global>));
sized_indexable!(case_branches, (Index<str>, OnceLock<Package>));
sized_indexable!(packages, OnceLock<Package>);
sized_indexable!(nodes, Global);

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

impl<T: Indexable + ?Sized> PartialEq for Index<T>
where
    T::Store: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        self.0.eq(&other.0)
    }
    fn ne(&self, other: &Self) -> bool {
        self.0.ne(&other.0)
    }
}
impl<T: Indexable + ?Sized> Eq for Index<T> where T::Store: Eq {}

impl<T: Indexable + ?Sized> PartialOrd for Index<T>
where
    T::Store: PartialOrd,
{
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.0.partial_cmp(&other.0)
    }
}
impl<T: Indexable + ?Sized> Ord for Index<T>
where
    T::Store: Ord,
{
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.cmp(&other.0)
    }
}
