use std::fmt::Debug;
use std::sync::OnceLock;

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
impl<T: Indexable + ?Sized> Index<T> {
    pub fn get<'s>(self, store: &'s Arena) -> &'s T {
        T::get(store, self)
    }
}
