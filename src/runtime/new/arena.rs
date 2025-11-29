use std::cell::OnceCell;
use std::fmt::Debug;

use super::runtime::{Global, Package};

pub trait Indexable {
    type Store: Clone;
    fn get<'s>(store: &'s Arena, index: Index<Self>) -> &'s Self;
    fn alloc<'s>(store: &'s mut Arena, data: Self) -> Index<Self>
    where
        Self: Sized;
    fn alloc_clone<'s>(store: &'s mut Arena, data: &Self) -> Index<Self> {
        todo!()
    }
}

impl Indexable for [Global] {
    type Store = (usize, usize);
    fn get<'s>(store: &'s Arena, index: Index<Self>) -> &'s Self {
        &store.nodes[index.0 .0..index.0 .1]
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
        &store.strings[index.0 .0..index.0 .1]
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
impl Indexable for OnceCell<Package> {
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

pub struct Index<T: Indexable + ?Sized>(T::Store);
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

#[derive(Default)]
pub struct Arena {
    nodes: Vec<Global>,
    strings: String,
    packages: Vec<OnceCell<Package>>,
}

impl Arena {
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
