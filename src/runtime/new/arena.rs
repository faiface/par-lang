use std::alloc::Layout;
use std::marker::PhantomData;
use std::ptr::NonNull;
use std::sync::{Arc, RwLock, Weak};

#[derive(Clone)]
pub struct Index<'brand, T> {
    phantom: PhantomData<(T, &'brand mut ())>,
    index: usize,
}

#[derive(Clone)]
pub struct IndexSlice<'brand, T> {
    phantom: PhantomData<(T, &'brand mut ())>,
    index: usize,
    len: usize,
}

#[derive(Clone)]
pub struct IndexStr<'brand> {
    phantom: PhantomData<&'brand mut ()>,
    index: usize,
    len: usize,
}

pub struct IndexMut<'brand, T> {
    phantom: PhantomData<(T, &'brand mut ())>,
    index: usize,
}

struct ArenaInner {
    start: NonNull<u8>,
    length: usize,
    capacity: usize,
    align: usize,
}

impl ArenaInner {
    unsafe fn set_at<T>(&mut self, index: usize, data: T) {
        self.start.add(index).cast::<T>().write(data);
    }
    unsafe fn get_at<T>(&self, index: usize) -> *const T {
        self.start.add(index).cast::<T>().as_ptr()
    }
    unsafe fn get_at_mut<T>(&self, index: usize) -> *mut T {
        self.start.add(index).cast::<T>().as_ptr()
    }
    unsafe fn allocate_with_layout(&mut self, layout: Layout) -> usize {
        if layout.align() > self.align {
            // not enough alignment
            let new_align = layout.align_to(self.align).unwrap();
            self.realloc(self.capacity, new_align.align());
            self.allocate_with_layout(layout);
        }
        let layout = layout.pad_to_align();
        let next_index = self.length.next_multiple_of(layout.align());
        if next_index + layout.size() > self.capacity {
            // not enough space
            self.realloc(self.capacity * 2, self.align);
            self.allocate_with_layout(layout);
        }
        next_index
    }
    fn layout(&self) -> Layout {
        Layout::from_size_align(self.capacity, self.align).unwrap()
    }
    unsafe fn new(capacity: usize, align: usize) -> Self {
        let layout = Layout::from_size_align(capacity, align).unwrap();
        let start = std::alloc::alloc(layout);
        Self {
            start: NonNull::new(start).unwrap(),
            length: 0,
            capacity,
            align,
        }
    }
    unsafe fn dealloc(self) {
        std::alloc::dealloc(self.start.as_ptr(), self.layout());
    }
    unsafe fn realloc(&mut self, capacity: usize, align: usize) {
        if align != self.align {
            let old = core::mem::replace(self, Self::new(capacity, align));
            old.move_to(self);
        } else {
            let old_layout = Layout::from_size_align(self.capacity, self.align).unwrap();
            let new_start = std::alloc::realloc(self.start.as_ptr(), old_layout, capacity);
            self.capacity = capacity;
            self.start = NonNull::new(new_start).unwrap();
        }
    }
    unsafe fn move_to(self, destination: &mut ArenaInner) {
        std::ptr::copy_nonoverlapping(self.start.as_ptr(), destination.start.as_ptr(), self.length);
        destination.length = self.length;
        self.dealloc();
    }
}

pub struct IndexedArena<'brand> {
    phantom: PhantomData<&'brand mut ()>,
    inner: ArenaInner,
}

#[derive(Clone)]
pub struct FrozenIndexedArena<'brand> {
    phantom: PhantomData<&'brand mut ()>,
    inner: Arc<ArenaInner>,
}

struct ExternalIndex<T> {
    phantom: PhantomData<*const T>,
    weak: Weak<ArenaInner>,
    index: usize,
}

impl<'brand> IndexedArena<'brand> {
    fn new() -> Self {
        Self {
            phantom: PhantomData,
            inner: unsafe { ArenaInner::new(0, 16) },
        }
    }
    fn alloc<T>(&mut self, data: T) -> IndexMut<'brand, T> {
        let layout = std::alloc::Layout::for_value(&data);
        let index = unsafe {
            let index = self.inner.allocate_with_layout(layout);
            self.inner.set_at(index, data);
            index
        };
        IndexMut {
            phantom: PhantomData,
            index,
        }
    }
    pub fn get<'s, T>(&'s self, index: Index<'brand, T>) -> &'s T {
        unsafe { self.inner.get_at::<T>(index.index).as_ref().unwrap() }
    }
    pub fn get_slice<'s, T>(&'s self, index: IndexSlice<'brand, T>) -> &'s [T] {
        unsafe { core::slice::from_raw_parts(self.inner.get_at::<T>(index.index), index.len) }
    }
    fn get_mut<'s, T>(&'s self, index: IndexMut<'brand, T>) -> &'s mut T {
        unsafe { self.inner.get_at_mut::<T>(index.index).as_mut().unwrap() }
    }
    fn freeze(self) -> FrozenIndexedArena<'brand> {
        FrozenIndexedArena {
            phantom: PhantomData,
            inner: Arc::new(self.inner),
        }
    }
}

impl<'brand> FrozenIndexedArena<'brand> {
    fn get<'s, T>(&'s self, index: Index<'brand, T>) -> &'s T {
        unsafe { self.inner.get_at::<T>(index.index).as_ref().unwrap() }
    }
    fn externalize<'s, T>(&'s self, index: Index<'brand, T>) -> ExternalIndex<T> {
        ExternalIndex {
            phantom: PhantomData,
            weak: Arc::downgrade(&self.inner),
            index: index.index,
        }
    }
}
