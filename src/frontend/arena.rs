use std::{fmt::Debug, marker::PhantomData, num::NonZeroUsize};

#[derive(Debug, Clone)]
pub struct ArenaInner<T> {
    inner: Vec<T>,
    /// we use 1-indexed refs for null-pointer optimization
    next_ref: NonZeroUsize,
}

#[derive(Debug)]
pub struct Arena<T>(ArenaInner<T>);

#[derive(Debug)]
pub struct Ref<'a, T>(NonZeroUsize, PhantomData<&'a T>);

/// explicit impl because we want it even when `T` is not Clone
impl<'a, T> Clone for Ref<'a, T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<'a, T> Copy for Ref<'a, T> {}

impl<T> Default for Arena<T> {
    fn default() -> Self {
        Self::new()
    }
}

/// Hack to get a constant NonZeroUsize(1).  Uses out-of-bounds
/// array access to simulate panicking in a const declaration.
const ONE: NonZeroUsize = match NonZeroUsize::new(1) {
    Some(x) => x,
    None => [][0],
};

impl<T> Arena<T> {
    #[inline]
    #[must_use]
    pub const fn new() -> Self {
        Self(ArenaInner {
            inner: Vec::new(),
            next_ref: ONE,
        })
    }

    #[inline]
    #[must_use]
    pub fn get<'a, 's>(&'s self, r: Ref<'a, T>) -> &'s T
    where
        'a: 's,
    {
        &self.0.inner[r.0.get() - 1]
    }

    pub fn insert<'a, 's>(&'s mut self, item: T) -> Ref<'a, T>
    where
        'a: 's,
    {
        let r = Ref(self.0.next_ref, PhantomData);
        self.0.next_ref = self
            .0
            .next_ref
            .checked_add(1)
            .expect("maximum elements in arena.");
        self.0.inner.push(item);
        r
    }
}

#[cfg(test)]
mod tests {
    use super::Arena;

    #[test]
    fn test_compiles() {
        let mut arena = Arena::<u8>::new();
        let r1 = arena.insert(1);
        let r2 = arena.insert(2);
        assert_eq!(*arena.get(r1), 1);
        assert_eq!(*arena.get(r2), 2);
    }

    #[test]
    fn test_no_copy() {
        #[derive(Debug, PartialEq, Eq)]
        struct NoCopy(u8);

        let mut arena = Arena::new();
        let r1 = arena.insert(NoCopy(1));
        let r2 = arena.insert(NoCopy(2));

        assert_eq!(&NoCopy(1), arena.get(r1));
        assert_eq!(&NoCopy(2), arena.get(r2));
    }
}
