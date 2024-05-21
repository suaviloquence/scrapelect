use std::{cell::UnsafeCell, fmt::Debug, marker::PhantomData, num::NonZeroUsize};

#[derive(Debug, Clone)]
pub struct ArenaInner<T> {
    inner: Vec<T>,
    /// we use 1-indexed refs for null-pointer optimization
    next_ref: NonZeroUsize,
}

#[derive(Debug, Clone)]
pub struct Arena<'a, T>(ArenaInner<T>, PhantomData<&'a ()>);

#[derive(Debug)]
pub struct ArenaBuilder<'a, T>(UnsafeCell<Arena<'a, T>>);

#[derive(Debug)]
pub struct Ref<'a, T>(NonZeroUsize, PhantomData<&'a T>);

/// explicit impl because we want it even when `T` is not Clone
impl<'a, T> Clone for Ref<'a, T> {
    fn clone(&self) -> Self {
        Self(self.0, PhantomData)
    }
}

impl<'a, T> Copy for Ref<'a, T> {}

impl<'a, T> Arena<'a, T> {
    #[inline]
    #[must_use]
    pub const fn new() -> Self {
        Self(
            ArenaInner {
                inner: Vec::new(),
                // safety: 1 is nonzero
                next_ref: unsafe { NonZeroUsize::new_unchecked(1) },
            },
            PhantomData,
        )
    }

    #[inline]
    #[must_use]
    pub const fn builder() -> ArenaBuilder<'a, T> {
        ArenaBuilder(UnsafeCell::new(Self::new()))
    }

    #[inline]
    #[must_use]
    pub fn get<'s>(&'s self, r: Ref<'s, T>) -> &'s T
    where
        'a: 's,
    {
        &self.0.inner[r.0.get() - 1]
    }
}

impl<'a, T> ArenaBuilder<'a, T> {
    pub fn insert<'s>(&'s self, item: T) -> Ref<'a, T> {
        // Safety: there can be no direct references to items in `inner`
        // because `ArenaBuilder` does not expose this functionality,
        // and always starts empty in [`Arena::builder`]
        //
        // Any *indirect* references (i.e., [`Ref`]s) will still be valid,
        // because we are just *appending* to the internal vector
        // Thus any previous references vec[i] will still refer to the same
        // data after the append.
        //
        // Additionally, because `ArenaBuilder` contains `UnsafeCell`, it is not
        // `Send` or `Sync`, so this can only happen on one thread.
        unsafe {
            let this = &mut *self.0.get();
            let r = this.0.next_ref;
            this.0.next_ref = this
                .0
                .next_ref
                .checked_add(1)
                .expect("maximum arena size reached");

            this.0.inner.push(item);
            Ref(r, PhantomData)
        }
    }

    #[must_use]
    #[inline]
    pub fn build(self) -> Arena<'a, T> {
        self.0.into_inner()
    }
}

#[cfg(test)]
mod tests {
    use super::Arena;

    #[test]
    fn test_compiles() {
        let builder = Arena::<u8>::builder();
        let r1 = builder.insert(1);
        let r2 = builder.insert(2);
        let arena = builder.build();
        assert_eq!(*arena.get(r1), 1);
        assert_eq!(*arena.get(r2), 2);
    }

    #[test]
    fn test_no_copy() {
        #[derive(Debug, PartialEq, Eq)]
        struct NoCopy(u8);

        let builder = Arena::builder();
        let r1 = builder.insert(NoCopy(1));
        let r2 = builder.insert(NoCopy(2));
        let arena = builder.build();

        assert_eq!(&NoCopy(1), arena.get(r1));
        assert_eq!(&NoCopy(2), arena.get(r2));
    }
}
