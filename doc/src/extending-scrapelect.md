# Extending Scrapelect

*Note: plugin/dylib loading is not yet implemented (see
[#32](https://github.com/suaviloquence/scrapelect/issues/32) for tracking).
This documents the process of writing a filter, which is also applicable for
creating new builtin filters*

## Writing a new filter

The easiest way to write a new filter is with the
[`#[filter_fn]`](https://docs.rs/scrapelect-filter-proc-macro/latest/scrapelect_filter_proc_macro/attr.filter_fn.html).
attribute macro.

```rust
/// Signature: value: ValueT | filter_name(arg: ArgT, ...): ReturnT
///
/// Description of what this filter does.
///
/// # Examples
///
/// It's helpful to include a list of examples here, with their outputs/effects.
pub fn filter_name<'doc>(
    value: RustValueT,
    ctx: &mut ElementContext<'_, 'doc> // this can be omitted if you don't need it
    arg: RustArgT,
    ...
) -> Result<PValue<'doc>> {
    todo!()
}
```

The `Rust*T`s must implement
[`TryFromValue<Pipeline>`](https://docs.rs/scrapelect/latest/scrapelect/interpreter/value/trait.TryFromValue.html)
which allows type validation and automatic `Value` unwrapping.  The return value must be
rewrapped into `Value::Type`.

With the `#[filter_fn]` proc_macro, this function will be transformed into a
`fn() -> impl FilterDyn`, which is the object-safe trait that represents a filter
call.

### Registering a filter

*TODO: this is not implemented because there is no dynamic loading.*

To add a built-in filter in the `scrapelect` crate itself, add it to the `build_map!`
macro in the `interpreter::filter::builtin` module.

### implementing `Filter` manually

[`Filter`](https://docs.rs/scrapelect/latest/scrapelect/interpreter/filter/trait.Filter.html)
is the non-object-safe trait that has typed `Value` and `Args` types.  Its inherent
function, `Filter::apply`, takes a `Self::Value`, `Self::Args` and `&mut ElementContext`
and returns a `Result<PValue>`.  Often, deriving the `Args` trait is sufficient to
specify arguments, but for finer-grained control, you can implement
[`Args`](https://docs.rs/scrapelect/latest/scrapelect/interpreter/filter/trait.Args.html)
manually, which tries to deserialize `Self` from a `BTreeMap<&str, EValue>`.  If you
need more expressivity in arguments (e.g., for variadic functions), you may have
to implement this trait manually.

### Implementing `FilterDyn` manually

All `Filter`s implement the [`FilterDyn`](https://docs.rs/scrapelect/latest/scrapelect/interpreter/filter/trait.FilterDyn.html)
trait, which is the object-safe trait used for dynamic filter dispatch.  It is
often enough to not need to manually implement `FilterDyn`, but it may
sometimes be necessary. Because `FilterDyn` takes an `&self`, it is possible to
have filter state, but consider deeply whether this is truly necessary, as filters
can be called from anywhere, so you must reason the soundness of your filter state.

All `FilterDyn`s registed with `scrapelect`'s filter dispatch must also be `Send`,
`Sync`, and `'static`.
