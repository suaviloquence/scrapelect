pub mod builtin;

use std::collections::BTreeMap;

use scrapelect_filter_types::{bail, ElementContextView};
pub use scrapelect_filter_types::{EValue, ElementContext, Filter, FilterDyn, PValue, Result};

pub fn dispatch_filter<'ast, 'doc, E: ElementContextView<'ast, 'doc>>(
    name: &str,
    value: PValue<'doc>,
    args: BTreeMap<&'ast str, EValue<'doc>>,
    ctx: &mut E,
) -> Result<PValue<'doc>> {
    match builtin::FILTERS.get(name) {
        Some(filter) => filter.apply(value, args, ctx),
        None => bail!("unrecognized filter `{name}`"),
    }
}
