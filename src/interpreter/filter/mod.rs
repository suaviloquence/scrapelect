pub mod builtin;

use std::collections::BTreeMap;

use scrapelect_filter_types::bail;
pub use scrapelect_filter_types::{EValue, ElementContext, Filter, FilterDyn, PValue, Result};

pub fn dispatch_filter<'ast, 'doc>(
    name: &str,
    value: PValue<'doc>,
    args: BTreeMap<&'ast str, EValue<'doc>>,
    ctx: &mut ElementContext<'ast, 'doc>,
) -> Result<PValue<'doc>> {
    match builtin::FILTERS.get(name) {
        Some(filter) => filter.apply(value, args, ctx),
        None => bail!("unrecognized filter `{name}`"),
    }
}
