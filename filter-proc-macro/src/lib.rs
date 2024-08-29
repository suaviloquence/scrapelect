use proc_macro::{Span, TokenStream};
use quote::{quote, ToTokens};
use syn::{
    punctuated::Punctuated, Data, DeriveInput, GenericParam, Lifetime, LifetimeParam, Pat, PatIdent,
};

/// Procedural macro to derive [`scrapelect_filter_types::Args`] on a structure.
///
/// If you need to use the value lifetime, use `'doc`, otherwise the generator will get confused.
///
/// # Panics
/// `#[derive(Args)]` must be called on a *valid* structure with named fields (not a tuple struct).
/// If not, it will panic and fail.
#[proc_macro_derive(Args)]
pub fn derive_args(input: TokenStream) -> TokenStream {
    let ast = syn::parse(input).expect("token stream should be valid");

    derive_args_impl(&ast)
}

fn derive_args_impl(ast: &DeriveInput) -> TokenStream {
    let name = &ast.ident;

    // add 'doc if it is not already present
    let generics = &ast.generics;
    let (_, ty_generics, where_clause) = generics.split_for_impl();
    let mut added_generics = generics.clone();
    if !generics.params.iter().any(|x| match x {
        GenericParam::Lifetime(lt) => lt.lifetime.ident == "doc",
        _ => false,
    }) {
        added_generics
            .params
            .push(GenericParam::Lifetime(LifetimeParam {
                attrs: vec![],
                bounds: Punctuated::new(),
                colon_token: None,
                lifetime: Lifetime::new("'doc", Span::call_site().into()),
            }));
    }

    let (impl_generics, _, _) = added_generics.split_for_impl();

    let Data::Struct(s) = &ast.data else {
        return quote! {
            compile_error!("#[derive(Args)] on a non-struct is not supported.");
        }
        .into();
    };

    let field = s.fields.iter().map(|x| {
        if let Some(id) = &x.ident {
            id
        } else {
            panic!("#[derive(Args)] not supported on a tuple struct")
        }
    });

    let field_extract = field
        .clone()
        .filter(|x| !x.to_string().starts_with("_marker"));

    let field_assign = field.clone().map(|x| {
        if x.to_string().starts_with("_marker") {
            quote! { #x: Default::default() }
        } else {
            quote! { #x }
        }
    });

    quote! {
        impl #impl_generics scrapelect_filter_types::Args<'doc> for #name #ty_generics #where_clause {
            fn try_deserialize<'ast>(
                mut args: ::std::collections::BTreeMap<&'ast str, scrapelect_filter_types::EValue<'doc>>
            ) -> scrapelect_filter_types::Result<Self> {
                #(
                    let #field_extract = scrapelect_filter_types::TryFromValue::try_from_option(args.remove(stringify!(#field_extract)))?;
                )*

                if !args.is_empty() {
                    scrapelect_filter_types::bail!("found unexpected arguments {args:?}");
                }

                Ok(Self {
                    #(#field_assign),*
                })
            }
        }
    }
    .into()
}

/// Procedural macro that makes a function that generates a stateless `impl Filter`
/// filter using the function body as the method for `apply`.
///
/// Parameter conversion: We generate an `impl Args<'doc>` struct using the parameters
/// to the function.  The value/ctx lifetime must be `'doc` for compatibility with
/// `#[derive(Args)]`.  Here is how the arguments are converted
///
/// - `value: T` - this must be present, and `T` must be `TryFromValue<'doc>`
/// - `ctx: ElementContext<'_, 'doc>`: this is optionally present.  If it is present, it must have the given type.
/// - `...other_arg: T`: For all other args, they will be put in `Self::Args`, and `T` must be `TryFromValue<'doc>`.
///
/// Note that patterns are not supported beyond `(mut)? x: T`
///
/// The return type must be `scrapelect_filter_types::Result<PValue<'doc>>`
///
/// # Panics
/// Panics if the token stream is not valid or the function signature is not as specified.
#[proc_macro_attribute]
pub fn filter_fn(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let func: syn::ItemFn = syn::parse(item).expect("token stream should be valid");
    let inner = func.clone();
    let name = func.sig.ident;
    let vis = func.vis;
    let attrs = func.attrs;

    let (value, args) = func
        .sig
        .inputs
        .into_iter()
        .map(|arg| match arg {
            syn::FnArg::Receiver(_) => panic!("Calling #[filter_fn] on a method"),
            syn::FnArg::Typed(x) => match *x.pat {
                Pat::Ident(PatIdent {
                    ident,
                    subpat: None,
                    ..
                }) => (ident, x.ty),
                other => panic!("I don't know what to do with pattern {other:?}"),
            },
        })
        .partition::<Vec<_>, _>(|(ident, _)| ident == "value");
    let (ctx, args) = args
        .into_iter()
        .partition::<Vec<_>, _>(|(ident, _)| ident == "ctx");

    let [(value, vty)]: [_; 1] = value.try_into().expect("expected exactly 1 value arg");

    let arg = args.iter().map(|(id, _)| id);
    let ty = args.iter().map(|(_, ty)| ty);

    let (ctx, _cty) = if let Some(x) = ctx.into_iter().next() {
        (Some(x.0), Some(x.1))
    } else {
        (None, None)
    };

    let call_args = std::iter::once(value.clone().into_token_stream())
        .chain(arg.clone().map(|arg| quote! {args.#arg}))
        .chain(ctx.clone().into_iter().map(|x| quote! {#x }));

    quote! {
        #(#attrs)*
        #vis fn #name() -> impl scrapelect_filter_types::Filter {
            #[derive(Debug, scrapelect_filter_types::Args)]
            pub struct Args<'doc> {
                _marker: core::marker::PhantomData<&'doc ()>,
                #(#arg: #ty),*
            }

            #[derive(Debug)]
            pub struct Filter;

            impl scrapelect_filter_types::Filter for Filter {
                type Args<'doc> = Args<'doc>;
                type Value<'doc> = #vty;

                fn apply<'ast, 'ctx, E: scrapelect_filter_types::ElementContextView<'ast, 'ctx> + ?Sized>(
                    #value: Self::Value<'ctx>,
                    args: Self::Args<'ctx>,
                    #[allow(unused)]
                    ctx: &mut E,
                ) -> scrapelect_filter_types::Result<scrapelect_filter_types::PValue<'ctx>> {
                    // we can't elide the 'doc lifetime here because it needs to
                    // also be in the struct, unless we make a smarter macro
                    // (i.e., lifetime-aware)
                    #[allow(clippy::needless_lifetimes, clippy::needless_pass_by_value, clippy::unnecessary_wraps)]
                    #inner

                    #name (#(#call_args),*)
                }
            }

            Filter
        }
    }
    .into()
}
