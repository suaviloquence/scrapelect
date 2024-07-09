use proc_macro::{Span, TokenStream};
use quote::quote;
use syn::{punctuated::Punctuated, Data, DeriveInput, GenericParam, Lifetime, LifetimeParam};

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

    let Some(field) = &s
        .fields
        .iter()
        .map(|x| x.ident.as_ref())
        .collect::<Option<Vec<_>>>()
    else {
        return quote! {
            compile_error!("#[derive(Args)] is not supported on tuple structs.");
        }
        .into();
    };

    quote! {
        impl #impl_generics crate::interpreter::filter::Args<'doc> for #name #ty_generics #where_clause {
            fn try_deserialize<'ast>(
                mut args: ::std::collections::BTreeMap<&'ast str, crate::interpreter::Value<'doc>>
            ) -> anyhow::Result<Self> {
                #(
                    let #field = crate::interpreter::TryFromValue::try_from_option_value(args.remove(stringify!(#field)))?;
                )*

                if !args.is_empty() {
                    anyhow::bail!("Found unexpected arguments {args:?}");
                }

                Ok(Self { #(#field),* })
            }
        }
    }
    .into()
}
