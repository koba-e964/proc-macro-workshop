extern crate proc_macro;

use proc_macro::TokenStream;
use quote::quote;
use syn::export::Span;
use syn::Data;
use syn::{Ident, Type};

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let derive_input = syn::parse_macro_input!(input as syn::DeriveInput);

    let ident = derive_input.ident;

    let builder_ident = Ident::new(&format!("{}Builder", ident), Span::call_site());

    // This macro doesn't make sense if the input is not `struct`.
    // Enumerates all declarations of form `varname: typename` and emits `varname: Option<typename>`.
    let builder_members = match derive_input.data {
        Data::Struct(members) => {
            let mut members = members.clone();
            members.fields.iter_mut().for_each(|field| {
                let ty = field.ty.clone();
                field.ty = Type::Verbatim(quote! {
                    Option<#ty>
                })
            });
            members.fields
        }
        _ => panic!("Only structs can have derive(Builder) implementation"),
    };

    let builder_field_names = builder_members.iter().map(|field| {
        field
            .ident
            .clone()
            .expect("Only named structs are expected")
    });

    let build_impl = quote! {
        struct #builder_ident
            #builder_members
        impl #ident {
            fn builder() -> #builder_ident {
                #builder_ident {
                    #(#builder_field_names: None,)*
                }
            }
        }
    };
    build_impl.into()
}
