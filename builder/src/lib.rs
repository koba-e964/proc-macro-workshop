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
    let orig_fields = match derive_input.data {
        Data::Struct(members) => {
            let members = members.clone();
            members.fields
        }
        _ => panic!("Only structs can have derive(Builder) implementation"),
    };
    let mut builder_fields = orig_fields.clone();
    builder_fields.iter_mut().for_each(|field| {
        let ty = field.ty.clone();
        field.ty = Type::Verbatim(quote! {
            Option<#ty>
        })
    });

    let builder_field_names: Vec<_> = builder_fields
        .iter()
        .map(|field| {
            field
                .ident
                .clone()
                .expect("Only named structs are expected")
        })
        .collect();

    let builder_member_inits = orig_fields.iter().map(|field| {
        let ident = field
            .ident
            .clone()
            .expect("Only named structs are expected");
        let ty = field.ty.clone();
        quote! {
            fn #ident(&mut self, #ident: #ty) -> &mut Self {
                self.#ident = std::option::Option::Some(#ident);
                self
            }
        }
    });

    let build_impl = quote! {
        struct #builder_ident
            #builder_fields
        impl #builder_ident {
            #(#builder_member_inits)*
            fn build(&mut self) -> ::std::result::Result<#ident, ::std::boxed::Box<dyn ::std::error::Error>> {
                #(let #builder_field_names = match self.#builder_field_names.take() {
                    ::std::option::Option::Some(value) => value,
                    ::std::option::Option::None => return ::std::result::Result::Err(stringify!(#builder_field_names).into()),
                };)*
                ::std::result::Result::Ok(#ident {
                    #(#builder_field_names: #builder_field_names,)*
                })
            }
        }
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
