extern crate proc_macro;

use proc_macro::TokenStream;
use quote::quote;
use syn::export::Span;
use syn::{
    AngleBracketedGenericArguments, Data, GenericArgument, Path, PathArguments, PathSegment,
    TypePath,
};
use syn::{Ident, Type};

// Check if ty is of form Option<T> and returns Some(T) if so.
fn detect_option_type(ty: &Type) -> Option<&Type> {
    let segments = if let Type::Path(TypePath {
        qself: None,
        path: Path { segments, .. },
    }) = ty
    {
        segments
    } else {
        return None;
    };
    if segments.len() != 1 {
        return None;
    }
    let (option_ident, args) = if let PathSegment {
        ident: option_ident,
        arguments: PathArguments::AngleBracketed(AngleBracketedGenericArguments { args, .. }),
    } = &segments[0]
    {
        (option_ident, args)
    } else {
        return None;
    };
    if option_ident != "Option" {
        return None;
    }
    if args.len() != 1 {
        return None;
    }
    match &args[0] {
        GenericArgument::Type(ref ty) => Some(ty),
        _ => None,
    }
}

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
        let builder_field_ty = match detect_option_type(&ty) {
            Some(_) => ty,
            None => Type::Verbatim(quote! {
                Option<#ty>
            }),
        };
        field.ty = builder_field_ty;
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
        // If ty is Option<_>, unwrap Option once.
        let ty = match detect_option_type(&ty) {
            Some(ty) => ty,
            None => &ty,
        };
        quote! {
            fn #ident(&mut self, #ident: #ty) -> &mut Self {
                self.#ident = std::option::Option::Some(#ident);
                self
            }
        }
    });

    let build_fields_init: Vec<_> = orig_fields.iter().map(|field| {
        let ty = field.ty.clone();
        let field_name = field.ident.clone();
        match detect_option_type(&ty) {
            Some(_) => quote! {
                let #field_name = self.#field_name.take();
            },
            None => quote! {
                let #field_name = match self.#field_name.take() {
                    ::std::option::Option::Some(value) => value,
                    ::std::option::Option::None => return ::std::result::Result::Err(stringify!(#field_name).into()),
                };
            },
        }
    }).collect();

    let build_impl = quote! {
        struct #builder_ident
            #builder_fields
        impl #builder_ident {
            #(#builder_member_inits)*
            fn build(&mut self) -> ::std::result::Result<#ident, ::std::boxed::Box<dyn ::std::error::Error>> {
                #(#build_fields_init)*
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
