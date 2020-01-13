extern crate proc_macro;

use proc_macro::TokenStream;
use quote::quote;
use std::collections::HashMap;
use syn::export::Span;
use syn::{
    parse_str, AngleBracketedGenericArguments, Data, Error, Fields, GenericArgument, Meta,
    NestedMeta, Path, PathArguments, PathSegment, Result, TypePath,
};
use syn::{Ident, Lit, Type};

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

// Check if ty is of form ???<T[0], T[1], ...> and returns Some(T[pos]) if so.
fn get_generic_type_parameter(ty: &Type, pos: usize) -> Option<&Type> {
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
    let args = if let PathSegment {
        arguments: PathArguments::AngleBracketed(AngleBracketedGenericArguments { args, .. }),
        ..
    } = &segments[0]
    {
        args
    } else {
        return None;
    };
    if pos >= args.len() {
        return None;
    }
    match &args[pos] {
        GenericArgument::Type(ref ty) => Some(ty),
        _ => None,
    }
}

// Checks if path is `name`. The leading Colon2 is not relevant in the comparison.
fn path_is_equal_to_str(path: &Path, name: &str) -> bool {
    let ref segments = path.segments;
    if segments.len() != 1 {
        return false;
    }
    let ref segment = segments[0];
    segment.ident == name
}

// For each field annotated with `#[builder(each = name)]`, makes an entry (field_name => (name, ty)).
fn make_each_map(fields: &Fields) -> Result<HashMap<Ident, (Ident, Type)>> {
    let mut map: HashMap<_, (_, Type)> = HashMap::new();
    for field in fields.iter() {
        let ref ident = field.ident.clone().unwrap();
        let ref attrs = field.attrs;
        let mut each_arg = None;
        for attr in attrs {
            let whole_meta = attr.parse_meta()?;
            let _args: Meta = attr.parse_args()?;
            match whole_meta {
                Meta::List(ref x) => {
                    // Check if `x.path` is `builder`.
                    if path_is_equal_to_str(&x.path, "builder") {
                        // Hit
                        let ref nested = x.nested;
                        for meta_x in nested {
                            // search for `each`
                            if let NestedMeta::Meta(name_value) = meta_x {
                                if let Meta::NameValue(meta) = name_value {
                                    if path_is_equal_to_str(&meta.path, "each") {
                                        // we've found `#[builder(each = ...)]` so far.
                                        // To complete we must check ... is a string literal.
                                        if let Lit::Str(ref s) = meta.lit {
                                            each_arg = Some(s.value());
                                        }
                                    } else {
                                        // Compile error
                                        return Err(Error::new_spanned(
                                            whole_meta.clone(),
                                            "expected `builder(each = \"...\")`",
                                        ));
                                    }
                                }
                            }
                        }
                    }
                }
                _ => (),
            }
        }
        if let Some(each_arg) = each_arg {
            let each_arg_ident =
                parse_str::<Ident>(&each_arg).expect("each doesn't have a valid identifier");
            let contained_type = get_generic_type_parameter(&field.ty, 0)
                .expect("Vec<...> was expected")
                .clone();
            map.insert(ident.clone(), (each_arg_ident, contained_type));
        }
    }
    Ok(map)
}

fn derive_impl(derive_input: syn::DeriveInput) -> Result<TokenStream> {
    let ident = derive_input.ident;

    let builder_ident = Ident::new(&format!("{}Builder", ident), Span::call_site());

    // The whole thing doesn't make sense if the input is not `struct`.
    let orig_fields = match derive_input.data {
        Data::Struct(members) => {
            let members = members.clone();
            members.fields
        }
        _ => panic!("Only structs can have derive(Builder) implementation"),
    };

    let each_map = make_each_map(&orig_fields)?;

    // Enumerates all declarations of form `varname: typename` and emits `varname: Option<typename>`.
    // With two exceptions.
    let builder_fields = orig_fields.iter().map(|field| {
        let ref ident = field.ident.as_ref().unwrap();
        let ty = field.ty.clone();
        let builder_field_ty = match (each_map.get(ident), detect_option_type(&ty)) {
            (Some(_), None) => ty,
            (_, Some(_)) => ty,
            (None, None) => Type::Verbatim(quote! {
                Option<#ty>
            }),
        };
        quote! {
            #ident: #builder_field_ty,
        }
    });

    let builder_field_names: Vec<_> = orig_fields
        .iter()
        .map(|field| {
            field
                .ident
                .clone()
                .expect("Only named structs are expected")
        })
        .collect();

    let builder_each_assigners = orig_fields.iter().map(|field| {
        let ref ident = field.ident.as_ref().unwrap();
        if let Some((each_arg, each_ty)) = each_map.get(ident) {
            quote! {
                fn #each_arg(&mut self, #ident: #each_ty) -> &mut Self {
                    self.#ident.push(#ident);
                    self
                }
            }
        } else {
            quote!()
        }
    });

    let builder_member_inits = orig_fields.iter().map(|field| {
        let ref ident = field
            .ident
            .as_ref()
            .expect("Only named structs are expected");
        let ty = field.ty.clone();
        // If ty is Option<_>, unwrap Option once.
        let ty = match (each_map.get(ident), detect_option_type(&ty)) {
            // We handle [each] case separately, because it's hard to handle it with the latter case.
            (Some((each_arg, _)), None) => {
                return if each_arg != *ident {
                    quote! {
                        fn #ident(&mut self, #ident: #ty) -> &mut Self {
                            self.#ident = #ident;
                            self
                        }
                    }
                } else {
                    quote!()
                };
            }
            (_, Some(ty)) => ty,
            (None, None) => &ty,
        };
        quote! {
            fn #ident(&mut self, #ident: #ty) -> &mut Self {
                self.#ident = std::option::Option::Some(#ident);
                self
            }
        }
    });

    let build_fields_init: Vec<_> = orig_fields.iter().map(|field| {
        let ref ident = field
            .ident
            .as_ref()
            .expect("Only named structs are expected");
        let ty = field.ty.clone();
        let field_name = field.ident.clone();
        match (each_map.get(ident), detect_option_type(&ty)) {
            (Some(_), None) |
            (_, Some(_)) => quote! {
                // In this case we handle both Vec<_> and Option<_>.
                // Default::default returns a value for both cases, with no trait bounds.
                let #field_name = ::std::mem::replace(&mut self.#field_name, ::std::default::Default::default());
            },
            (None, None) => quote! {
                let #field_name = match self.#field_name.take() {
                    ::std::option::Option::Some(value) => value,
                    ::std::option::Option::None => return ::std::result::Result::Err(stringify!(#field_name).into()),
                };
            },
        }
    }).collect();

    let build_impl = quote! {
        struct #builder_ident {
            #(#builder_fields)*
        }
        impl #builder_ident {
            #(#builder_member_inits)*
            #(#builder_each_assigners)*
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
                    #(#builder_field_names: ::std::default::Default::default(),)*
                }
            }
        }
    };
    Ok(build_impl.into())
}

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let derive_input = syn::parse_macro_input!(input as syn::DeriveInput);
    match derive_impl(derive_input) {
        Ok(output) => output,
        Err(e) => e.to_compile_error().into(),
    }
}
