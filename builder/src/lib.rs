extern crate proc_macro;

use proc_macro::TokenStream;

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let _ = syn::parse_macro_input!(input as syn::DeriveInput);

    TokenStream::new()
}
