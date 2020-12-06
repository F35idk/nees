use proc_macro::*;

#[proc_macro_derive(Serialize)]
pub fn derive_serialize(stream: TokenStream) -> TokenStream {
    let ast = syn::parse_macro_input!(stream as syn::DeriveInput);
    let type_name = ast.ident;

    let output = if let syn::Data::Struct(s) = ast.data {
        match s.fields {
            syn::Fields::Named(named) => {
                let field_names: Vec<_> = named
                    .named
                    .into_iter()
                    .map(|field| field.ident.unwrap())
                    .collect();

                quote::quote! {
                    impl crate::serialize::Serialize for #type_name {
                        fn serialize(
                            &self,
                            file: &mut ::std::io::BufWriter<::std::fs::File>
                        ) -> Result<(), String> {
                            #( self.#field_names.serialize(file)?; )*
                            Ok(())
                        }

                        fn deserialize(
                            &mut self,
                            file: &mut ::std::io::BufReader<::std::fs::File>
                        ) -> Result<(), String> {
                            #( self.#field_names.deserialize(file)?; )*
                            Ok(())
                        }
                    }
                }
            }
            syn::Fields::Unnamed(unnamed) => {
                let (field_indices1, field_indices2) = {
                    let indices = 0..unnamed.unnamed.len();
                    (
                        indices.clone().map(syn::Index::from),
                        indices.clone().map(syn::Index::from),
                    )
                };

                quote::quote! {
                    impl crate::serialize::Serialize for #type_name {
                        fn serialize(
                            &self,
                            file: &mut ::std::io::BufWriter<::std::fs::File>
                        ) -> Result<(), String> {
                            #( self.#field_indices1.serialize(file)?; )*
                            Ok(())
                        }

                        fn deserialize(
                            &mut self,
                            file: &mut ::std::io::BufReader<::std::fs::File>
                        ) -> Result<(), String> {
                            #( self.#field_indices2.deserialize(file)?; )*
                            Ok(())
                        }
                    }
                }
            }
            syn::Fields::Unit => panic!(),
        }
    } else {
        panic!("Serialize can only be derived for structs")
    };

    output.into()
}
