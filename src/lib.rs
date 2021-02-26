//! This crate provides derive to generate struct with bitfield access.
//!
//! # Examples
//!
//! ```
//! use bitfield_derive::BitFields;
//!
//! #[derive(Default, BitFields)]
//! struct Foo {
//!     #[bitfield(bar : [3:0] as u8 "The bar flags")]
//!     #[bitfield(baz : [7:4] as u8 "The baz flags")]
//!     #[bitfield(ro, _ : [8] as bool)] // Read only
//!     #[bitfield(_ , set_wr : [9] as bool)] // Write only
//!     #[bitfield(stuff : [31:16] as u16)]
//!     #[bitfield(all_bits : [31:0])]
//!     _bi1: u32,
//!     other: usize,
//! }
//!
//! // Instance a struct with bitfields.
//! let mut foo = Foo::default();
//!
//! // Initial states.
//! assert_eq!(foo.bar(), 0);
//! assert_eq!(foo.baz(), 0);
//!
//! foo.set_bar(7);
//! foo.set_baz(3);
//! assert_eq!(foo.bar(), 7);
//! assert_eq!(foo.baz(), 3);
//!
//! // Overflowing tests.
//! foo.set_bar(0x13);
//! foo.set_baz(0x17);
//! assert_eq!(foo.bar(), 3);
//! assert_eq!(foo.baz(), 7);
//!
//! foo.set_bar(0x0f);
//! foo.set_baz(0x0f);
//! assert_eq!(foo.bar(), 0x0f);
//! assert_eq!(foo.baz(), 0x0f);
//!
//! assert_eq!(foo.ro(), false);
//! // Compile fail if uncomment the follow line.
//! // foo.set_ro(false);
//!
//! // Compile fail if uncomment the follow line.
//! // assert_eq!(foo.wr(), 0);
//! foo.set_wr(true);
//!
//! assert_eq!(foo.stuff(), 0);
//! foo.set_stuff(0xffff);
//! assert_eq!(foo.stuff(), 0xffff);
//!
//! // All bits in the container field.
//! assert_eq!(foo.all_bits(), 0xffff_02ff);
//! foo.set_all_bits(0);
//! assert_eq!(foo.all_bits(), 0);
//! ```
use proc_macro::TokenStream;
use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::quote;
use syn::parse::{Parse, ParseStream, Result as ParseResult};
use syn::{Data, DeriveInput, Field, Ident, LitInt, LitStr, Token, Type};

#[derive(Debug)]
struct BitField<'a> {
    parent: Option<&'a Field>,
    getter: Option<Ident>,
    setter: Option<Ident>,
    msb: LitInt,
    lsb: LitInt,
    as_type: Option<Type>,
    doc: Option<LitStr>,
}

impl<'a> BitField<'a> {
    fn codegen(&self, ident: &Ident) -> TokenStream2 {
        let empty_str = LitStr::new("", Span::call_site());
        let getter = &self.getter;
        let setter = &self.setter;
        let msb = &self.msb;
        let lsb = &self.lsb;
        let as_type = &self.as_type;
        let field = self.parent.unwrap();
        let field_name = &field.ident;
        let value_type = &field.ty;
        let vis = &field.vis;
        let (getter_doc, setter_doc) = match self.doc {
            Some(ref x) => {
                let sa = format!(
                    "See also: [`{}`]({}::{})",
                    quote!(#getter),
                    quote!(#ident),
                    quote!(#getter)
                );
                let gd = x.clone();
                let sd = if getter.is_none() {
                    x.clone()
                } else {
                    LitStr::new(&sa, Span::call_site())
                };
                (gd, sd)
            }
            None => (empty_str.clone(), empty_str),
        };

        let getter_tokens = if getter.is_some() {
            if self.is_as_bool() {
                quote! {
                    #[doc = #getter_doc]
                    #[inline]
                    #vis fn #getter(&self) -> bool {
                        let mask = ((1 << (#msb - #lsb + 1)) - 1) << #lsb;
                        ((self.#field_name & mask) >> #lsb) != 0
                    }
                }
            } else if as_type.is_some() {
                quote! {
                    #[doc = #getter_doc]
                    #[inline]
                    #vis fn #getter(&self) -> #as_type {
                        let one: #value_type = 1;
                        let (mask, over) = one.overflowing_shl(#msb - #lsb + 1);
                        let mask = if over {
                            #value_type::MAX
                        } else {
                            (mask - 1) << #lsb
                        };
                        ((self.#field_name & mask) >> #lsb) as #as_type
                    }
                }
            } else {
                quote! {
                    #[doc = #getter_doc]
                    #[inline]
                    #vis fn #getter(&self) -> #value_type {
                        let one: #value_type = 1;
                        let (mask, over) = one.overflowing_shl(#msb - #lsb + 1);
                        let mask = if over {
                            #value_type::MAX
                        } else {
                            (mask - 1) << #lsb
                        };
                        (self.#field_name & mask) >> #lsb
                    }
                }
            }
        } else {
            quote! {}
        };

        let setter_tokens = if setter.is_some() {
            if self.is_as_bool() {
                quote! {
                    #[doc = #setter_doc]
                    #[inline]
                    #vis fn #setter(&mut self, value: bool) {
                        if value {
                            self.#field_name |= 1 << #lsb;
                        } else {
                            self.#field_name &= !(1 << #lsb);
                        }
                    }
                }
            } else if as_type.is_some() {
                quote! {
                    #[doc = #setter_doc]
                    #[inline]
                    #vis fn #setter(&mut self, value: #as_type) {
                        let one: #value_type = 1;
                        let (mask, over) = one.overflowing_shl(#msb - #lsb + 1);
                        let mask = if over {
                            #value_type::MAX
                        } else {
                            (mask - 1) << #lsb
                        };
                        self.#field_name &= !mask;
                        self.#field_name |= ((value as #value_type) << #lsb) & mask;
                    }
                }
            } else {
                quote! {
                    #[doc = #setter_doc]
                    #[inline]
                    #vis fn #setter(&mut self, value: #value_type) {
                        let one: #value_type = 1;
                        let (mask, over) = one.overflowing_shl(#msb - #lsb + 1);
                        let mask = if over {
                            #value_type::MAX
                        } else {
                            (mask - 1) << #lsb
                        };
                        // let mask = ((1 << (#msb - #lsb + 1)) - 1) << #lsb;
                        self.#field_name &= !mask;
                        self.#field_name |= (value << #lsb) & mask;
                    }
                }
            }
        } else {
            quote! {}
        };

        quote! {
            #getter_tokens
            #setter_tokens
        }
    }

    fn is_as_bool(&self) -> bool {
        self.as_type
            .as_ref()
            .map(|x| match x {
                Type::Path(p) => p.path.is_ident("bool"),
                _ => false,
            })
            .unwrap_or(false)
    }
}

impl<'a> Parse for BitField<'a> {
    fn parse(input: ParseStream<'_>) -> ParseResult<Self> {
        // GETTER
        let getter = input
            .parse::<Token![_]>()
            .ok()
            .map_or(input.parse::<Ident>().ok(), |_| None);

        // SETTER
        let setter = input
            .parse::<Token![,]>()
            .ok()
            .map_or(setter_for_name(&getter), |_| {
                input
                    .parse::<Token![_]>()
                    .ok()
                    .map_or(input.parse::<Ident>().ok(), |_| None)
            });

        // COLON
        let _colon_token: Token![:] = input.parse()?;

        // [MSB : LSB]
        let content;
        let _bracket_token = syn::bracketed!(content in input);
        let msb = content.parse::<LitInt>()?;
        let lsb = content
            .parse::<Token![:]>()
            .map_or(Clone::clone(&msb), |_| content.parse::<LitInt>().unwrap());

        // AS TYPE
        let as_type = input
            .parse::<Token![as]>()
            .ok()
            .and_then(|_| input.parse::<Type>().ok());

        // DOC
        let doc: Option<LitStr> = input.parse()?;

        Ok(Self {
            parent: None,
            getter,
            setter,
            msb,
            lsb,
            as_type,
            doc,
        })
    }
}

/// A derive to generate struct with bitfield access.
#[proc_macro_derive(BitFields, attributes(bitfield))]
pub fn derive_bitfields(input: TokenStream) -> TokenStream {
    let ast: DeriveInput = syn::parse(input).unwrap();
    let ident = &ast.ident;
    let generics = &ast.generics;
    let bitfields = parse_bitfields(&ast);
    let bitfields_impls: Vec<TokenStream2> = bitfields.iter().map(|x| x.codegen(ident)).collect();

    let tokens = quote! {
        impl #generics #ident #generics {
            #(#bitfields_impls)*
        }
    };

    tokens.into()
}

/// Parsing annotated bitfields.
fn parse_bitfields(ast: &DeriveInput) -> Vec<BitField> {
    let mut bitfields: Vec<BitField> = vec![];
    if let Data::Struct(s) = &ast.data {
        for f in &s.fields {
            for a in &f.attrs {
                if a.path.is_ident("bitfield") {
                    let mut bitfield: BitField = a.parse_args().unwrap();
                    bitfield.parent = Some(f);
                    bitfields.push(bitfield);
                }
            }
        }
    }
    bitfields
}

/// Generate setter with `name`.
fn setter_for_name(name: &Option<Ident>) -> Option<Ident> {
    name.as_ref()
        .map(|x| Ident::new(&format!("set_{}", x), Span::call_site()))
}
