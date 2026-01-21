use proc_macro2::{Ident, Spacing, Span, TokenStream, TokenTree};
use quote::{quote, ToTokens};
use syn::spanned::Spanned;
use syn::{parse_quote, Lifetime, LifetimeParam, Path, Type};

use crate::error::Errors;

#[derive(Default)]
pub struct TypeParams {
    type_params: Vec<(Ident, Option<Type>)>,
    lifetimes: Vec<LifetimeParam>,
    source_lifetime: Option<SourceLifetime>,
    og_source_lifetime: Option<Lifetime>,
}

pub enum SourceLifetime {
    None(Span),
    Source(Lifetime),
}

impl SourceLifetime {
    pub fn new(value: TokenStream) -> Result<Self, ()> {
        let mut iter = value.into_iter();
        match (iter.next(), iter.next(), iter.next()) {
            (Some(TokenTree::Ident(none)), None, None) if none == "None" => {
                Ok(Self::None(none.span()))
            }
            (Some(TokenTree::Punct(ap)), Some(TokenTree::Ident(ident)), None)
                if ap.as_char() == '\'' && ap.spacing() == Spacing::Joint =>
            {
                Ok(Self::Source(Lifetime {
                    apostrophe: ap.span(),
                    ident,
                }))
            }
            _ => Err(()),
        }
    }

    pub fn span(&self) -> Option<Span> {
        match self {
            SourceLifetime::None(span) => Some(*span),
            SourceLifetime::Source(lt) => Some(lt.span()),
        }
    }
}

pub struct LogosGenerics {
    pub impl_generics: TokenStream,
    pub ty_generics: TokenStream,
    pub lt_generics: TokenStream,
    pub source_lifetime: Lifetime,
}

impl TypeParams {
    pub fn explicit_lifetime(&mut self, lt: LifetimeParam) {
        if self.og_source_lifetime.is_none() {
            self.og_source_lifetime = Some(lt.lifetime.clone());
        }
        self.lifetimes.push(lt);
    }

    pub fn add(&mut self, param: Ident) {
        self.type_params.push((param, None));
    }

    pub fn set(&mut self, param: Ident, ty: TokenStream, errors: &mut Errors) {
        let ty = match syn::parse2::<Type>(ty) {
            Ok(ty) => {
                // replace_lifetimes(self, &mut ty);
                ty
            }
            Err(err) => {
                errors.err(err.to_string(), err.span());
                return;
            }
        };

        match self.type_params.iter_mut().find(|(name, _)| *name == param) {
            Some((_, slot)) => {
                if let Some(previous) = slot.replace(ty) {
                    errors
                        .err(
                            format!("{param} can only have one type assigned to it"),
                            param.span(),
                        )
                        .err("Previously assigned here", previous.span());
                }
            }
            None => {
                errors.err(
                    format!("{param} is not a declared type parameter"),
                    param.span(),
                );
            }
        }
    }

    pub fn find(&self, path: &Path) -> Option<Type> {
        for (ident, ty) in &self.type_params {
            if path.is_ident(ident) {
                return ty.clone();
            }
        }

        None
    }

    pub fn generics(&self, errors: &mut Errors) -> LogosGenerics {
        if self.lifetimes.is_empty() && self.type_params.is_empty() {
            return LogosGenerics {
                impl_generics: quote!(<'__source>),
                lt_generics: quote!(<'__source>),
                ty_generics: quote!(),
                source_lifetime: parse_quote!('__source),
            };
        }
        if self.lifetimes.len() > 1 && self.source_lifetime.is_none() {
            errors.err(
                concat!(
                    "Logos types can only have one lifetime",
                    " unless #[logos(lifetime = 'source)] is present"
                ),
                self.lifetimes[1].span(),
            );
        }

        let source_lifetime = match &self.source_lifetime {
            None => {
                if let Some(lt) = &self.og_source_lifetime {
                    lt.clone()
                } else {
                    parse_quote!('__source)
                }
            }
            Some(SourceLifetime::None(_)) => {
                parse_quote!('__source)
            }
            Some(SourceLifetime::Source(lt)) => lt.clone(),
        };
        let mut generics = LogosGenerics {
            impl_generics: TokenStream::new(),
            ty_generics: TokenStream::new(),
            lt_generics: TokenStream::new(),
            source_lifetime: source_lifetime.clone(),
        };
        generics.impl_generics.extend(quote!(<));
        generics.ty_generics.extend(quote!(<));
        generics.lt_generics.extend(quote!(<));

        match &self.source_lifetime {
            None => {
                if self.og_source_lifetime.is_none() {
                    source_lifetime.to_tokens(&mut generics.impl_generics);
                    source_lifetime.to_tokens(&mut generics.lt_generics);
                    generics.impl_generics.extend(quote!(,));
                    generics.lt_generics.extend(quote!(,));
                }
            }
            Some(SourceLifetime::None(_)) => {
                source_lifetime.to_tokens(&mut generics.impl_generics);
                source_lifetime.to_tokens(&mut generics.lt_generics);
                generics.impl_generics.extend(quote!(,));
                generics.lt_generics.extend(quote!(,));
            }
            Some(SourceLifetime::Source(_)) => {}
        }

        for lt in &self.lifetimes {
            lt.to_tokens(&mut generics.impl_generics);
            lt.lifetime.to_tokens(&mut generics.ty_generics);
            lt.lifetime.to_tokens(&mut generics.lt_generics);
            generics.impl_generics.extend(quote!(,));
            generics.ty_generics.extend(quote!(,));
            generics.lt_generics.extend(quote!(,));
        }

        for (ty, replace) in self.type_params.iter() {
            match replace {
                Some(ty) => {
                    ty.to_tokens(&mut generics.ty_generics);
                    generics.ty_generics.extend(quote!(,));
                }
                None => {
                    errors.err(
                        format!(
                            "Generic type parameter without a concrete type\n\
                            \n\
                            Define a concrete type Logos can use: #[logos(type {ty} = Type)]",
                        ),
                        ty.span(),
                    );
                }
            }
        }
        generics.impl_generics.extend(quote!(>));
        generics.ty_generics.extend(quote!(>));
        generics.lt_generics.extend(quote!(>));

        generics
    }

    pub fn lifetime(&mut self, value: TokenStream, errors: &mut Errors) {
        let span = value.span();

        match SourceLifetime::new(value) {
            Ok(lifetime) => {
                if let Some(previous) = self.source_lifetime.replace(lifetime) {
                    let previous_span = previous
                        .span()
                        .expect("Did not expect `Default` variant to be user-supplied");
                    errors
                        .err("Lifetime can be defined only once", span)
                        .err("Previous definition here", previous_span);
                }
            }
            Err(()) => {
                errors.err("Expected: 'lifetime or None", span);
            }
        }
    }
}

/*
pub fn replace_lifetimes(params: &TypeParams, ty: &mut Type) {
    traverse_type(params, ty, &mut replace_lifetime)
}

pub fn replace_lifetime(params: &TypeParams, ty: &mut Type) {
    use syn::{GenericArgument, PathArguments};

    let lt_ident = params.source_lt_ident();
    match ty {
        Type::Path(p) => {
            p.path
                .segments
                .iter_mut()
                .filter_map(|segment| match &mut segment.arguments {
                    PathArguments::AngleBracketed(ab) => Some(ab),
                    _ => None,
                })
                .flat_map(|ab| ab.args.iter_mut())
                .for_each(|arg| {
                    if let GenericArgument::Lifetime(lt) = arg {
                        *lt = Lifetime::new(&lt_ident, lt.span());
                    }
                });
        }
        Type::Reference(r) => {
            let span = match r.lifetime.take() {
                Some(lt) => lt.span(),
                None => Span::call_site(),
            };

            r.lifetime = Some(Lifetime::new(&lt_ident, span));
        }
        _ => (),
    }
}
*/
pub fn traverse_type(
    params: &TypeParams,
    ty: &mut Type,
    f: &mut impl FnMut(&TypeParams, &mut Type),
) {
    f(params, ty);
    match ty {
        Type::Array(array) => traverse_type(params, &mut array.elem, f),
        Type::BareFn(bare_fn) => {
            for input in &mut bare_fn.inputs {
                traverse_type(params, &mut input.ty, f);
            }
            if let syn::ReturnType::Type(_, ty) = &mut bare_fn.output {
                traverse_type(params, ty, f);
            }
        }
        Type::Group(group) => traverse_type(params, &mut group.elem, f),
        Type::Paren(paren) => traverse_type(params, &mut paren.elem, f),
        Type::Path(path) => traverse_path(params, &mut path.path, f),
        Type::Ptr(p) => traverse_type(params, &mut p.elem, f),
        Type::Reference(r) => traverse_type(params, &mut r.elem, f),
        Type::Slice(slice) => traverse_type(params, &mut slice.elem, f),
        Type::TraitObject(object) => object.bounds.iter_mut().for_each(|bound| {
            if let syn::TypeParamBound::Trait(trait_bound) = bound {
                traverse_path(params, &mut trait_bound.path, f);
            }
        }),
        Type::Tuple(tuple) => tuple
            .elems
            .iter_mut()
            .for_each(|elem| traverse_type(params, elem, f)),
        _ => (),
    }
}

fn traverse_path(params: &TypeParams, path: &mut Path, f: &mut impl FnMut(&TypeParams, &mut Type)) {
    for segment in &mut path.segments {
        match &mut segment.arguments {
            syn::PathArguments::None => (),
            syn::PathArguments::AngleBracketed(args) => {
                for arg in &mut args.args {
                    match arg {
                        syn::GenericArgument::Type(ty) => {
                            traverse_type(params, ty, f);
                        }
                        syn::GenericArgument::AssocType(assoc) => {
                            traverse_type(params, &mut assoc.ty, f);
                        }
                        _ => (),
                    }
                }
            }
            syn::PathArguments::Parenthesized(args) => {
                for arg in &mut args.inputs {
                    traverse_type(params, arg, f);
                }
                if let syn::ReturnType::Type(_, ty) = &mut args.output {
                    traverse_type(params, ty, f);
                }
            }
        }
    }
}
