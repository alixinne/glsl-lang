//! Token derive support definitions

/// Information about a known token
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Hash)]
#[cfg_attr(feature = "serde", derive(rserde::Serialize))]
#[cfg_attr(feature = "serde", serde(crate = "rserde"))]
pub struct TokenDescriptor {
    /// Variant name
    pub variant_name: &'static str,

    /// Parser token name
    pub parser_token: &'static str,

    /// List of kinds this token belongs to
    pub kinds: &'static [&'static str],
}

impl TokenDescriptor {
    /// Create a new token descriptor
    pub const fn new(
        variant_name: &'static str,
        parser_token: &'static str,
        kinds: &'static [&'static str],
    ) -> Self {
        Self {
            variant_name,
            parser_token,
            kinds,
        }
    }
}

/// Trait to implement for a token to be used with `lang_util`'s infrastructure
pub trait Token: std::fmt::Display {
    /// Return the variant name of the current token
    fn variant_name(&self) -> &'static str;

    /// Return the name used by the lalrpop parser for this token
    fn parser_token(&self) -> &'static str;

    /// Return the token kinds this token belongs to
    fn kinds(&self) -> &'static [&'static str];

    /// Return the descriptions for all known tokens
    fn all_tokens() -> &'static [TokenDescriptor];
}
