#![allow(dead_code)]

include!(concat!(env!("OUT_DIR"), "/type_names.rs"));

#[derive(Debug, Clone)]
pub enum TypeNameState {
    Ident,
    Type,
    #[cfg(feature = "exts")]
    WarnType(crate::exts::names::ExtNameAtom),
}

impl TypeNameState {
    #[cfg(feature = "exts")]
    pub fn is_type_name(&self) -> bool {
        matches!(self, Self::Type | Self::WarnType(_))
    }

    #[cfg(not(feature = "exts"))]
    pub fn is_type_name(&self) -> bool {
        matches!(self, Self::Type)
    }
}
