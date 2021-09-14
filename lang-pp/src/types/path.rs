#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParsedPath {
    pub path: String,
    pub ty: PathType,
}

impl std::fmt::Display for ParsedPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.ty {
            PathType::Angle => write!(f, "<{}>", self.path),
            PathType::Quote => write!(f, "\"{}\"", self.path),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PathType {
    Angle,
    Quote,
}
