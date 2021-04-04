use std::fmt;

/// A wrapper for a syntax node to be displayed
#[derive(Clone, Copy)]
pub struct NodeDisplayWrapper<'a, T: ?Sized> {
    node: &'a T,
    current_level: usize,
}

impl NodeDisplayWrapper<'static, str> {
    /// Create a new [NodeDisplayWrapper]
    ///
    /// # Parameters
    ///
    /// * `name`: name of the node being displayed
    /// * `level`: current indentation level
    pub fn new(name: &'static str, level: usize) -> Self {
        Self {
            node: name,
            current_level: level,
        }
    }
}

impl fmt::Display for NodeDisplayWrapper<'static, str> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for _ in 0..self.current_level {
            write!(f, "  ")?;
        }

        writeln!(f, "{}", self.node)?;

        Ok(())
    }
}

impl<'a, T> NodeDisplayWrapper<'a, T> {
    /// Set the level of this display wrapper
    ///
    /// # Parameters
    ///
    /// * `level`: new indentation level
    ///
    /// # Returns
    ///
    /// New display wrapper with the updated level.
    pub fn set_level(self, level: usize) -> Self {
        Self {
            node: self.node,
            current_level: level,
        }
    }
}

impl<T: NodeDisplay> fmt::Display for NodeDisplayWrapper<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(name) = T::name() {
            for _ in 0..self.current_level {
                write!(f, "  ")?;
            }

            if let (Some(start), Some(end)) = (self.node.start(), self.node.end()) {
                if let Some(source_id) = self.node.source_id() {
                    write!(f, "{}@{}:{}..{}", name, source_id, start, end)?;
                } else {
                    write!(f, "{}@{}..{}", name, start, end)?;
                }
            } else {
                write!(f, "{}", name)?;
            }

            self.node.display_extra(f)?;
            writeln!(f)?;

            self.node.display_children(self.current_level + 1, f)?;
        } else {
            self.node.display_children(self.current_level, f)?;
        }

        Ok(())
    }
}

/// Trait for displaying an AST node's content
pub trait NodeContentDisplay {
    /// Name of the node
    ///
    /// # Returns
    ///
    /// `None` if this node is just a transparent wrapper (and should not be displayed), otherwise
    /// the name of the node type.
    fn name() -> Option<&'static str>;

    /// Display extra information for the node
    ///
    /// # Parameters
    ///
    /// * `f`: formatter to output to
    fn display_extra(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result;

    /// Display the node's children
    ///
    /// # Parameters
    ///
    /// * `level`: current indentation level
    /// * `f`: formatter to output to
    fn display_children(&self, level: usize, f: &mut fmt::Formatter<'_>) -> fmt::Result;
}

macro_rules! forward_display {
    ($i:ty => $e:expr) => {
        impl NodeContentDisplay for $i {
            fn name() -> Option<&'static str> {
                Some($e)
            }

            fn display_extra(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, " `{}`", self)
            }

            fn display_children(&self, _level: usize, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
                Ok(())
            }
        }
    }
}

forward_display!(smol_str::SmolStr => "String");
forward_display!(String => "String");
forward_display!(f32 => "FloatConst");
forward_display!(f64 => "DoubleConst");
forward_display!(i32 => "IntConst");
forward_display!(u16 => "ShortConst");
forward_display!(u32 => "UIntConst");
forward_display!(bool => "BoolConst");

/// Trait for displaying a syntax node
pub trait NodeDisplay: Sized {
    /// Name of the syntax node's type
    fn name() -> Option<&'static str>;

    /// Starting position of the node
    fn start(&self) -> Option<usize>;
    /// Ending position of the node
    fn end(&self) -> Option<usize>;
    /// Source id of the node
    fn source_id(&self) -> Option<usize>;

    /// Obtain a display wrapper for the current node
    fn display(&self) -> NodeDisplayWrapper<Self>;

    /// Display extra information for the node
    ///
    /// # Parameters
    ///
    /// * `f`: formatter to output to
    fn display_extra(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result;

    /// Display the node's children
    ///
    /// # Parameters
    ///
    /// * `level`: current indentation level
    /// * `f`: formatter to output to
    fn display_children(&self, level: usize, f: &mut fmt::Formatter<'_>) -> fmt::Result;
}

impl<T: NodeContentDisplay + super::NodeContent> NodeDisplay for super::Node<T> {
    fn name() -> Option<&'static str> {
        T::name()
    }

    fn start(&self) -> Option<usize> {
        self.span.map(|s| s.start)
    }

    fn end(&self) -> Option<usize> {
        self.span.map(|s| s.end)
    }

    fn source_id(&self) -> Option<usize> {
        self.span.map(|s| s.source_id)
    }

    fn display(&self) -> NodeDisplayWrapper<Self> {
        NodeDisplayWrapper {
            node: self,
            current_level: 0,
        }
    }

    fn display_extra(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        (**self).display_extra(f)
    }

    fn display_children(&self, level: usize, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        (**self).display_children(level, f)
    }
}

impl<T: NodeContentDisplay> NodeDisplay for T {
    fn name() -> Option<&'static str> {
        T::name()
    }

    fn start(&self) -> Option<usize> {
        None
    }

    fn end(&self) -> Option<usize> {
        None
    }

    fn source_id(&self) -> Option<usize> {
        None
    }

    fn display(&self) -> NodeDisplayWrapper<Self> {
        NodeDisplayWrapper {
            node: self,
            current_level: 0,
        }
    }

    fn display_extra(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.display_extra(f)
    }

    fn display_children(&self, level: usize, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.display_children(level, f)
    }
}

impl<T: NodeDisplay> NodeContentDisplay for Box<T> {
    fn name() -> Option<&'static str> {
        T::name()
    }

    fn display_extra(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        (**self).display_extra(f)
    }

    fn display_children(&self, level: usize, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        (**self).display_children(level, f)
    }
}

impl<T: NodeDisplay> NodeContentDisplay for Option<T> {
    fn name() -> Option<&'static str> {
        None
    }

    fn display_extra(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(inner) = self.as_ref() {
            inner.display_extra(f)
        } else {
            Ok(())
        }
    }

    fn display_children(&self, level: usize, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(inner) = self.as_ref() {
            inner.display_children(level, f)
        } else {
            Ok(())
        }
    }
}

impl<T: NodeDisplay> NodeContentDisplay for Vec<T> {
    fn name() -> Option<&'static str> {
        None
    }

    fn display_extra(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Ok(())
    }

    fn display_children(&self, level: usize, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for item in self.iter() {
            write!(f, "{}", item.display().set_level(level))?;
        }

        Ok(())
    }
}
