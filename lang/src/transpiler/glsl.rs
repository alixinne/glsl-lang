//! A GLSL450/GLSL460 transpiler that takes a syntax tree and writes it as a plain raw GLSL
//! [`String`].
//!
//! # Foreword
//!
//! This module exports several functions that just transform a part of a syntax tree into its raw
//! GLSL [`String`] representation.
//!
//! > Important note: this module – and actually, any [`transpiler`] module – is not responsible in
//! > optimizing the syntax tree nor semantically check its validity. This is done in other stages
//! > of the compilation process.
//!
//! In order to achieve that purpose, you could:
//!
//! - For each elements in the AST, return a [`String`] or [`Cow<str>`].
//! - Insert the string representation via a formatter.
//!
//! The second solution is better because it lets the user handle the memory the way they want:
//! they might just use a dynamic buffer that implements [`Write`] or simply pass a `&mut`
//! [`String`]. It’s up to you.
//!
//! # How to use this module
//!
//! First, head over to the [`ast`] module. That module defines the AST items defined by GLSL. This
//! very module provides you with functions like `show_*` taking the AST item and writing it to a
//! [`Write`] object. You’re likely to be interested in [`show_translation_unit`] to start with.
//!
//! [`Cow<str>`]: std::borrow::Cow
//! [`Write`]: std::fmt::Write
//! [`show_translation_unit`]: crate::transpiler::glsl::show_translation_unit
//! [`ast`]: crate::ast
//! [`transpiler`]: crate::transpiler

use std::fmt::Write;

use lazy_static::lazy_static;

/// Indentation style of the output
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum IndentStyle {
    /// No indentation is generated
    None,
    /// Items are indented with tabs. In case spaces are needed for alignment,
    /// tabs are assumed to be `tab_size` characters wide.
    Tabs {
        /// Size of the tabs in characters
        tab_size: u32,
        /// Number of tab characters used per indent level
        count: u32,
    },
    /// Items are indented with spaces.
    Spaces {
        /// Number of space characters used per indent level
        count: u32,
    },
}

impl IndentStyle {
    /// Write the current indenting level and style to the output
    pub fn write<F>(&self, f: &mut F, levels: u32) -> std::fmt::Result
    where
        F: Write,
    {
        match self {
            Self::None => {}
            Self::Tabs { count, .. } => {
                for _ in 0..count * levels {
                    f.write_char('\t')?;
                }
            }
            Self::Spaces { count, .. } => {
                for _ in 0..count * levels {
                    f.write_char(' ')?;
                }
            }
        }

        Ok(())
    }
}

impl Default for IndentStyle {
    fn default() -> Self {
        Self::Spaces { count: 4 }
    }
}

/// Formatter whitespace
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Whitespace {
    /// No whitespace
    None,
    /// A space
    Space,
    /// A newline
    Newline,
}

impl Whitespace {
    /// Write this whitespace to the output
    pub fn write<F>(&self, f: &mut F, state: &mut FormattingState) -> std::fmt::Result
    where
        F: Write,
    {
        match self {
            Self::None => Ok(()),
            Self::Space => f.write_char(' '),
            Self::Newline => state.new_line(true),
        }
    }
}

/// Formatting settings for the GLSL transpiler
#[derive(Debug, Clone, PartialEq)]
pub struct FormattingSettings {
    /// Indentation style of the output
    pub indent_style: IndentStyle,
    /// Insert newlines after block open braces
    pub newline_after_open_block: bool,
    /// Insert newlines before block close braces
    pub newline_before_close_block: bool,
    /// Insert newline after block close brace
    pub newline_after_close_block: bool,
    /// What to insert between fields of a struct
    pub struct_field_separator: Whitespace,
    /// What to insert after a struct declaration
    pub struct_declaration_terminator: Whitespace,
    /// What to insert after a declaration
    pub declaration_terminator: Whitespace,
    /// Insert spaces around binary ops
    pub spaces_around_binary_ops: bool,
    /// What to insert after a statement
    pub statement_terminator: Whitespace,
    /// What to insert after a function definition
    pub function_definition_terminator: Whitespace,
}

impl Default for FormattingSettings {
    fn default() -> Self {
        Self {
            indent_style: IndentStyle::default(),
            newline_after_open_block: true,
            newline_before_close_block: true,
            newline_after_close_block: true,
            struct_field_separator: Whitespace::Newline,
            struct_declaration_terminator: Whitespace::Newline,
            declaration_terminator: Whitespace::Newline,
            spaces_around_binary_ops: true,
            statement_terminator: Whitespace::Newline,
            function_definition_terminator: Whitespace::Newline,
        }
    }
}

/// Formatting state of the GLSL transpiler
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct FormattingState<'s> {
    /// Formatting settings
    pub settings: &'s FormattingSettings,
    indentation_level: u32,
    new_line_pending: bool,
}

impl<'s> FormattingState<'s> {
    fn write_indent<F>(&self, f: &mut F) -> std::fmt::Result
    where
        F: Write,
    {
        self.settings.indent_style.write(f, self.indentation_level)
    }

    fn write_line<F>(&mut self, f: &mut F) -> std::fmt::Result
    where
        F: Write,
    {
        f.write_char('\n')?;
        self.write_indent(f)
    }

    /// Append a pending new line to the output
    pub fn new_line(&mut self, required: bool) -> std::fmt::Result {
        if required {
            self.new_line_pending = true;
        }

        Ok(())
    }

    /// Consume the pending newlines
    pub fn consume_newline(&mut self) {
        self.new_line_pending = false;
    }

    /// Flush pending newlines to the output, if any
    pub fn flush_line<F>(&mut self, f: &mut F) -> std::fmt::Result
    where
        F: Write,
    {
        if self.new_line_pending {
            self.write_line(f)?;
            self.new_line_pending = false;
        }

        Ok(())
    }

    /// Flush pending newlines as spaces to the output, if any
    pub fn flush_space<F>(&mut self, f: &mut F) -> std::fmt::Result
    where
        F: Write,
    {
        if self.new_line_pending {
            f.write_char(' ')?;
            self.new_line_pending = false;
        }

        Ok(())
    }

    /// Enter a new block, and update the indentation level
    pub fn enter_block<F>(&mut self, f: &mut F) -> std::fmt::Result
    where
        F: Write,
    {
        // Open block
        f.write_char('{')?;

        // Write line with new indentation level
        self.indentation_level += 1;
        self.new_line(self.settings.newline_after_open_block)?;

        Ok(())
    }

    /// Exit the current block, and update the indentation level
    pub fn exit_block<F>(&mut self, f: &mut F) -> std::fmt::Result
    where
        F: Write,
    {
        // Update indentation level
        self.indentation_level -= 1;

        // Next line
        self.new_line(self.settings.newline_before_close_block)?;

        // Flush lines
        self.flush_line(f)?;

        // Close block
        f.write_char('}')?;

        // Next line
        self.new_line(self.settings.newline_after_close_block)?;

        Ok(())
    }

    /// Write a struct field separator
    pub fn write_struct_field_separator<F>(&mut self, f: &mut F) -> std::fmt::Result
    where
        F: Write,
    {
        f.write_char(';')?;
        self.settings.struct_field_separator.write(f, self)
    }

    /// Write a struct declaration terminator
    pub fn write_struct_declaration_terminator<F>(&mut self, f: &mut F) -> std::fmt::Result
    where
        F: Write,
    {
        f.write_char(';')?;
        self.settings.struct_declaration_terminator.write(f, self)
    }

    /// Write a declaration terminator
    pub fn write_declaration_terminator<F>(&mut self, f: &mut F) -> std::fmt::Result
    where
        F: Write,
    {
        f.write_char(';')?;
        self.settings.declaration_terminator.write(f, self)
    }

    /// Write a binary operator
    pub fn write_binary_op<F>(&self, f: &mut F, op: &str) -> std::fmt::Result
    where
        F: Write,
    {
        if self.settings.spaces_around_binary_ops {
            f.write_char(' ')?;
        }

        f.write_str(op)?;

        if self.settings.spaces_around_binary_ops {
            f.write_char(' ')?;
        }

        Ok(())
    }

    /// Write a statement terminator
    pub fn write_statement_terminator<F>(&mut self, f: &mut F) -> std::fmt::Result
    where
        F: Write,
    {
        f.write_char(';')?;
        self.settings.statement_terminator.write(f, self)
    }

    /// Write a function definition terminator
    pub fn write_function_definition_terminator<F>(&mut self, f: &mut F) -> std::fmt::Result
    where
        F: Write,
    {
        self.settings.function_definition_terminator.write(f, self)
    }
}

impl<'s> From<&'s FormattingSettings> for FormattingState<'s> {
    fn from(settings: &'s FormattingSettings) -> Self {
        Self {
            settings,
            indentation_level: 0,
            new_line_pending: false,
        }
    }
}

lazy_static! {
    static ref DEFAULT_SETTINGS: FormattingSettings = FormattingSettings::default();
}

impl Default for FormattingState<'static> {
    fn default() -> Self {
        Self {
            settings: &DEFAULT_SETTINGS,
            indentation_level: 0,
            new_line_pending: false,
        }
    }
}

use crate::ast;

/// Precedence information for transpiling parentheses properly
trait HasPrecedence {
    /// Return the precedence level of the expression
    fn precedence(&self) -> u32;
}

impl HasPrecedence for ast::Expr {
    fn precedence(&self) -> u32 {
        match self {
            // 0 isn't a valid precedence, but we use this to represent atomic expressions
            Self::Variable(_)
            | Self::IntConst(_)
            | Self::UIntConst(_)
            | Self::BoolConst(_)
            | Self::FloatConst(_)
            | Self::DoubleConst(_) => 0,
            // Precedence operator expression is precedence of operator
            Self::Unary(op, _) => op.precedence(),
            Self::Binary(op, _, _) => op.precedence(),
            Self::Ternary(_, _, _) => 15,
            Self::Assignment(_, op, _) => op.precedence(),
            Self::Bracket(_, _)
            | Self::FunCall(_, _)
            | Self::Dot(_, _)
            | Self::PostInc(_)
            | Self::PostDec(_) => 2,
            Self::Comma(_, _) => 17,
        }
    }
}

impl HasPrecedence for ast::UnaryOp {
    fn precedence(&self) -> u32 {
        3
    }
}

impl HasPrecedence for ast::BinaryOp {
    fn precedence(&self) -> u32 {
        match self {
            Self::Mult | Self::Div | Self::Mod => 4,
            Self::Add | Self::Sub => 5,
            Self::LShift | Self::RShift => 6,
            Self::LT | Self::GT | Self::LTE | Self::GTE => 7,
            Self::Equal | Self::NonEqual => 8,
            Self::BitAnd => 9,
            Self::BitXor => 10,
            Self::BitOr => 11,
            Self::And => 12,
            Self::Xor => 13,
            Self::Or => 14,
        }
    }
}

impl HasPrecedence for ast::AssignmentOp {
    fn precedence(&self) -> u32 {
        16
    }
}

/// Transpile an identifier to GLSL
pub fn show_identifier<F>(
    f: &mut F,
    i: &ast::Identifier,
    _: &mut FormattingState<'_>,
) -> std::fmt::Result
where
    F: Write,
{
    f.write_str(&i.0)
}

/// Transpile a type_name to GLSL
pub fn show_type_name<F>(
    f: &mut F,
    t: &ast::TypeName,
    _: &mut FormattingState<'_>,
) -> std::fmt::Result
where
    F: Write,
{
    f.write_str(&t.0)
}

/// Transpile a type_specifier_non_array to GLSL
pub fn show_type_specifier_non_array<F>(
    f: &mut F,
    t: &ast::TypeSpecifierNonArray,
    state: &mut FormattingState<'_>,
) -> std::fmt::Result
where
    F: Write,
{
    match *t {
        ast::TypeSpecifierNonArray::Void => f.write_str("void"),
        ast::TypeSpecifierNonArray::Bool => f.write_str("bool"),
        ast::TypeSpecifierNonArray::Int => f.write_str("int"),
        ast::TypeSpecifierNonArray::UInt => f.write_str("uint"),
        ast::TypeSpecifierNonArray::Float => f.write_str("float"),
        ast::TypeSpecifierNonArray::Double => f.write_str("double"),
        ast::TypeSpecifierNonArray::Vec2 => f.write_str("vec2"),
        ast::TypeSpecifierNonArray::Vec3 => f.write_str("vec3"),
        ast::TypeSpecifierNonArray::Vec4 => f.write_str("vec4"),
        ast::TypeSpecifierNonArray::DVec2 => f.write_str("dvec2"),
        ast::TypeSpecifierNonArray::DVec3 => f.write_str("dvec3"),
        ast::TypeSpecifierNonArray::DVec4 => f.write_str("dvec4"),
        ast::TypeSpecifierNonArray::BVec2 => f.write_str("bvec2"),
        ast::TypeSpecifierNonArray::BVec3 => f.write_str("bvec3"),
        ast::TypeSpecifierNonArray::BVec4 => f.write_str("bvec4"),
        ast::TypeSpecifierNonArray::IVec2 => f.write_str("ivec2"),
        ast::TypeSpecifierNonArray::IVec3 => f.write_str("ivec3"),
        ast::TypeSpecifierNonArray::IVec4 => f.write_str("ivec4"),
        ast::TypeSpecifierNonArray::UVec2 => f.write_str("uvec2"),
        ast::TypeSpecifierNonArray::UVec3 => f.write_str("uvec3"),
        ast::TypeSpecifierNonArray::UVec4 => f.write_str("uvec4"),
        ast::TypeSpecifierNonArray::Mat2 => f.write_str("mat2"),
        ast::TypeSpecifierNonArray::Mat3 => f.write_str("mat3"),
        ast::TypeSpecifierNonArray::Mat4 => f.write_str("mat4"),
        ast::TypeSpecifierNonArray::Mat22 => f.write_str("mat2x2"),
        ast::TypeSpecifierNonArray::Mat23 => f.write_str("mat2x3"),
        ast::TypeSpecifierNonArray::Mat24 => f.write_str("mat2x4"),
        ast::TypeSpecifierNonArray::Mat32 => f.write_str("mat3x2"),
        ast::TypeSpecifierNonArray::Mat33 => f.write_str("mat3x3"),
        ast::TypeSpecifierNonArray::Mat34 => f.write_str("mat3x4"),
        ast::TypeSpecifierNonArray::Mat42 => f.write_str("mat4x2"),
        ast::TypeSpecifierNonArray::Mat43 => f.write_str("mat4x3"),
        ast::TypeSpecifierNonArray::Mat44 => f.write_str("mat4x4"),
        ast::TypeSpecifierNonArray::DMat2 => f.write_str("dmat2"),
        ast::TypeSpecifierNonArray::DMat3 => f.write_str("dmat3"),
        ast::TypeSpecifierNonArray::DMat4 => f.write_str("dmat4"),
        ast::TypeSpecifierNonArray::DMat22 => f.write_str("dmat2x2"),
        ast::TypeSpecifierNonArray::DMat23 => f.write_str("dmat2x3"),
        ast::TypeSpecifierNonArray::DMat24 => f.write_str("dmat2x4"),
        ast::TypeSpecifierNonArray::DMat32 => f.write_str("dmat3x2"),
        ast::TypeSpecifierNonArray::DMat33 => f.write_str("dmat3x3"),
        ast::TypeSpecifierNonArray::DMat34 => f.write_str("dmat3x4"),
        ast::TypeSpecifierNonArray::DMat42 => f.write_str("dmat4x2"),
        ast::TypeSpecifierNonArray::DMat43 => f.write_str("dmat4x3"),
        ast::TypeSpecifierNonArray::DMat44 => f.write_str("dmat4x4"),
        ast::TypeSpecifierNonArray::Sampler1D => f.write_str("sampler1D"),
        ast::TypeSpecifierNonArray::Image1D => f.write_str("image1D"),
        ast::TypeSpecifierNonArray::Sampler2D => f.write_str("sampler2D"),
        ast::TypeSpecifierNonArray::Image2D => f.write_str("image2D"),
        ast::TypeSpecifierNonArray::Sampler3D => f.write_str("sampler3D"),
        ast::TypeSpecifierNonArray::Image3D => f.write_str("image3D"),
        ast::TypeSpecifierNonArray::SamplerCube => f.write_str("samplerCube"),
        ast::TypeSpecifierNonArray::ImageCube => f.write_str("imageCube"),
        ast::TypeSpecifierNonArray::Sampler2DRect => f.write_str("sampler2DRect"),
        ast::TypeSpecifierNonArray::Image2DRect => f.write_str("image2DRect"),
        ast::TypeSpecifierNonArray::Sampler1DArray => f.write_str("sampler1DArray"),
        ast::TypeSpecifierNonArray::Image1DArray => f.write_str("image1DArray"),
        ast::TypeSpecifierNonArray::Sampler2DArray => f.write_str("sampler2DArray"),
        ast::TypeSpecifierNonArray::Image2DArray => f.write_str("image2DArray"),
        ast::TypeSpecifierNonArray::SamplerBuffer => f.write_str("samplerBuffer"),
        ast::TypeSpecifierNonArray::ImageBuffer => f.write_str("imageBuffer"),
        ast::TypeSpecifierNonArray::Sampler2DMS => f.write_str("sampler2DMS"),
        ast::TypeSpecifierNonArray::Image2DMS => f.write_str("image2DMS"),
        ast::TypeSpecifierNonArray::Sampler2DMSArray => f.write_str("sampler2DMSArray"),
        ast::TypeSpecifierNonArray::Image2DMSArray => f.write_str("image2DMSArray"),
        ast::TypeSpecifierNonArray::SamplerCubeArray => f.write_str("samplerCubeArray"),
        ast::TypeSpecifierNonArray::ImageCubeArray => f.write_str("imageCubeArray"),
        ast::TypeSpecifierNonArray::Sampler1DShadow => f.write_str("sampler1DShadow"),
        ast::TypeSpecifierNonArray::Sampler2DShadow => f.write_str("sampler2DShadow"),
        ast::TypeSpecifierNonArray::Sampler2DRectShadow => f.write_str("sampler2DRectShadow"),
        ast::TypeSpecifierNonArray::Sampler1DArrayShadow => f.write_str("sampler1DArrayShadow"),
        ast::TypeSpecifierNonArray::Sampler2DArrayShadow => f.write_str("sampler2DArrayShadow"),
        ast::TypeSpecifierNonArray::SamplerCubeShadow => f.write_str("samplerCubeShadow"),
        ast::TypeSpecifierNonArray::SamplerCubeArrayShadow => f.write_str("samplerCubeArrayShadow"),
        ast::TypeSpecifierNonArray::ISampler1D => f.write_str("isampler1D"),
        ast::TypeSpecifierNonArray::IImage1D => f.write_str("iimage1D"),
        ast::TypeSpecifierNonArray::ISampler2D => f.write_str("isampler2D"),
        ast::TypeSpecifierNonArray::IImage2D => f.write_str("iimage2D"),
        ast::TypeSpecifierNonArray::ISampler3D => f.write_str("isampler3D"),
        ast::TypeSpecifierNonArray::IImage3D => f.write_str("iimage3D"),
        ast::TypeSpecifierNonArray::ISamplerCube => f.write_str("isamplerCube"),
        ast::TypeSpecifierNonArray::IImageCube => f.write_str("iimageCube"),
        ast::TypeSpecifierNonArray::ISampler2DRect => f.write_str("isampler2DRect"),
        ast::TypeSpecifierNonArray::IImage2DRect => f.write_str("iimage2DRect"),
        ast::TypeSpecifierNonArray::ISampler1DArray => f.write_str("isampler1DArray"),
        ast::TypeSpecifierNonArray::IImage1DArray => f.write_str("iimage1DArray"),
        ast::TypeSpecifierNonArray::ISampler2DArray => f.write_str("isampler2DArray"),
        ast::TypeSpecifierNonArray::IImage2DArray => f.write_str("iimage2DArray"),
        ast::TypeSpecifierNonArray::ISamplerBuffer => f.write_str("isamplerBuffer"),
        ast::TypeSpecifierNonArray::IImageBuffer => f.write_str("iimageBuffer"),
        ast::TypeSpecifierNonArray::ISampler2DMS => f.write_str("isampler2MS"),
        ast::TypeSpecifierNonArray::IImage2DMS => f.write_str("iimage2DMS"),
        ast::TypeSpecifierNonArray::ISampler2DMSArray => f.write_str("isampler2DMSArray"),
        ast::TypeSpecifierNonArray::IImage2DMSArray => f.write_str("iimage2DMSArray"),
        ast::TypeSpecifierNonArray::ISamplerCubeArray => f.write_str("isamplerCubeArray"),
        ast::TypeSpecifierNonArray::IImageCubeArray => f.write_str("iimageCubeArray"),
        ast::TypeSpecifierNonArray::AtomicUInt => f.write_str("atomic_uint"),
        ast::TypeSpecifierNonArray::USampler1D => f.write_str("usampler1D"),
        ast::TypeSpecifierNonArray::UImage1D => f.write_str("uimage1D"),
        ast::TypeSpecifierNonArray::USampler2D => f.write_str("usampler2D"),
        ast::TypeSpecifierNonArray::UImage2D => f.write_str("uimage2D"),
        ast::TypeSpecifierNonArray::USampler3D => f.write_str("usampler3D"),
        ast::TypeSpecifierNonArray::UImage3D => f.write_str("uimage3D"),
        ast::TypeSpecifierNonArray::USamplerCube => f.write_str("usamplerCube"),
        ast::TypeSpecifierNonArray::UImageCube => f.write_str("uimageCube"),
        ast::TypeSpecifierNonArray::USampler2DRect => f.write_str("usampler2DRect"),
        ast::TypeSpecifierNonArray::UImage2DRect => f.write_str("uimage2DRect"),
        ast::TypeSpecifierNonArray::USampler1DArray => f.write_str("usampler1DArray"),
        ast::TypeSpecifierNonArray::UImage1DArray => f.write_str("uimage1DArray"),
        ast::TypeSpecifierNonArray::USampler2DArray => f.write_str("usampler2DArray"),
        ast::TypeSpecifierNonArray::UImage2DArray => f.write_str("uimage2DArray"),
        ast::TypeSpecifierNonArray::USamplerBuffer => f.write_str("usamplerBuffer"),
        ast::TypeSpecifierNonArray::UImageBuffer => f.write_str("uimageBuffer"),
        ast::TypeSpecifierNonArray::USampler2DMS => f.write_str("usampler2DMS"),
        ast::TypeSpecifierNonArray::UImage2DMS => f.write_str("uimage2DMS"),
        ast::TypeSpecifierNonArray::USampler2DMSArray => f.write_str("usamplerDMSArray"),
        ast::TypeSpecifierNonArray::UImage2DMSArray => f.write_str("uimage2DMSArray"),
        ast::TypeSpecifierNonArray::USamplerCubeArray => f.write_str("usamplerCubeArray"),
        ast::TypeSpecifierNonArray::UImageCubeArray => f.write_str("uimageCubeArray"),
        ast::TypeSpecifierNonArray::Struct(ref st) => show_struct_non_declaration(f, st, state),
        ast::TypeSpecifierNonArray::TypeName(ref tn) => show_type_name(f, tn, state),
    }
}

/// Transpile a type_specifier to GLSL
pub fn show_type_specifier<F>(
    f: &mut F,
    t: &ast::TypeSpecifier,
    state: &mut FormattingState<'_>,
) -> std::fmt::Result
where
    F: Write,
{
    show_type_specifier_non_array(f, &t.ty, state)?;

    if let Some(ref arr_spec) = t.array_specifier {
        show_array_spec(f, arr_spec, state)?;
    }

    Ok(())
}

/// Transpile a fully_specified_type to GLSL
pub fn show_fully_specified_type<F>(
    f: &mut F,
    t: &ast::FullySpecifiedType,
    state: &mut FormattingState<'_>,
) -> std::fmt::Result
where
    F: Write,
{
    if let Some(ref qual) = t.qualifier {
        show_type_qualifier(f, &qual, state)?;
        f.write_str(" ")?;
    }

    show_type_specifier(f, &t.ty, state)
}

/// Transpile a struct_non_declaration to GLSL
pub fn show_struct_non_declaration<F>(
    f: &mut F,
    st: &ast::StructSpecifier,
    state: &mut FormattingState<'_>,
) -> std::fmt::Result
where
    F: Write,
{
    f.write_str("struct ")?;

    if let Some(ref name) = st.name {
        write!(f, "{} ", name)?;
    }

    state.enter_block(f)?;

    for field in &st.fields {
        state.flush_line(f)?;
        show_struct_field(f, field, state)?;
        state.write_struct_field_separator(f)?;
    }

    state.exit_block(f)?;

    Ok(())
}

/// Transpile a struct to GLSL
pub fn show_struct<F>(
    f: &mut F,
    st: &ast::StructSpecifier,
    state: &mut FormattingState<'_>,
) -> std::fmt::Result
where
    F: Write,
{
    show_struct_non_declaration(f, st, state)?;
    state.write_struct_declaration_terminator(f)
}

/// Transpile a struct_field to GLSL
pub fn show_struct_field<F>(
    f: &mut F,
    field: &ast::StructFieldSpecifier,
    state: &mut FormattingState<'_>,
) -> std::fmt::Result
where
    F: Write,
{
    if let Some(ref qual) = field.qualifier {
        show_type_qualifier(f, &qual, state)?;
        f.write_str(" ")?;
    }

    show_type_specifier(f, &field.ty, state)?;
    f.write_str(" ")?;

    // there’s at least one identifier
    let mut identifiers = field.identifiers.iter();
    let identifier = identifiers.next().unwrap();

    show_arrayed_identifier(f, identifier, state)?;

    // write the rest of the identifiers
    for identifier in identifiers {
        f.write_str(", ")?;
        show_arrayed_identifier(f, identifier, state)?;
    }

    Ok(())
}

/// Transpile an array_spec to GLSL
pub fn show_array_spec<F>(
    f: &mut F,
    a: &ast::ArraySpecifier,
    state: &mut FormattingState<'_>,
) -> std::fmt::Result
where
    F: Write,
{
    for dimension in &a.dimensions {
        match *dimension {
            ast::ArraySpecifierDimension::Unsized => f.write_str("[]")?,
            ast::ArraySpecifierDimension::ExplicitlySized(ref e) => {
                f.write_str("[")?;
                show_expr(f, &e, state)?;
                f.write_str("]")?
            }
        }
    }

    Ok(())
}

/// Transpile an arrayed_identifier to GLSL
pub fn show_arrayed_identifier<F>(
    f: &mut F,
    a: &ast::ArrayedIdentifier,
    state: &mut FormattingState<'_>,
) -> std::fmt::Result
where
    F: Write,
{
    write!(f, "{}", a.ident)?;

    if let Some(ref arr_spec) = a.array_spec {
        show_array_spec(f, arr_spec, state)?;
    }

    Ok(())
}

/// Transpile a type_qualifier to GLSL
pub fn show_type_qualifier<F>(
    f: &mut F,
    q: &ast::TypeQualifier,
    state: &mut FormattingState<'_>,
) -> std::fmt::Result
where
    F: Write,
{
    let mut qualifiers = q.qualifiers.iter();
    let first = qualifiers.next().unwrap();

    show_type_qualifier_spec(f, first, state)?;

    for qual_spec in qualifiers {
        f.write_str(" ")?;
        show_type_qualifier_spec(f, qual_spec, state)?;
    }

    Ok(())
}

/// Transpile a type_qualifier_spec to GLSL
pub fn show_type_qualifier_spec<F>(
    f: &mut F,
    q: &ast::TypeQualifierSpec,
    state: &mut FormattingState<'_>,
) -> std::fmt::Result
where
    F: Write,
{
    match *q {
        ast::TypeQualifierSpec::Storage(ref st) => show_storage_qualifier(f, &st, state),
        ast::TypeQualifierSpec::Layout(ref l) => show_layout_qualifier(f, &l, state),
        ast::TypeQualifierSpec::Precision(ref p) => show_precision_qualifier(f, &p, state),
        ast::TypeQualifierSpec::Interpolation(ref i) => show_interpolation_qualifier(f, &i, state),
        ast::TypeQualifierSpec::Invariant => f.write_str("invariant"),
        ast::TypeQualifierSpec::Precise => f.write_str("precise"),
    }
}

/// Transpile a storage_qualifier to GLSL
pub fn show_storage_qualifier<F>(
    f: &mut F,
    q: &ast::StorageQualifier,
    state: &mut FormattingState<'_>,
) -> std::fmt::Result
where
    F: Write,
{
    match *q {
        ast::StorageQualifier::Const => f.write_str("const"),
        ast::StorageQualifier::InOut => f.write_str("inout"),
        ast::StorageQualifier::In => f.write_str("in"),
        ast::StorageQualifier::Out => f.write_str("out"),
        ast::StorageQualifier::Centroid => f.write_str("centroid"),
        ast::StorageQualifier::Patch => f.write_str("patch"),
        ast::StorageQualifier::Sample => f.write_str("sample"),
        ast::StorageQualifier::Uniform => f.write_str("uniform"),
        ast::StorageQualifier::Buffer => f.write_str("buffer"),
        ast::StorageQualifier::Shared => f.write_str("shared"),
        ast::StorageQualifier::Coherent => f.write_str("coherent"),
        ast::StorageQualifier::Volatile => f.write_str("volatile"),
        ast::StorageQualifier::Restrict => f.write_str("restrict"),
        ast::StorageQualifier::ReadOnly => f.write_str("readonly"),
        ast::StorageQualifier::WriteOnly => f.write_str("writeonly"),
        ast::StorageQualifier::Subroutine(ref n) => show_subroutine(f, &n, state),
    }
}

/// Transpile a subroutine to GLSL
pub fn show_subroutine<F>(
    f: &mut F,
    types: &[ast::TypeSpecifier],
    state: &mut FormattingState<'_>,
) -> std::fmt::Result
where
    F: Write,
{
    f.write_str("subroutine")?;

    if !types.is_empty() {
        f.write_str("(")?;

        let mut types_iter = types.iter();
        let first = types_iter.next().unwrap();

        show_type_specifier(f, first, state)?;

        for type_name in types_iter {
            f.write_str(", ")?;
            show_type_specifier(f, type_name, state)?;
        }

        f.write_str(")")?;
    }

    Ok(())
}

/// Transpile a layout_qualifier to GLSL
pub fn show_layout_qualifier<F>(
    f: &mut F,
    l: &ast::LayoutQualifier,
    state: &mut FormattingState<'_>,
) -> std::fmt::Result
where
    F: Write,
{
    let mut qualifiers = l.ids.iter();
    let first = qualifiers.next().unwrap();

    f.write_str("layout (")?;
    show_layout_qualifier_spec(f, first, state)?;

    for qual_spec in qualifiers {
        f.write_str(", ")?;
        show_layout_qualifier_spec(f, qual_spec, state)?;
    }

    f.write_str(")")
}

/// Transpile a layout_qualifier_spec to GLSL
pub fn show_layout_qualifier_spec<F>(
    f: &mut F,
    l: &ast::LayoutQualifierSpec,
    state: &mut FormattingState<'_>,
) -> std::fmt::Result
where
    F: Write,
{
    match *l {
        ast::LayoutQualifierSpec::Identifier(ref i, Some(ref e)) => {
            write!(f, "{} = ", i)?;
            show_expr(f, &e, state)
        }
        ast::LayoutQualifierSpec::Identifier(ref i, None) => show_identifier(f, &i, state),
        ast::LayoutQualifierSpec::Shared => f.write_str("shared"),
    }
}

/// Transpile a precision_qualifier to GLSL
pub fn show_precision_qualifier<F>(
    f: &mut F,
    p: &ast::PrecisionQualifier,
    _: &mut FormattingState<'_>,
) -> std::fmt::Result
where
    F: Write,
{
    match *p {
        ast::PrecisionQualifier::High => f.write_str("highp"),
        ast::PrecisionQualifier::Medium => f.write_str("mediump"),
        ast::PrecisionQualifier::Low => f.write_str("low"),
    }
}

/// Transpile an interpolation_qualifier to GLSL
pub fn show_interpolation_qualifier<F>(
    f: &mut F,
    i: &ast::InterpolationQualifier,
    _: &mut FormattingState<'_>,
) -> std::fmt::Result
where
    F: Write,
{
    match *i {
        ast::InterpolationQualifier::Smooth => f.write_str("smooth"),
        ast::InterpolationQualifier::Flat => f.write_str("flat"),
        ast::InterpolationQualifier::NoPerspective => f.write_str("noperspective"),
    }
}

/// Transpile a float<F>(f: &mut F, x: f32, _: &mut FormattingState to GLSL
pub fn show_float<F>(f: &mut F, x: f32, _: &mut FormattingState<'_>) -> std::fmt::Result
where
    F: Write,
{
    if x.fract() == 0. {
        write!(f, "{}.", x)
    } else {
        write!(f, "{}", x)
    }
}

/// Transpile a double<F>(f: &mut F, x: f64, _: &mut FormattingState to GLSL
pub fn show_double<F>(f: &mut F, x: f64, _: &mut FormattingState<'_>) -> std::fmt::Result
where
    F: Write,
{
    if x.fract() == 0. {
        write!(f, "{}.lf", x)
    } else {
        write!(f, "{}lf", x)
    }
}

/// Transpile an expr to GLSL
pub fn show_expr<F>(
    f: &mut F,
    expr: &ast::Expr,
    state: &mut FormattingState<'_>,
) -> std::fmt::Result
where
    F: Write,
{
    match *expr {
        ast::Expr::Variable(ref i) => show_identifier(f, &i, state),
        ast::Expr::IntConst(ref x) => write!(f, "{}", x),
        ast::Expr::UIntConst(ref x) => write!(f, "{}u", x),
        ast::Expr::BoolConst(ref x) => write!(f, "{}", x),
        ast::Expr::FloatConst(ref x) => show_float(f, *x, state),
        ast::Expr::DoubleConst(ref x) => show_double(f, *x, state),
        ast::Expr::Unary(ref op, ref e) => {
            // Note: all unary ops are right-to-left associative
            show_unary_op(f, &op, state)?;

            if e.precedence() > op.precedence() {
                f.write_str("(")?;
                show_expr(f, &e, state)?;
                f.write_str(")")
            } else if let ast::Expr::Unary(eop, _) = &**e {
                // Prevent double-unary plus/minus turning into inc/dec
                if eop == op && (*eop == ast::UnaryOp::Add || *eop == ast::UnaryOp::Minus) {
                    f.write_str("(")?;
                    show_expr(f, &e, state)?;
                    f.write_str(")")
                } else {
                    show_expr(f, &e, state)
                }
            } else {
                show_expr(f, &e, state)
            }
        }
        ast::Expr::Binary(ref op, ref l, ref r) => {
            // Note: all binary ops are left-to-right associative (<= for left part)

            if l.precedence() <= op.precedence() {
                show_expr(f, &l, state)?;
            } else {
                f.write_str("(")?;
                show_expr(f, &l, state)?;
                f.write_str(")")?;
            }

            show_binary_op(f, &op, state)?;

            if r.precedence() < op.precedence() {
                show_expr(f, &r, state)
            } else {
                f.write_str("(")?;
                show_expr(f, &r, state)?;
                f.write_str(")")
            }
        }
        ast::Expr::Ternary(ref c, ref st, ref e) => {
            // Note: ternary is right-to-left associative (<= for right part)

            if c.precedence() < expr.precedence() {
                show_expr(f, &c, state)?;
            } else {
                f.write_str("(")?;
                show_expr(f, &c, state)?;
                f.write_str(")")?;
            }
            f.write_str(" ? ")?;
            show_expr(f, &st, state)?;
            f.write_str(" : ")?;
            if e.precedence() <= expr.precedence() {
                show_expr(f, &e, state)
            } else {
                f.write_str("(")?;
                show_expr(f, &e, state)?;
                f.write_str(")")
            }
        }
        ast::Expr::Assignment(ref v, ref op, ref e) => {
            // Note: all assignment ops are right-to-left associative

            if v.precedence() < op.precedence() {
                show_expr(f, &v, state)?;
            } else {
                f.write_str("(")?;
                show_expr(f, &v, state)?;
                f.write_str(")")?;
            }

            show_assignment_op(f, &op, state)?;

            if e.precedence() <= op.precedence() {
                show_expr(f, &e, state)
            } else {
                f.write_str("(")?;
                show_expr(f, &e, state)?;
                f.write_str(")")
            }
        }
        ast::Expr::Bracket(ref e, ref a) => {
            // Note: bracket is left-to-right associative

            if e.precedence() <= expr.precedence() {
                show_expr(f, &e, state)?;
            } else {
                f.write_str("(")?;
                show_expr(f, &e, state)?;
                f.write_str(")")?;
            }

            show_expr(f, &a, state)
        }
        ast::Expr::FunCall(ref fun, ref args) => {
            show_function_identifier(f, &fun, state)?;
            f.write_str("(")?;

            if !args.is_empty() {
                let mut args_iter = args.iter();
                let first = args_iter.next().unwrap();
                show_expr(f, first, state)?;

                for e in args_iter {
                    f.write_str(", ")?;
                    show_expr(f, e, state)?;
                }
            }

            f.write_str(")")
        }
        ast::Expr::Dot(ref e, ref i) => {
            // Note: dot is left-to-right associative

            if e.precedence() <= expr.precedence() {
                show_expr(f, &e, state)?;
            } else {
                f.write_str("(")?;
                show_expr(f, &e, state)?;
                f.write_str(")")?;
            }
            f.write_str(".")?;
            show_identifier(f, &i, state)
        }
        ast::Expr::PostInc(ref e) => {
            // Note: post-increment is right-to-left associative

            if e.precedence() < expr.precedence() {
                show_expr(f, &e, state)?;
            } else {
                f.write_str("(")?;
                show_expr(f, &e, state)?;
                f.write_str(")")?;
            }

            f.write_str("++")
        }
        ast::Expr::PostDec(ref e) => {
            // Note: post-decrement is right-to-left associative

            if e.precedence() < expr.precedence() {
                show_expr(f, &e, state)?;
            } else {
                f.write_str("(")?;
                show_expr(f, &e, state)?;
                f.write_str(")")?;
            }

            f.write_str("--")
        }
        ast::Expr::Comma(ref a, ref b) => {
            // Note: comma is left-to-right associative

            if a.precedence() <= expr.precedence() {
                show_expr(f, &a, state)?;
            } else {
                f.write_str("(")?;
                show_expr(f, &a, state)?;
                f.write_str(")")?;
            }

            f.write_str(", ")?;

            if b.precedence() < expr.precedence() {
                show_expr(f, &b, state)
            } else {
                f.write_str("(")?;
                show_expr(f, &b, state)?;
                f.write_str(")")
            }
        }
    }
}

/// Transpile a path<F>(f: &mut F, path: &ast::Path, _: &mut FormattingState to GLSL
pub fn show_path<F>(f: &mut F, path: &ast::Path, _: &mut FormattingState<'_>) -> std::fmt::Result
where
    F: Write,
{
    match path {
        ast::Path::Absolute(s) => write!(f, "<{}>", s),
        ast::Path::Relative(s) => write!(f, "\"{}\"", s),
    }
}

/// Transpile an unary_op to GLSL
pub fn show_unary_op<F>(
    f: &mut F,
    op: &ast::UnaryOp,
    _: &mut FormattingState<'_>,
) -> std::fmt::Result
where
    F: Write,
{
    match *op {
        ast::UnaryOp::Inc => f.write_str("++"),
        ast::UnaryOp::Dec => f.write_str("--"),
        ast::UnaryOp::Add => f.write_str("+"),
        ast::UnaryOp::Minus => f.write_str("-"),
        ast::UnaryOp::Not => f.write_str("!"),
        ast::UnaryOp::Complement => f.write_str("~"),
    }
}

/// Transpile a binary_op to GLSL
pub fn show_binary_op<F>(
    f: &mut F,
    op: &ast::BinaryOp,
    state: &mut FormattingState<'_>,
) -> std::fmt::Result
where
    F: Write,
{
    match *op {
        ast::BinaryOp::Or => state.write_binary_op(f, "||"),
        ast::BinaryOp::Xor => state.write_binary_op(f, "^^"),
        ast::BinaryOp::And => state.write_binary_op(f, "&&"),
        ast::BinaryOp::BitOr => state.write_binary_op(f, "|"),
        ast::BinaryOp::BitXor => state.write_binary_op(f, "^"),
        ast::BinaryOp::BitAnd => state.write_binary_op(f, "&"),
        ast::BinaryOp::Equal => state.write_binary_op(f, "=="),
        ast::BinaryOp::NonEqual => state.write_binary_op(f, "!="),
        ast::BinaryOp::LT => state.write_binary_op(f, "<"),
        ast::BinaryOp::GT => state.write_binary_op(f, ">"),
        ast::BinaryOp::LTE => state.write_binary_op(f, "<="),
        ast::BinaryOp::GTE => state.write_binary_op(f, ">="),
        ast::BinaryOp::LShift => state.write_binary_op(f, "<<"),
        ast::BinaryOp::RShift => state.write_binary_op(f, ">>"),
        ast::BinaryOp::Add => state.write_binary_op(f, "+"),
        ast::BinaryOp::Sub => state.write_binary_op(f, "-"),
        ast::BinaryOp::Mult => state.write_binary_op(f, "*"),
        ast::BinaryOp::Div => state.write_binary_op(f, "/"),
        ast::BinaryOp::Mod => state.write_binary_op(f, "%"),
    }
}

/// Transpile an assignment_op to GLSL
pub fn show_assignment_op<F>(
    f: &mut F,
    op: &ast::AssignmentOp,
    state: &mut FormattingState<'_>,
) -> std::fmt::Result
where
    F: Write,
{
    match *op {
        ast::AssignmentOp::Equal => state.write_binary_op(f, "="),
        ast::AssignmentOp::Mult => state.write_binary_op(f, "*="),
        ast::AssignmentOp::Div => state.write_binary_op(f, "/="),
        ast::AssignmentOp::Mod => state.write_binary_op(f, "%="),
        ast::AssignmentOp::Add => state.write_binary_op(f, "+="),
        ast::AssignmentOp::Sub => state.write_binary_op(f, "-="),
        ast::AssignmentOp::LShift => state.write_binary_op(f, "<<="),
        ast::AssignmentOp::RShift => state.write_binary_op(f, ">>="),
        ast::AssignmentOp::And => state.write_binary_op(f, "&="),
        ast::AssignmentOp::Xor => state.write_binary_op(f, "^="),
        ast::AssignmentOp::Or => state.write_binary_op(f, "|="),
    }
}

/// Transpile a function_identifier to GLSL
pub fn show_function_identifier<F>(
    f: &mut F,
    i: &ast::FunIdentifier,
    state: &mut FormattingState<'_>,
) -> std::fmt::Result
where
    F: Write,
{
    match *i {
        ast::FunIdentifier::TypeSpecifier(ref n) => show_type_specifier(f, &n, state),
        ast::FunIdentifier::Expr(ref e) => show_expr(f, &*e, state),
    }
}

/// Transpile a declaration to GLSL
pub fn show_declaration<F>(
    f: &mut F,
    d: &ast::Declaration,
    state: &mut FormattingState<'_>,
) -> std::fmt::Result
where
    F: Write,
{
    match **d {
        ast::DeclarationData::FunctionPrototype(ref proto) => {
            show_function_prototype(f, &proto, state)?;
            state.write_declaration_terminator(f)
        }
        ast::DeclarationData::InitDeclaratorList(ref list) => {
            show_init_declarator_list(f, &list, state)?;
            state.write_declaration_terminator(f)
        }
        ast::DeclarationData::Precision(ref qual, ref ty) => {
            show_precision_qualifier(f, &qual, state)?;
            show_type_specifier(f, &ty, state)?;
            state.write_declaration_terminator(f)
        }
        ast::DeclarationData::Block(ref block) => {
            show_block(f, &block, state)?;
            state.write_declaration_terminator(f)
        }
    }
}

/// Transpile a function_prototype to GLSL
pub fn show_function_prototype<F>(
    f: &mut F,
    fp: &ast::FunctionPrototype,
    state: &mut FormattingState<'_>,
) -> std::fmt::Result
where
    F: Write,
{
    show_fully_specified_type(f, &fp.ty, state)?;
    f.write_str(" ")?;
    show_identifier(f, &fp.name, state)?;

    f.write_str("(")?;

    if !fp.parameters.is_empty() {
        let mut iter = fp.parameters.iter();
        let first = iter.next().unwrap();
        show_function_parameter_declaration(f, first, state)?;

        for param in iter {
            f.write_str(", ")?;
            show_function_parameter_declaration(f, param, state)?;
        }
    }

    f.write_str(")")
}
/// Transpile a function_parameter_declaration to GLSL
pub fn show_function_parameter_declaration<F>(
    f: &mut F,
    p: &ast::FunctionParameterDeclaration,
    state: &mut FormattingState<'_>,
) -> std::fmt::Result
where
    F: Write,
{
    match **p {
        ast::FunctionParameterDeclarationData::Named(ref qual, ref fpd) => {
            if let Some(ref q) = *qual {
                show_type_qualifier(f, q, state)?;
                f.write_str(" ")?;
            }

            show_function_parameter_declarator(f, fpd, state)
        }
        ast::FunctionParameterDeclarationData::Unnamed(ref qual, ref ty) => {
            if let Some(ref q) = *qual {
                show_type_qualifier(f, q, state)?;
                f.write_str(" ")?;
            }

            show_type_specifier(f, ty, state)
        }
    }
}

/// Transpile a function_parameter_declarator to GLSL
pub fn show_function_parameter_declarator<F>(
    f: &mut F,
    p: &ast::FunctionParameterDeclarator,
    state: &mut FormattingState<'_>,
) -> std::fmt::Result
where
    F: Write,
{
    show_type_specifier(f, &p.ty, state)?;
    f.write_str(" ")?;
    show_arrayed_identifier(f, &p.ident, state)
}

/// Transpile an init_declarator_list to GLSL
pub fn show_init_declarator_list<F>(
    f: &mut F,
    i: &ast::InitDeclaratorList,
    state: &mut FormattingState<'_>,
) -> std::fmt::Result
where
    F: Write,
{
    show_single_declaration(f, &i.head, state)?;

    for decl in &i.tail {
        f.write_str(", ")?;
        show_single_declaration_no_type(f, decl, state)?;
    }

    Ok(())
}

/// Transpile a single_declaration to GLSL
pub fn show_single_declaration<F>(
    f: &mut F,
    d: &ast::SingleDeclaration,
    state: &mut FormattingState<'_>,
) -> std::fmt::Result
where
    F: Write,
{
    show_fully_specified_type(f, &d.ty, state)?;

    if let Some(ref name) = d.name {
        f.write_str(" ")?;
        show_identifier(f, name, state)?;
    }

    if let Some(ref arr_spec) = d.array_specifier {
        show_array_spec(f, arr_spec, state)?;
    }

    if let Some(ref initializer) = d.initializer {
        f.write_str(" = ")?;
        show_initializer(f, initializer, state)?;
    }

    Ok(())
}

/// Transpile a single_declaration_no_type to GLSL
pub fn show_single_declaration_no_type<F>(
    f: &mut F,
    d: &ast::SingleDeclarationNoType,
    state: &mut FormattingState<'_>,
) -> std::fmt::Result
where
    F: Write,
{
    show_arrayed_identifier(f, &d.ident, state)?;

    if let Some(ref initializer) = d.initializer {
        f.write_str(" = ")?;
        show_initializer(f, initializer, state)?;
    }

    Ok(())
}

/// Transpile an initializer to GLSL
pub fn show_initializer<F>(
    f: &mut F,
    i: &ast::Initializer,
    state: &mut FormattingState<'_>,
) -> std::fmt::Result
where
    F: Write,
{
    match *i {
        ast::Initializer::Simple(ref e) => show_expr(f, e, state),
        ast::Initializer::List(ref list) => {
            let mut iter = list.iter();
            let first = iter.next().unwrap();

            f.write_str("{ ")?;
            show_initializer(f, first, state)?;

            for ini in iter {
                f.write_str(", ")?;
                show_initializer(f, ini, state)?;
            }

            f.write_str(" }")
        }
    }
}

/// Transpile a block<F>(f: &mut F, b: &ast::Block, state: &mut FormattingState to GLSL
pub fn show_block<F>(f: &mut F, b: &ast::Block, state: &mut FormattingState<'_>) -> std::fmt::Result
where
    F: Write,
{
    show_type_qualifier(f, &b.qualifier, state)?;
    f.write_str(" ")?;
    show_identifier(f, &b.name, state)?;
    f.write_str(" ")?;

    state.enter_block(f)?;

    for field in &b.fields {
        state.flush_line(f)?;
        show_struct_field(f, field, state)?;
        state.write_struct_field_separator(f)?;
    }

    state.exit_block(f)?;

    if let Some(ref ident) = b.identifier {
        show_arrayed_identifier(f, ident, state)?;
    }

    Ok(())
}

/// Transpile a function_definition to GLSL
pub fn show_function_definition<F>(
    f: &mut F,
    fd: &ast::FunctionDefinition,
    state: &mut FormattingState<'_>,
) -> std::fmt::Result
where
    F: Write,
{
    show_function_prototype(f, &fd.prototype, state)?;
    f.write_str(" ")?;
    show_compound_statement(f, &fd.statement, state)?;
    state.flush_line(f)?;
    state.write_function_definition_terminator(f)
}

/// Transpile a compound_statement to GLSL
pub fn show_compound_statement<F>(
    f: &mut F,
    cst: &ast::CompoundStatement,
    state: &mut FormattingState<'_>,
) -> std::fmt::Result
where
    F: Write,
{
    state.enter_block(f)?;

    for st in &cst.statement_list {
        show_statement(f, st, state)?;
    }

    state.exit_block(f)?;

    Ok(())
}

/// Transpile a statement to GLSL
pub fn show_statement<F>(
    f: &mut F,
    st: &ast::Statement,
    state: &mut FormattingState<'_>,
) -> std::fmt::Result
where
    F: Write,
{
    state.flush_line(f)?;

    match **st {
        ast::StatementData::Declaration(ref d) => show_declaration(f, d, state),
        ast::StatementData::Expression(ref e) => show_expression_statement(f, e, state),
        ast::StatementData::Selection(ref st) => show_selection_statement(f, st, state),
        ast::StatementData::Switch(ref st) => show_switch_statement(f, st, state),
        ast::StatementData::CaseLabel(ref cl) => show_case_label(f, cl, state),
        ast::StatementData::Iteration(ref i) => show_iteration_statement(f, i, state),
        ast::StatementData::Jump(ref j) => show_jump_statement(f, j, state),
        ast::StatementData::Compound(ref c) => show_compound_statement(f, c, state),
    }
}

/// Transpile an expression_statement to GLSL
pub fn show_expression_statement<F>(
    f: &mut F,
    est: &ast::ExprStatement,
    state: &mut FormattingState<'_>,
) -> std::fmt::Result
where
    F: Write,
{
    if let Some(ref e) = est.0 {
        show_expr(f, e, state)?;
    }

    state.write_statement_terminator(f)
}

/// Transpile a selection_statement to GLSL
pub fn show_selection_statement<F>(
    f: &mut F,
    sst: &ast::SelectionStatement,
    state: &mut FormattingState<'_>,
) -> std::fmt::Result
where
    F: Write,
{
    f.write_str("if (")?;
    show_expr(f, &sst.cond, state)?;
    f.write_str(") ")?;
    show_selection_rest_statement(f, &sst.rest, state)
}

/// Transpile a selection_rest_statement to GLSL
pub fn show_selection_rest_statement<F>(
    f: &mut F,
    sst: &ast::SelectionRestStatement,
    state: &mut FormattingState<'_>,
) -> std::fmt::Result
where
    F: Write,
{
    match *sst {
        ast::SelectionRestStatement::Statement(ref if_st) => show_statement(f, if_st, state),
        ast::SelectionRestStatement::Else(ref if_st, ref else_st) => {
            show_statement(f, if_st, state)?;
            f.write_str(" else ")?;
            // TODO: This should be configurable instead of relying on show_statement's calling
            // flush_line
            state.consume_newline();
            show_statement(f, else_st, state)
        }
    }
}

/// Transpile a switch_statement to GLSL
pub fn show_switch_statement<F>(
    f: &mut F,
    sst: &ast::SwitchStatement,
    state: &mut FormattingState<'_>,
) -> std::fmt::Result
where
    F: Write,
{
    f.write_str("switch (")?;
    show_expr(f, &sst.head, state)?;
    f.write_str(") {\n")?;

    for st in &sst.body {
        show_statement(f, st, state)?;
    }

    f.write_str("}\n")
}

/// Transpile a case_label to GLSL
pub fn show_case_label<F>(
    f: &mut F,
    cl: &ast::CaseLabel,
    state: &mut FormattingState<'_>,
) -> std::fmt::Result
where
    F: Write,
{
    match *cl {
        ast::CaseLabel::Case(ref e) => {
            f.write_str("case ")?;
            show_expr(f, e, state)?;
            f.write_str(":\n")
        }
        ast::CaseLabel::Def => f.write_str("default:\n"),
    }
}

/// Transpile an iteration_statement to GLSL
pub fn show_iteration_statement<F>(
    f: &mut F,
    ist: &ast::IterationStatement,
    state: &mut FormattingState<'_>,
) -> std::fmt::Result
where
    F: Write,
{
    match *ist {
        ast::IterationStatement::While(ref cond, ref body) => {
            f.write_str("while (")?;
            show_condition(f, cond, state)?;
            f.write_str(") ")?;
            show_statement(f, body, state)
        }
        ast::IterationStatement::DoWhile(ref body, ref cond) => {
            f.write_str("do ")?;
            show_statement(f, body, state)?;
            f.write_str(" while (")?;
            show_expr(f, cond, state)?;
            f.write_str(")")?;
            state.write_statement_terminator(f)
        }
        ast::IterationStatement::For(ref init, ref rest, ref body) => {
            f.write_str("for (")?;
            show_for_init_statement(f, init, state)?;
            state.flush_space(f)?;
            show_for_rest_statement(f, rest, state)?;
            f.write_str(") ")?;
            show_statement(f, body, state)
        }
    }
}

/// Transpile a condition to GLSL
pub fn show_condition<F>(
    f: &mut F,
    c: &ast::Condition,
    state: &mut FormattingState<'_>,
) -> std::fmt::Result
where
    F: Write,
{
    match *c {
        ast::Condition::Expr(ref e) => show_expr(f, e, state),
        ast::Condition::Assignment(ref ty, ref name, ref initializer) => {
            show_fully_specified_type(f, ty, state)?;
            f.write_str(" ")?;
            show_identifier(f, name, state)?;
            f.write_str(" = ")?;
            show_initializer(f, initializer, state)
        }
    }
}

/// Transpile a for_init_statement to GLSL
pub fn show_for_init_statement<F>(
    f: &mut F,
    i: &ast::ForInitStatement,
    state: &mut FormattingState<'_>,
) -> std::fmt::Result
where
    F: Write,
{
    match *i {
        ast::ForInitStatement::Expression(ref expr) => {
            if let Some(ref e) = *expr {
                show_expr(f, e, state)?;
            }

            Ok(())
        }
        ast::ForInitStatement::Declaration(ref d) => show_declaration(f, d, state),
    }
}

/// Transpile a for_rest_statement to GLSL
pub fn show_for_rest_statement<F>(
    f: &mut F,
    r: &ast::ForRestStatement,
    state: &mut FormattingState<'_>,
) -> std::fmt::Result
where
    F: Write,
{
    if let Some(ref cond) = r.condition {
        show_condition(f, cond, state)?;
    }

    f.write_str("; ")?;

    if let Some(ref e) = r.post_expr {
        show_expr(f, e, state)?;
    }

    Ok(())
}

/// Transpile a jump_statement to GLSL
pub fn show_jump_statement<F>(
    f: &mut F,
    j: &ast::JumpStatement,
    state: &mut FormattingState<'_>,
) -> std::fmt::Result
where
    F: Write,
{
    match *j {
        ast::JumpStatement::Continue => f.write_str("continue")?,
        ast::JumpStatement::Break => f.write_str("break")?,
        ast::JumpStatement::Discard => f.write_str("discard")?,
        ast::JumpStatement::Return(ref e) => {
            f.write_str("return ")?;
            if let Some(e) = e {
                show_expr(f, e, state)?;
            }
        }
    }

    state.write_statement_terminator(f)
}

/// Transpile a preprocessor to GLSL
pub fn show_preprocessor<F>(
    f: &mut F,
    pp: &ast::Preprocessor,
    state: &mut FormattingState<'_>,
) -> std::fmt::Result
where
    F: Write,
{
    match **pp {
        ast::PreprocessorData::Define(ref pd) => show_preprocessor_define(f, pd, state),
        ast::PreprocessorData::Else => show_preprocessor_else(f, state),
        ast::PreprocessorData::ElseIf(ref pei) => show_preprocessor_elseif(f, pei, state),
        ast::PreprocessorData::EndIf => show_preprocessor_endif(f, state),
        ast::PreprocessorData::Error(ref pe) => show_preprocessor_error(f, pe, state),
        ast::PreprocessorData::If(ref pi) => show_preprocessor_if(f, pi, state),
        ast::PreprocessorData::IfDef(ref pid) => show_preprocessor_ifdef(f, pid, state),
        ast::PreprocessorData::IfNDef(ref pind) => show_preprocessor_ifndef(f, pind, state),
        ast::PreprocessorData::Include(ref pi) => show_preprocessor_include(f, pi, state),
        ast::PreprocessorData::Line(ref pl) => show_preprocessor_line(f, pl, state),
        ast::PreprocessorData::Pragma(ref pp) => show_preprocessor_pragma(f, pp, state),
        ast::PreprocessorData::Undef(ref pu) => show_preprocessor_undef(f, pu, state),
        ast::PreprocessorData::Version(ref pv) => show_preprocessor_version(f, pv, state),
        ast::PreprocessorData::Extension(ref pe) => show_preprocessor_extension(f, pe, state),
    }
}

/// Transpile a preprocessor_define to GLSL
pub fn show_preprocessor_define<F>(
    f: &mut F,
    pd: &ast::PreprocessorDefine,
    _: &mut FormattingState<'_>,
) -> std::fmt::Result
where
    F: Write,
{
    match *pd {
        ast::PreprocessorDefine::ObjectLike {
            ref ident,
            ref value,
        } => writeln!(f, "#define {} {}", ident, value),

        ast::PreprocessorDefine::FunctionLike {
            ref ident,
            ref args,
            ref value,
        } => {
            write!(f, "#define {}(", ident)?;

            if !args.is_empty() {
                write!(f, "{}", &args[0])?;

                for arg in &args[1..args.len()] {
                    write!(f, ", {}", arg)?;
                }
            }

            writeln!(f, ") {}", value)
        }
    }
}

/// Transpile a preprocessor_else<F>(f: &mut F, _: &mut FormattingState to GLSL
pub fn show_preprocessor_else<F>(f: &mut F, _: &mut FormattingState<'_>) -> std::fmt::Result
where
    F: Write,
{
    f.write_str("#else\n")
}

/// Transpile a preprocessor_elseif to GLSL
pub fn show_preprocessor_elseif<F>(
    f: &mut F,
    pei: &ast::PreprocessorElseIf,
    _: &mut FormattingState<'_>,
) -> std::fmt::Result
where
    F: Write,
{
    writeln!(f, "#elseif {}", pei.condition)
}

/// Transpile a preprocessor_error to GLSL
pub fn show_preprocessor_error<F>(
    f: &mut F,
    pe: &ast::PreprocessorError,
    _: &mut FormattingState<'_>,
) -> std::fmt::Result
where
    F: Write,
{
    writeln!(f, "#error {}", pe.message)
}

/// Transpile a preprocessor_endif<F>(f: &mut F, _: &mut FormattingState to GLSL
pub fn show_preprocessor_endif<F>(f: &mut F, _: &mut FormattingState<'_>) -> std::fmt::Result
where
    F: Write,
{
    f.write_str("#endif\n")
}

/// Transpile a preprocessor_if to GLSL
pub fn show_preprocessor_if<F>(
    f: &mut F,
    pi: &ast::PreprocessorIf,
    _: &mut FormattingState<'_>,
) -> std::fmt::Result
where
    F: Write,
{
    write!(f, "#if {}", pi.condition)
}

/// Transpile a preprocessor_ifdef to GLSL
pub fn show_preprocessor_ifdef<F>(
    f: &mut F,
    pid: &ast::PreprocessorIfDef,
    state: &mut FormattingState<'_>,
) -> std::fmt::Result
where
    F: Write,
{
    f.write_str("#ifdef ")?;
    show_identifier(f, &pid.ident, state)?;
    writeln!(f)
}

/// Transpile a preprocessor_ifndef to GLSL
pub fn show_preprocessor_ifndef<F>(
    f: &mut F,
    pind: &ast::PreprocessorIfNDef,
    state: &mut FormattingState<'_>,
) -> std::fmt::Result
where
    F: Write,
{
    f.write_str("#ifndef ")?;
    show_identifier(f, &pind.ident, state)?;
    writeln!(f)
}

/// Transpile a preprocessor_include to GLSL
pub fn show_preprocessor_include<F>(
    f: &mut F,
    pi: &ast::PreprocessorInclude,
    state: &mut FormattingState<'_>,
) -> std::fmt::Result
where
    F: Write,
{
    f.write_str("#include ")?;
    show_path(f, &pi.path, state)?;
    writeln!(f)
}

/// Transpile a preprocessor_line to GLSL
pub fn show_preprocessor_line<F>(
    f: &mut F,
    pl: &ast::PreprocessorLine,
    _: &mut FormattingState<'_>,
) -> std::fmt::Result
where
    F: Write,
{
    write!(f, "#line {}", pl.line)?;
    if let Some(source_string_number) = pl.source_string_number {
        write!(f, " {}", source_string_number)?;
    }
    writeln!(f)
}

/// Transpile a preprocessor_pragma to GLSL
pub fn show_preprocessor_pragma<F>(
    f: &mut F,
    pp: &ast::PreprocessorPragma,
    _: &mut FormattingState<'_>,
) -> std::fmt::Result
where
    F: Write,
{
    writeln!(f, "#pragma {}", pp.command)
}

/// Transpile a preprocessor_undef to GLSL
pub fn show_preprocessor_undef<F>(
    f: &mut F,
    pud: &ast::PreprocessorUndef,
    state: &mut FormattingState<'_>,
) -> std::fmt::Result
where
    F: Write,
{
    f.write_str("#undef ")?;
    show_identifier(f, &pud.name, state)?;
    writeln!(f)
}

/// Transpile a preprocessor_version to GLSL
pub fn show_preprocessor_version<F>(
    f: &mut F,
    pv: &ast::PreprocessorVersion,
    _: &mut FormattingState<'_>,
) -> std::fmt::Result
where
    F: Write,
{
    write!(f, "#version {}", pv.version)?;

    if let Some(ref profile) = pv.profile {
        match *profile {
            ast::PreprocessorVersionProfile::Core => {
                f.write_str(" core")?;
            }
            ast::PreprocessorVersionProfile::Compatibility => {
                f.write_str(" compatibility")?;
            }
            ast::PreprocessorVersionProfile::ES => {
                f.write_str(" es")?;
            }
        }
    }

    writeln!(f)
}

/// Transpile a preprocessor_extension to GLSL
pub fn show_preprocessor_extension<F>(
    f: &mut F,
    pe: &ast::PreprocessorExtension,
    _: &mut FormattingState<'_>,
) -> std::fmt::Result
where
    F: Write,
{
    f.write_str("#extension ")?;

    match pe.name {
        ast::PreprocessorExtensionName::All => {
            f.write_str("all")?;
        }
        ast::PreprocessorExtensionName::Specific(ref n) => {
            f.write_str(n)?;
        }
    }

    if let Some(ref behavior) = pe.behavior {
        match *behavior {
            ast::PreprocessorExtensionBehavior::Require => {
                f.write_str(" : require")?;
            }
            ast::PreprocessorExtensionBehavior::Enable => {
                f.write_str(" : enable")?;
            }
            ast::PreprocessorExtensionBehavior::Warn => {
                f.write_str(" : warn")?;
            }
            ast::PreprocessorExtensionBehavior::Disable => {
                f.write_str(" : disable")?;
            }
        }
    }

    writeln!(f)
}

/// Transpile an external_declaration to GLSL
pub fn show_external_declaration<F>(
    f: &mut F,
    ed: &ast::ExternalDeclaration,
    mut state: &mut FormattingState<'_>,
) -> std::fmt::Result
where
    F: Write,
{
    state.flush_line(f)?;

    match **ed {
        ast::ExternalDeclarationData::Preprocessor(ref pp) => show_preprocessor(f, pp, &mut state),
        ast::ExternalDeclarationData::FunctionDefinition(ref fd) => {
            show_function_definition(f, fd, &mut state)
        }
        ast::ExternalDeclarationData::Declaration(ref d) => show_declaration(f, d, &mut state),
    }
}

/// Transpile a translation_unit to GLSL
pub fn show_translation_unit<F>(
    f: &mut F,
    tu: &ast::TranslationUnit,
    mut state: FormattingState<'_>,
) -> std::fmt::Result
where
    F: Write,
{
    for ed in &tu.0 {
        show_external_declaration(f, ed, &mut state)?;
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse::Parsable;
    use expect_test::{expect, Expect};

    fn check_expr(src: &str, expected: Expect) {
        let expr = ast::Expr::parse(src).unwrap();
        let actual = to_string(&expr);
        expected.assert_eq(&actual);
    }

    fn to_string(e: &ast::Expr) -> String {
        let mut state = String::new();
        show_expr(&mut state, e, &mut FormattingState::default()).unwrap();
        state
    }

    #[test]
    fn unary_parentheses() {
        check_expr("-a", expect![["-a"]]);
        check_expr("-(a + b)", expect![[r#"-(a + b)"#]]);
        check_expr("-a.x", expect![["-a.x"]]);

        check_expr("-(-a)", expect![["-(-a)"]]);
        check_expr("+(+a)", expect![["+(+a)"]]);
        check_expr("~~a", expect![["~~a"]]);
        check_expr("--a", expect![["--a"]]);
        check_expr("++a", expect![["++a"]]);
        check_expr("+-a", expect![["+-a"]]);
    }

    #[test]
    fn binary_parentheses() {
        check_expr("a + b", expect![[r#"a + b"#]]);
        check_expr("a * b + c", expect![[r#"a * b + c"#]]);
        check_expr("(a + b) * c", expect![[r#"(a + b) * c"#]]);
        check_expr("a + (b * c)", expect![[r#"a + b * c"#]]);
        check_expr("a * (b + c)", expect![[r#"a * (b + c)"#]]);
        check_expr("(a * b) * c", expect![[r#"a * b * c"#]]);
        check_expr("a * (b * c)", expect![[r#"a * (b * c)"#]]);
        check_expr("a&&b&&c", expect![[r#"a && b && c"#]]);
        check_expr(
            "n - p > 0. && u.y < n && u.y > p",
            expect![[r#"n - p > 0. && u.y < n && u.y > p"#]],
        );
    }

    #[test]
    fn ternary_parentheses() {
        check_expr("a ? b : c ? d : e", expect![["a ? b : c ? d : e"]]);
        check_expr("(a ? b : c) ? d : e", expect![["(a ? b : c) ? d : e"]]);
    }

    #[test]
    fn assignment_parentheses() {
        check_expr("a = b = c", expect![["a = b = c"]]);
        check_expr("(a = b) = c", expect![["(a = b) = c"]]);
    }

    #[test]
    fn dot_parentheses() {
        check_expr("a.x", expect![["a.x"]]);
        check_expr("(a + b).x", expect![[r#"(a + b).x"#]]);
    }

    #[test]
    fn test_parentheses() {
        const SRC: &str = r#"vec2 main() {
float n = 0.;
float p = 0.;
float u = vec2(0., 0.);
if (n-p>0.&&u.y<n&&u.y>p) {
}
return u;
}
"#;

        let mut s = String::new();
        show_function_definition(
            &mut s,
            &ast::FunctionDefinition::parse(SRC).unwrap(),
            &mut FormattingState::default(),
        )
        .unwrap();

        let expected = expect![[r#"
            vec2 main() {
                float n = 0.;
                float p = 0.;
                float u = vec2(0., 0.);
                if (n - p > 0. && u.y < n && u.y > p) {
                }
                return u;
            }
        "#]];

        expected.assert_eq(&s);
    }

    #[test]
    fn roundtrip_glsl_complex_expr() {
        use lang_util::{assert_ceq, node::NodeContent};

        let zero = ast::Expr::DoubleConst(0.);
        let ray = ast::Expr::Variable("ray".into_node());
        let raydir = ast::Expr::Dot(Box::new(ray), "dir".into_node());
        let vec4 = ast::Expr::FunCall(
            ast::FunIdentifier::TypeSpecifier(ast::TypeSpecifierNonArray::Vec4.into()),
            vec![raydir, zero],
        );
        let view = ast::Expr::variable("view");
        let iview = ast::Expr::FunCall(ast::FunIdentifier::ident("inverse"), vec![view]);
        let mul = ast::Expr::Binary(ast::BinaryOp::Mult, Box::new(iview), Box::new(vec4));
        let xyz = ast::Expr::Dot(Box::new(mul), "xyz".into_node());
        let input = ast::Expr::FunCall(ast::FunIdentifier::ident("normalize"), vec![xyz]);

        let mut output = String::new();
        show_expr(&mut output, &input, &mut FormattingState::default()).unwrap();

        let back = ast::Expr::parse(&output).unwrap();
        assert_ceq!(back, input, "intermediate source '{}'", output);
    }

    #[test]
    fn block_decl_formatting() {
        let src = r#"uniform Global { float param; };"#;

        let mut s = String::new();
        show_external_declaration(
            &mut s,
            &ast::TranslationUnit::parse(src).unwrap().0[0],
            &mut FormattingState::default(),
        )
        .unwrap();

        let expected = expect![[r#"
            uniform Global {
                float param;
            };"#]];

        expected.assert_eq(&s);
    }
}
