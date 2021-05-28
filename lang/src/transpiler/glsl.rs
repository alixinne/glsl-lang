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

impl HasPrecedence for ast::ExprData {
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

impl HasPrecedence for ast::UnaryOpData {
    fn precedence(&self) -> u32 {
        3
    }
}

impl HasPrecedence for ast::BinaryOpData {
    fn precedence(&self) -> u32 {
        match self {
            Self::Mult | Self::Div | Self::Mod => 4,
            Self::Add | Self::Sub => 5,
            Self::LShift | Self::RShift => 6,
            Self::Lt | Self::Gt | Self::Lte | Self::Gte => 7,
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
    match **t {
        ast::TypeSpecifierNonArrayData::Void => f.write_str("void"),
        ast::TypeSpecifierNonArrayData::Bool => f.write_str("bool"),
        ast::TypeSpecifierNonArrayData::Int => f.write_str("int"),
        ast::TypeSpecifierNonArrayData::UInt => f.write_str("uint"),
        ast::TypeSpecifierNonArrayData::Float => f.write_str("float"),
        ast::TypeSpecifierNonArrayData::Double => f.write_str("double"),
        ast::TypeSpecifierNonArrayData::Vec2 => f.write_str("vec2"),
        ast::TypeSpecifierNonArrayData::Vec3 => f.write_str("vec3"),
        ast::TypeSpecifierNonArrayData::Vec4 => f.write_str("vec4"),
        ast::TypeSpecifierNonArrayData::DVec2 => f.write_str("dvec2"),
        ast::TypeSpecifierNonArrayData::DVec3 => f.write_str("dvec3"),
        ast::TypeSpecifierNonArrayData::DVec4 => f.write_str("dvec4"),
        ast::TypeSpecifierNonArrayData::BVec2 => f.write_str("bvec2"),
        ast::TypeSpecifierNonArrayData::BVec3 => f.write_str("bvec3"),
        ast::TypeSpecifierNonArrayData::BVec4 => f.write_str("bvec4"),
        ast::TypeSpecifierNonArrayData::IVec2 => f.write_str("ivec2"),
        ast::TypeSpecifierNonArrayData::IVec3 => f.write_str("ivec3"),
        ast::TypeSpecifierNonArrayData::IVec4 => f.write_str("ivec4"),
        ast::TypeSpecifierNonArrayData::UVec2 => f.write_str("uvec2"),
        ast::TypeSpecifierNonArrayData::UVec3 => f.write_str("uvec3"),
        ast::TypeSpecifierNonArrayData::UVec4 => f.write_str("uvec4"),
        ast::TypeSpecifierNonArrayData::Mat2 => f.write_str("mat2"),
        ast::TypeSpecifierNonArrayData::Mat3 => f.write_str("mat3"),
        ast::TypeSpecifierNonArrayData::Mat4 => f.write_str("mat4"),
        ast::TypeSpecifierNonArrayData::Mat22 => f.write_str("mat2x2"),
        ast::TypeSpecifierNonArrayData::Mat23 => f.write_str("mat2x3"),
        ast::TypeSpecifierNonArrayData::Mat24 => f.write_str("mat2x4"),
        ast::TypeSpecifierNonArrayData::Mat32 => f.write_str("mat3x2"),
        ast::TypeSpecifierNonArrayData::Mat33 => f.write_str("mat3x3"),
        ast::TypeSpecifierNonArrayData::Mat34 => f.write_str("mat3x4"),
        ast::TypeSpecifierNonArrayData::Mat42 => f.write_str("mat4x2"),
        ast::TypeSpecifierNonArrayData::Mat43 => f.write_str("mat4x3"),
        ast::TypeSpecifierNonArrayData::Mat44 => f.write_str("mat4x4"),
        ast::TypeSpecifierNonArrayData::DMat2 => f.write_str("dmat2"),
        ast::TypeSpecifierNonArrayData::DMat3 => f.write_str("dmat3"),
        ast::TypeSpecifierNonArrayData::DMat4 => f.write_str("dmat4"),
        ast::TypeSpecifierNonArrayData::DMat22 => f.write_str("dmat2x2"),
        ast::TypeSpecifierNonArrayData::DMat23 => f.write_str("dmat2x3"),
        ast::TypeSpecifierNonArrayData::DMat24 => f.write_str("dmat2x4"),
        ast::TypeSpecifierNonArrayData::DMat32 => f.write_str("dmat3x2"),
        ast::TypeSpecifierNonArrayData::DMat33 => f.write_str("dmat3x3"),
        ast::TypeSpecifierNonArrayData::DMat34 => f.write_str("dmat3x4"),
        ast::TypeSpecifierNonArrayData::DMat42 => f.write_str("dmat4x2"),
        ast::TypeSpecifierNonArrayData::DMat43 => f.write_str("dmat4x3"),
        ast::TypeSpecifierNonArrayData::DMat44 => f.write_str("dmat4x4"),
        ast::TypeSpecifierNonArrayData::Sampler1D => f.write_str("sampler1D"),
        ast::TypeSpecifierNonArrayData::Image1D => f.write_str("image1D"),
        ast::TypeSpecifierNonArrayData::Sampler2D => f.write_str("sampler2D"),
        ast::TypeSpecifierNonArrayData::Image2D => f.write_str("image2D"),
        ast::TypeSpecifierNonArrayData::Sampler3D => f.write_str("sampler3D"),
        ast::TypeSpecifierNonArrayData::Image3D => f.write_str("image3D"),
        ast::TypeSpecifierNonArrayData::SamplerCube => f.write_str("samplerCube"),
        ast::TypeSpecifierNonArrayData::ImageCube => f.write_str("imageCube"),
        ast::TypeSpecifierNonArrayData::Sampler2DRect => f.write_str("sampler2DRect"),
        ast::TypeSpecifierNonArrayData::Image2DRect => f.write_str("image2DRect"),
        ast::TypeSpecifierNonArrayData::Sampler1DArray => f.write_str("sampler1DArray"),
        ast::TypeSpecifierNonArrayData::Image1DArray => f.write_str("image1DArray"),
        ast::TypeSpecifierNonArrayData::Sampler2DArray => f.write_str("sampler2DArray"),
        ast::TypeSpecifierNonArrayData::Image2DArray => f.write_str("image2DArray"),
        ast::TypeSpecifierNonArrayData::SamplerBuffer => f.write_str("samplerBuffer"),
        ast::TypeSpecifierNonArrayData::ImageBuffer => f.write_str("imageBuffer"),
        ast::TypeSpecifierNonArrayData::Sampler2DMs => f.write_str("sampler2DMS"),
        ast::TypeSpecifierNonArrayData::Image2DMs => f.write_str("image2DMS"),
        ast::TypeSpecifierNonArrayData::Sampler2DMsArray => f.write_str("sampler2DMSArray"),
        ast::TypeSpecifierNonArrayData::Image2DMsArray => f.write_str("image2DMSArray"),
        ast::TypeSpecifierNonArrayData::SamplerCubeArray => f.write_str("samplerCubeArray"),
        ast::TypeSpecifierNonArrayData::ImageCubeArray => f.write_str("imageCubeArray"),
        ast::TypeSpecifierNonArrayData::Sampler1DShadow => f.write_str("sampler1DShadow"),
        ast::TypeSpecifierNonArrayData::Sampler2DShadow => f.write_str("sampler2DShadow"),
        ast::TypeSpecifierNonArrayData::Sampler2DRectShadow => f.write_str("sampler2DRectShadow"),
        ast::TypeSpecifierNonArrayData::Sampler1DArrayShadow => f.write_str("sampler1DArrayShadow"),
        ast::TypeSpecifierNonArrayData::Sampler2DArrayShadow => f.write_str("sampler2DArrayShadow"),
        ast::TypeSpecifierNonArrayData::SamplerCubeShadow => f.write_str("samplerCubeShadow"),
        ast::TypeSpecifierNonArrayData::SamplerCubeArrayShadow => {
            f.write_str("samplerCubeArrayShadow")
        }
        ast::TypeSpecifierNonArrayData::ISampler1D => f.write_str("isampler1D"),
        ast::TypeSpecifierNonArrayData::IImage1D => f.write_str("iimage1D"),
        ast::TypeSpecifierNonArrayData::ISampler2D => f.write_str("isampler2D"),
        ast::TypeSpecifierNonArrayData::IImage2D => f.write_str("iimage2D"),
        ast::TypeSpecifierNonArrayData::ISampler3D => f.write_str("isampler3D"),
        ast::TypeSpecifierNonArrayData::IImage3D => f.write_str("iimage3D"),
        ast::TypeSpecifierNonArrayData::ISamplerCube => f.write_str("isamplerCube"),
        ast::TypeSpecifierNonArrayData::IImageCube => f.write_str("iimageCube"),
        ast::TypeSpecifierNonArrayData::ISampler2DRect => f.write_str("isampler2DRect"),
        ast::TypeSpecifierNonArrayData::IImage2DRect => f.write_str("iimage2DRect"),
        ast::TypeSpecifierNonArrayData::ISampler1DArray => f.write_str("isampler1DArray"),
        ast::TypeSpecifierNonArrayData::IImage1DArray => f.write_str("iimage1DArray"),
        ast::TypeSpecifierNonArrayData::ISampler2DArray => f.write_str("isampler2DArray"),
        ast::TypeSpecifierNonArrayData::IImage2DArray => f.write_str("iimage2DArray"),
        ast::TypeSpecifierNonArrayData::ISamplerBuffer => f.write_str("isamplerBuffer"),
        ast::TypeSpecifierNonArrayData::IImageBuffer => f.write_str("iimageBuffer"),
        ast::TypeSpecifierNonArrayData::ISampler2DMs => f.write_str("isampler2MS"),
        ast::TypeSpecifierNonArrayData::IImage2DMs => f.write_str("iimage2DMS"),
        ast::TypeSpecifierNonArrayData::ISampler2DMsArray => f.write_str("isampler2DMSArray"),
        ast::TypeSpecifierNonArrayData::IImage2DMsArray => f.write_str("iimage2DMSArray"),
        ast::TypeSpecifierNonArrayData::ISamplerCubeArray => f.write_str("isamplerCubeArray"),
        ast::TypeSpecifierNonArrayData::IImageCubeArray => f.write_str("iimageCubeArray"),
        ast::TypeSpecifierNonArrayData::AtomicUInt => f.write_str("atomic_uint"),
        ast::TypeSpecifierNonArrayData::USampler1D => f.write_str("usampler1D"),
        ast::TypeSpecifierNonArrayData::UImage1D => f.write_str("uimage1D"),
        ast::TypeSpecifierNonArrayData::USampler2D => f.write_str("usampler2D"),
        ast::TypeSpecifierNonArrayData::UImage2D => f.write_str("uimage2D"),
        ast::TypeSpecifierNonArrayData::USampler3D => f.write_str("usampler3D"),
        ast::TypeSpecifierNonArrayData::UImage3D => f.write_str("uimage3D"),
        ast::TypeSpecifierNonArrayData::USamplerCube => f.write_str("usamplerCube"),
        ast::TypeSpecifierNonArrayData::UImageCube => f.write_str("uimageCube"),
        ast::TypeSpecifierNonArrayData::USampler2DRect => f.write_str("usampler2DRect"),
        ast::TypeSpecifierNonArrayData::UImage2DRect => f.write_str("uimage2DRect"),
        ast::TypeSpecifierNonArrayData::USampler1DArray => f.write_str("usampler1DArray"),
        ast::TypeSpecifierNonArrayData::UImage1DArray => f.write_str("uimage1DArray"),
        ast::TypeSpecifierNonArrayData::USampler2DArray => f.write_str("usampler2DArray"),
        ast::TypeSpecifierNonArrayData::UImage2DArray => f.write_str("uimage2DArray"),
        ast::TypeSpecifierNonArrayData::USamplerBuffer => f.write_str("usamplerBuffer"),
        ast::TypeSpecifierNonArrayData::UImageBuffer => f.write_str("uimageBuffer"),
        ast::TypeSpecifierNonArrayData::USampler2DMs => f.write_str("usampler2DMS"),
        ast::TypeSpecifierNonArrayData::UImage2DMs => f.write_str("uimage2DMS"),
        ast::TypeSpecifierNonArrayData::USampler2DMsArray => f.write_str("usamplerDMSArray"),
        ast::TypeSpecifierNonArrayData::UImage2DMsArray => f.write_str("uimage2DMSArray"),
        ast::TypeSpecifierNonArrayData::USamplerCubeArray => f.write_str("usamplerCubeArray"),
        ast::TypeSpecifierNonArrayData::UImageCubeArray => f.write_str("uimageCubeArray"),
        ast::TypeSpecifierNonArrayData::Struct(ref st) => show_struct_non_declaration(f, st, state),
        ast::TypeSpecifierNonArrayData::TypeName(ref tn) => show_type_name(f, tn, state),
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
        match **dimension {
            ast::ArraySpecifierDimensionData::Unsized => f.write_str("[]")?,
            ast::ArraySpecifierDimensionData::ExplicitlySized(ref e) => {
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
    match **q {
        ast::TypeQualifierSpecData::Storage(ref st) => show_storage_qualifier(f, &st, state),
        ast::TypeQualifierSpecData::Layout(ref l) => show_layout_qualifier(f, &l, state),
        ast::TypeQualifierSpecData::Precision(ref p) => show_precision_qualifier(f, &p, state),
        ast::TypeQualifierSpecData::Interpolation(ref i) => {
            show_interpolation_qualifier(f, &i, state)
        }
        ast::TypeQualifierSpecData::Invariant => f.write_str("invariant"),
        ast::TypeQualifierSpecData::Precise => f.write_str("precise"),
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
    match **q {
        ast::StorageQualifierData::Const => f.write_str("const"),
        ast::StorageQualifierData::InOut => f.write_str("inout"),
        ast::StorageQualifierData::In => f.write_str("in"),
        ast::StorageQualifierData::Out => f.write_str("out"),
        ast::StorageQualifierData::Centroid => f.write_str("centroid"),
        ast::StorageQualifierData::Patch => f.write_str("patch"),
        ast::StorageQualifierData::Sample => f.write_str("sample"),
        ast::StorageQualifierData::Uniform => f.write_str("uniform"),
        ast::StorageQualifierData::Buffer => f.write_str("buffer"),
        ast::StorageQualifierData::Shared => f.write_str("shared"),
        ast::StorageQualifierData::Coherent => f.write_str("coherent"),
        ast::StorageQualifierData::Volatile => f.write_str("volatile"),
        ast::StorageQualifierData::Restrict => f.write_str("restrict"),
        ast::StorageQualifierData::ReadOnly => f.write_str("readonly"),
        ast::StorageQualifierData::WriteOnly => f.write_str("writeonly"),
        ast::StorageQualifierData::Subroutine(ref n) => show_subroutine(f, &n, state),
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
    match **l {
        ast::LayoutQualifierSpecData::Identifier(ref i, Some(ref e)) => {
            write!(f, "{} = ", i)?;
            show_expr(f, &e, state)
        }
        ast::LayoutQualifierSpecData::Identifier(ref i, None) => show_identifier(f, &i, state),
        ast::LayoutQualifierSpecData::Shared => f.write_str("shared"),
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
    match **p {
        ast::PrecisionQualifierData::High => f.write_str("highp"),
        ast::PrecisionQualifierData::Medium => f.write_str("mediump"),
        ast::PrecisionQualifierData::Low => f.write_str("low"),
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
    match **i {
        ast::InterpolationQualifierData::Smooth => f.write_str("smooth"),
        ast::InterpolationQualifierData::Flat => f.write_str("flat"),
        ast::InterpolationQualifierData::NoPerspective => f.write_str("noperspective"),
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
    match **expr {
        ast::ExprData::Variable(ref i) => show_identifier(f, &i, state),
        ast::ExprData::IntConst(ref x) => write!(f, "{}", x),
        ast::ExprData::UIntConst(ref x) => write!(f, "{}u", x),
        ast::ExprData::BoolConst(ref x) => write!(f, "{}", x),
        ast::ExprData::FloatConst(ref x) => show_float(f, *x, state),
        ast::ExprData::DoubleConst(ref x) => show_double(f, *x, state),
        ast::ExprData::Unary(ref op, ref e) => {
            // Note: all unary ops are right-to-left associative
            show_unary_op(f, &op, state)?;

            if e.precedence() > op.precedence() {
                f.write_str("(")?;
                show_expr(f, &e, state)?;
                f.write_str(")")
            } else if let ast::ExprData::Unary(eop, _) = &***e {
                // Prevent double-unary plus/minus turning into inc/dec
                if eop == op && (**eop == ast::UnaryOpData::Add || **eop == ast::UnaryOpData::Minus)
                {
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
        ast::ExprData::Binary(ref op, ref l, ref r) => {
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
        ast::ExprData::Ternary(ref c, ref st, ref e) => {
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
        ast::ExprData::Assignment(ref v, ref op, ref e) => {
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
        ast::ExprData::Bracket(ref e, ref a) => {
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
        ast::ExprData::FunCall(ref fun, ref args) => {
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
        ast::ExprData::Dot(ref e, ref i) => {
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
        ast::ExprData::PostInc(ref e) => {
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
        ast::ExprData::PostDec(ref e) => {
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
        ast::ExprData::Comma(ref a, ref b) => {
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
    match **path {
        ast::PathData::Absolute(ref s) => write!(f, "<{}>", s),
        ast::PathData::Relative(ref s) => write!(f, "\"{}\"", s),
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
    match **op {
        ast::UnaryOpData::Inc => f.write_str("++"),
        ast::UnaryOpData::Dec => f.write_str("--"),
        ast::UnaryOpData::Add => f.write_str("+"),
        ast::UnaryOpData::Minus => f.write_str("-"),
        ast::UnaryOpData::Not => f.write_str("!"),
        ast::UnaryOpData::Complement => f.write_str("~"),
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
    match **op {
        ast::BinaryOpData::Or => state.write_binary_op(f, "||"),
        ast::BinaryOpData::Xor => state.write_binary_op(f, "^^"),
        ast::BinaryOpData::And => state.write_binary_op(f, "&&"),
        ast::BinaryOpData::BitOr => state.write_binary_op(f, "|"),
        ast::BinaryOpData::BitXor => state.write_binary_op(f, "^"),
        ast::BinaryOpData::BitAnd => state.write_binary_op(f, "&"),
        ast::BinaryOpData::Equal => state.write_binary_op(f, "=="),
        ast::BinaryOpData::NonEqual => state.write_binary_op(f, "!="),
        ast::BinaryOpData::Lt => state.write_binary_op(f, "<"),
        ast::BinaryOpData::Gt => state.write_binary_op(f, ">"),
        ast::BinaryOpData::Lte => state.write_binary_op(f, "<="),
        ast::BinaryOpData::Gte => state.write_binary_op(f, ">="),
        ast::BinaryOpData::LShift => state.write_binary_op(f, "<<"),
        ast::BinaryOpData::RShift => state.write_binary_op(f, ">>"),
        ast::BinaryOpData::Add => state.write_binary_op(f, "+"),
        ast::BinaryOpData::Sub => state.write_binary_op(f, "-"),
        ast::BinaryOpData::Mult => state.write_binary_op(f, "*"),
        ast::BinaryOpData::Div => state.write_binary_op(f, "/"),
        ast::BinaryOpData::Mod => state.write_binary_op(f, "%"),
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
    match **op {
        ast::AssignmentOpData::Equal => state.write_binary_op(f, "="),
        ast::AssignmentOpData::Mult => state.write_binary_op(f, "*="),
        ast::AssignmentOpData::Div => state.write_binary_op(f, "/="),
        ast::AssignmentOpData::Mod => state.write_binary_op(f, "%="),
        ast::AssignmentOpData::Add => state.write_binary_op(f, "+="),
        ast::AssignmentOpData::Sub => state.write_binary_op(f, "-="),
        ast::AssignmentOpData::LShift => state.write_binary_op(f, "<<="),
        ast::AssignmentOpData::RShift => state.write_binary_op(f, ">>="),
        ast::AssignmentOpData::And => state.write_binary_op(f, "&="),
        ast::AssignmentOpData::Xor => state.write_binary_op(f, "^="),
        ast::AssignmentOpData::Or => state.write_binary_op(f, "|="),
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
    match **i {
        ast::FunIdentifierData::TypeSpecifier(ref n) => show_type_specifier(f, &n, state),
        ast::FunIdentifierData::Expr(ref e) => show_expr(f, &*e, state),
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
    match **i {
        ast::InitializerData::Simple(ref e) => show_expr(f, e, state),
        ast::InitializerData::List(ref list) => {
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
    match **sst {
        ast::SelectionRestStatementData::Statement(ref if_st) => show_statement(f, if_st, state),
        ast::SelectionRestStatementData::Else(ref if_st, ref else_st) => {
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
    match **cl {
        ast::CaseLabelData::Case(ref e) => {
            f.write_str("case ")?;
            show_expr(f, e, state)?;
            f.write_str(":\n")
        }
        ast::CaseLabelData::Def => f.write_str("default:\n"),
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
    match **ist {
        ast::IterationStatementData::While(ref cond, ref body) => {
            f.write_str("while (")?;
            show_condition(f, cond, state)?;
            f.write_str(") ")?;
            show_statement(f, body, state)
        }
        ast::IterationStatementData::DoWhile(ref body, ref cond) => {
            f.write_str("do ")?;
            show_statement(f, body, state)?;
            f.write_str(" while (")?;
            show_expr(f, cond, state)?;
            f.write_str(")")?;
            state.write_statement_terminator(f)
        }
        ast::IterationStatementData::For(ref init, ref rest, ref body) => {
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
    match **c {
        ast::ConditionData::Expr(ref e) => show_expr(f, e, state),
        ast::ConditionData::Assignment(ref ty, ref name, ref initializer) => {
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
    match **i {
        ast::ForInitStatementData::Expression(ref expr) => {
            if let Some(ref e) = *expr {
                show_expr(f, e, state)?;
            }

            Ok(())
        }
        ast::ForInitStatementData::Declaration(ref d) => show_declaration(f, d, state),
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
    match **j {
        ast::JumpStatementData::Continue => f.write_str("continue")?,
        ast::JumpStatementData::Break => f.write_str("break")?,
        ast::JumpStatementData::Discard => f.write_str("discard")?,
        ast::JumpStatementData::Return(ref e) => {
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
    match **pd {
        ast::PreprocessorDefineData::ObjectLike {
            ref ident,
            ref value,
        } => writeln!(f, "#define {} {}", ident, value),

        ast::PreprocessorDefineData::FunctionLike {
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
        match **profile {
            ast::PreprocessorVersionProfileData::Core => {
                f.write_str(" core")?;
            }
            ast::PreprocessorVersionProfileData::Compatibility => {
                f.write_str(" compatibility")?;
            }
            ast::PreprocessorVersionProfileData::Es => {
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

    match *pe.name {
        ast::PreprocessorExtensionNameData::All => {
            f.write_str("all")?;
        }
        ast::PreprocessorExtensionNameData::Specific(ref n) => {
            f.write_str(n)?;
        }
    }

    if let Some(ref behavior) = pe.behavior {
        match **behavior {
            ast::PreprocessorExtensionBehaviorData::Require => {
                f.write_str(" : require")?;
            }
            ast::PreprocessorExtensionBehaviorData::Enable => {
                f.write_str(" : enable")?;
            }
            ast::PreprocessorExtensionBehaviorData::Warn => {
                f.write_str(" : warn")?;
            }
            ast::PreprocessorExtensionBehaviorData::Disable => {
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
        use lang_util::node::NodeContent;

        let zero = ast::ExprData::DoubleConst(0.);
        let ray = ast::ExprData::Variable("ray".into_node());
        let raydir = ast::ExprData::Dot(Box::new(ray.into()), "dir".into_node());
        let vec4 = ast::ExprData::FunCall(
            ast::FunIdentifierData::TypeSpecifier(Box::new(
                ast::TypeSpecifierData::from(ast::TypeSpecifierNonArrayData::Vec4).into(),
            ))
            .into(),
            vec![raydir.into(), zero.into()],
        );
        let view = ast::ExprData::variable("view");
        let iview = ast::ExprData::FunCall(
            ast::FunIdentifierData::ident("inverse").into(),
            vec![view.into()],
        );
        let mul = ast::ExprData::Binary(
            ast::BinaryOpData::Mult.into(),
            Box::new(iview.into()),
            Box::new(vec4.into()),
        );
        let xyz = ast::ExprData::Dot(Box::new(mul.into()), "xyz".into_node());
        let input = ast::ExprData::FunCall(
            ast::FunIdentifierData::ident("normalize").into(),
            vec![xyz.into()],
        )
        .into();

        let mut output = String::new();
        show_expr(&mut output, &input, &mut FormattingState::default()).unwrap();

        let back = ast::Expr::parse(&output).unwrap();
        assert_eq!(back, input, "intermediate source '{}'", output);
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
