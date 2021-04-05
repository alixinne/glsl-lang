# glsl-lang

[![Crates.io](https://img.shields.io/crates/v/glsl-lang)](https://crates.io/crates/glsl-lang)
[![docs.rs](https://img.shields.io/docsrs/glsl-lang)](https://docs.rs/glsl-lang/)

`glsl-lang` is a crate implementing a LALR parser for the GLSL 4.x language,
with partial support for preprocessor directives. Its AST and features are
modeled after [Dimitri Sabadie's `glsl` crate](https://github.com/phaazon/glsl).

See the [homepage](https://github.com/vtavernier/glsl-lang) for more detailed comparison
elements.

## Examples

### Parsing GLSL

```rust
use glsl_lang::{ast, parse::Parse};

// Some GLSL source to parse
let source = r#"void main() {
    gl_FragColor = vec4(1., 0.5, 0.25, 1.);
}"#;

// Try parsing the source
let ast = ast::TranslationUnit::parse(source);
assert!(ast.is_ok());
```

## Crate features

This crate has the following features:
- `parser-expr`: generate parser code for parsing GLSL expressions
- `parser-preprocessor`: generate parser code for parsing GLSL preprocessor directives
- `parser-statement`: generate parser code for parsing GLSL statements

None of these features are enabled by default, as they significantly increase the compile
times. As an alternative, you may use the [`Parsable`](crate::parse::Parsable) trait, which
wraps grammar rules in suitable source and matches the result to extract the part of the AST
we're interested in.

```rust
// parse::Parse is not implemented for ast::Expr with the default features
use glsl_lang::{ast, parse::Parsable};

let source = "a = b ? 1.0 : 0.0";

// Parse with Parsable::parse
let ast = ast::Expr::parse(source);
assert!(ast.is_ok());
```

## Useful links

- [The OpenGL Shading Language Version 4.60](https://www.khronos.org/registry/OpenGL/specs/gl/GLSLangSpec.4.60.pdf)

## Author

Vincent Tavernier <vince.tavernier@gmail.com>

## License

BSD-3-Clause
