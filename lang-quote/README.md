# glsl-lang-quote

[![Crates.io](https://img.shields.io/crates/v/glsl-lang-quote)](https://crates.io/crates/glsl-lang-quote)
[![docs.rs](https://img.shields.io/docsrs/glsl-lang-quote)](https://docs.rs/glsl-lang-quote/)

`glsl-lang-quote` offers proc-macros to quote GLSL syntax in Rust, using the [glsl_lang] crate.

## Usage

```rust
use glsl_lang_quote::glsl;

// Parse a translation unit at compile time
let ast = glsl! {
    void main() {
        gl_FragColor = vec4(1., 0., 0., 1.);
    }
};

// There is exactly one external declaration
assert_eq!(ast.0.len(), 1);
```

## Parsing expressions

This crate offers a set of features `quote-expr`, `quote-preprocessor`, and `quote-statement`.
Enabling those features enable the respective `parser-expr`, `parser-preprocessor` and
`parser-statement` in [glsl_lang], which creates dedicated parsers for those types of GLSL
fragments of the language grammar.

This is the most efficient option for parsing lots of expressions and statements at
compile-time, however this will slow down the initial compilation of `glsl-lang-quote` since
the generated parser file in [glsl_lang] will be much larger.

This is why by default, this crate enables the `quote-parsable` feature, which uses
[glsl_lang]'s [glsl_lang::parse::Parsable] trait, whose limitations apply. Whichever method you
chose, the following code will work:

```rust
use glsl_lang_quote::glsl_expr;

// Parse an expression
let ast = glsl_expr! {
    a = vec4(1., 0., 0., 1.)
};
```

## Quoting and stateful lexers

Since [glsl_lang]'s parser has a stateful lexer (to handle the fact that GLSL's grammar is not
context-free), declaring a type (a `struct` for example) and using it must happen in the same
macro invocation, otherwise the parser will *forget* about the previously declared types. The
best is to only parse whole translation units ([glsl!] macro), or parse unambiguous fragments
(such as expressions, but not statements).

```rust
use glsl_lang_quote::{glsl, glsl_statement};

// This is ok:
let ast = glsl! {
    struct PointLight {
        vec3 pos;
        vec3 color;
    }

    PointLight p;
};

// This will not compile, PointLight can't be parsed as a type name without extra state
let statement = glsl_expr! {
    PointLight p;
};

## Author

Vincent Tavernier <vince.tavernier@gmail.com>

## License

BSD-3-Clause
