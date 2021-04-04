# [glsl-lang](https://github.com/vtavernier/glsl-lang)

[![Build](https://github.com/vtavernier/glsl-lang/workflows/build/badge.svg?branch=master)](https://github.com/vtavernier/glsl-lang/actions)
[![Crates.io](https://img.shields.io/crates/v/glsl-lang)](https://crates.io/crates/glsl-lang)
[![docs.rs](https://img.shields.io/docsrs/glsl-lang)](https://docs.rs/glsl-lang/)
[![License](https://img.shields.io/github/license/vtavernier/glsl-lang)](LICENSE)

`glsl-lang` is a crate implementing a LALR parser for the GLSL 4.x language,
with partial support for preprocessor directives. Its AST and features are
modeled after [Dimitri Sabadie's `glsl` crate](https://github.com/phaazon/glsl).

## Table of contents

<!-- vim-markdown-toc GFM -->

* [Repository structure](#repository-structure)
* [`glsl-lang` vs. `glsl` crates](#glsl-lang-vs-glsl-crates)
  * [Why pick this crate?](#why-pick-this-crate)
    * [It's fast](#its-fast)
    * [Syntax nodes have location information](#syntax-nodes-have-location-information)
    * [Re-written GLSL transpiler](#re-written-glsl-transpiler)
    * [`glsl-lang-quote` quoting support](#glsl-lang-quote-quoting-support)
  * [Why not pick this crate?](#why-not-pick-this-crate)
    * [Stateful lexer](#stateful-lexer)
    * [Parser generation and compile times](#parser-generation-and-compile-times)
    * [`glsl-lang-quote` state](#glsl-lang-quote-state)
    * [AST differences](#ast-differences)
    * [Documentation](#documentation)
* [Limitations](#limitations)
* [License](#license)

<!-- vim-markdown-toc -->

## Repository structure

| crates.io                                                                                                   | Path                                   | Description                                                       |
| ---                                                                                                         | ---                                    | ---                                                               |
| [![Crates.io](https://img.shields.io/crates/v/glsl-lang)](https://crates.io/crates/glsl-lang)               | [`lang`](lang)                         | AST, parser, visitor, transpiler for GLSL language                |
| [![Crates.io](https://img.shields.io/crates/v/glsl-lang-quote)](https://crates.io/crates/glsl-lang-quote)   | [`lang-quote`](lang-quote)             | proc-macro crate to parse GLSL at compile-time                    |
| [![Crates.io](https://img.shields.io/crates/v/glsl-lang-cli)](https://crates.io/crates/glsl-lang-cli)       | [`lang-cli`](lang-cli)                 | simple CLI tool to show GLSL syntax trees                         |
| [![Crates.io](https://img.shields.io/crates/v/lang-util)](https://crates.io/crates/lang-util)               | [`lang-util`](lang-util)               | utilities for implementing syntax trees                           |
| [![Crates.io](https://img.shields.io/crates/v/lang-util-derive)](https://crates.io/crates/lang-util-derive) | [`lang-util-derive`](lang-util-derive) | proc-macro crate to implement a syntax tree with span information |

## `glsl-lang` vs. `glsl` crates

### Why pick this crate?

#### It's fast

Due to using a LALR parser and dedicated tokenizer, it's 500x faster than
`glsl`:

    $ cargo criterion --bench glsl -- --sample-size 1000
    TranslationUnit: void main() { ((((((((1.0f)))))))); }/lalrpop
                            time:   [7.7858 us 7.7945 us 7.8027 us]
    TranslationUnit: void main() { ((((((((1.0f)))))))); }/glsl
                            time:   [3.1827 ms 3.1831 ms 3.1836 ms]

#### Syntax nodes have location information

Most nodes in the AST are wrapped in a special `Node` type, which holds:

* `source_id`: an `usize` to identify which parsing pass produced this node
* `start`: the starting offset of the node in the corresponding input
* `end`: the ending offset of the node in the corresponding input

#### Re-written GLSL transpiler

The GLSL transpiler has been partially rewritten to generate indented code.
It's still a work-in-progress but generates (mostly) readable code.

#### `glsl-lang-quote` quoting support

`glsl-lang-quote` is the `glsl-lang` version of `glsl-quasiquote`. It parses
GLSL at compile-time to generate an AST. However, you can also insert parts
of runtime-generated AST using a quoting syntax. Currently, the following
insertion locations for the `#(ident)` syntax are supported:

* Identifier
* Expression
* Function name

### Why not pick this crate?

#### Stateful lexer

C-based grammar are ambiguous by definition. The main ambiguity being the
inability of the parser to solve conflicts between type names and identifiers
without extra context. Thus, to enable LALR parsing of GLSL, we need to
maintain a list of identifiers that are declared as type names, so the lexer
can properly return `IDENT` or `TYPE_NAME` as it is reading the file.

Depending on your use case, this might prove unwieldy since the parser is not
context-free. Parsing one translation unit followed by another requires
forwarding the type name/identifier disambiguation table to the second pass.

#### Parser generation and compile times

The GLSL grammar is implemented in
[`lang/src/parser.lalrpop`](lang/src/parser.lalrpop) using
[LALRPOP](https://github.com/lalrpop/lalrpop). The default feature set only
allows parsing translation units (the top-level rule in the GLSL grammar),
which results in a 25k lines parser file. If you want to include more parsers
(for example for expressions, statements, etc.) you will need to enable the
respective features (`parser-expr`, `parser-statement`, etc.) but this will
slow down the compilation of `glsl-lang` by a significant amount.

To alleviate this issue, you can use the `Parsable` trait: by wrapping a syntax
item in a suitable source, and then matching the resulting AST, we can extract
the result of any rule in the grammar. Currently, this interface panics if the
output AST cannot be matched, so don't use it on unknown input. It's fine for
testing though.

#### `glsl-lang-quote` state

Parsing preprocessor directives is currently not supported.

#### AST differences

There are some differences in both crate's ASTs, so porting to `glsl-lang`
would require some changes to your code:
* The `Statement/SimpleStatement/CompoundStatement` structure was flattened to `Statement`
* The `subroutine` storage qualifier takes a `TypeSpecifier` array instead of a `TypeName` array
* `FunIdentifier::Identifier` was replaced with `FunIdentifier::TypeSpecifier`:
  this reflects the fact that a type specifier as a function identifier is a
  constructor, and array specifiers are only allowed in this position.
* Support for the `attribute` and `varying` qualifiers was removed
* The `NonEmpty` wrapper was removed
* `Declaration::Global` was removed since it's parsed as an `InitDeclaratorList`

#### Documentation

Most items are documented (through `#[deny(missing_docs)]`) although we are
currently missing some usage examples. These will come soon enough, promise!

## Limitations

Aside from the limitations mentioned in the paragraph above:

* Only GLSL 3.x/4.x is supported. GLSL 1.x is not
* As for the `glsl` crate, preprocessor parsing is mostly handled at the syntax
  level, so GLSL sources which are syntactically invalid without actual
  preprocessing will fail to parse.
* Currently, no semantic analysis

## License

This work is licensed under the BSD 3-clause license. Lexer and LALR parser by
Vincent Tavernier <vince.tavernier@gmail.com>. Original AST, test suite and
quoting code by Dimitri Sabadie <dimitri.sabadie@gmail.com>.
