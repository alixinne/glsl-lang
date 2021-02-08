# [glsl-lang](https://github.com/vtavernier/glsl-lang)

[![Build](https://github.com/vtavernier/glsl-lang/workflows/build/badge.svg?branch=master)](https://github.com/vtavernier/glsl-lang/actions)
[![GitHub release](https://img.shields.io/github/v/release/vtavernier/glsl-lang)](https://github.com/vtavernier/glsl-lang/releases)
[![License](https://img.shields.io/github/license/vtavernier/glsl-lang)](LICENSE)

`glsl-lang` is a crate implementing a LALR parser for the GLSL 4.x language,
with partial support for preprocessor directives. Its AST and features are
modeled after [Dimitri Sabadie's `glsl` crate](https://github.com/phaazon/glsl).

## Repository structure

* [`lang`](lang): AST, parser, visitor, transpiler for GLSL language
* [`lang-impl`](lang-impl): proc-macro crate to implement a syntax tree with span information
* [`lang-quote`](lang-quote): proc-macro crate to parse GLSL at compile-time

## License

This work is licensed under the BSD 3-clause license. Lexer and LALR parser by
Vincent Tavernier <vince.tavernier@gmail.com>. Original AST, test suite and
quoting code by Dimitri Sabadie <dimitri.sabadie@gmail.com>.
