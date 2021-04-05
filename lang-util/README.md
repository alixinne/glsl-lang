# lang-util

[![Crates.io](https://img.shields.io/crates/v/lang-util)](https://crates.io/crates/lang-util)
[![docs.rs](https://img.shields.io/docsrs/lang-util)](https://docs.rs/lang-util/)

`lang-util` is a crate that implements utilities to parse and represent syntax trees using
[`lalrpop`](https://crates.io/crates/lalrpop) and [`logos`](https://crates.io/crates/logos).

This crate is tailored for use in the [`glsl-lang`](https://crates.io/crates/glsl-lang) crate,
but you may use its utilities for implementing your own language parsers:
- [error]: parsing error reporting module, with user-readable location information
- [node]: AST node structure and display
- [position]: utilities for working with positions in strings

## Author

Vincent Tavernier <vince.tavernier@gmail.com>

## License

BSD-3-Clause
