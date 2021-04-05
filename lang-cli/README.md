# glsl-lang-cli

[![Crates.io](https://img.shields.io/crates/v/glsl-lang-cli)](https://crates.io/crates/glsl-lang-cli)

[`glsl-lang`](https://crates.io/crates/glsl-lang) debugging CLI.

*This is only a prototype for debugging, more options will be added in later updates.*

## Usage

Print GLSL AST to the standard output:
```bash
$ cargo run < source.glsl
TranslationUnit
  ExternalDeclaration@0:0..45 `Declaration`
    Declaration@0:0..45 `Block`
      [...]
```

## Author

Vincent Tavernier <vince.tavernier@gmail.com>

## License

BSD-3-Clause
