# glsl-lang-fuzz

## Usage

```bash
# Build instrumented targets
cargo afl build --target-dir target

# Fuzzing the preprocessor
./fuzz.sh in/pp -- target/debug/pp-fuzz

# Fuzzing the parser
./fuzz.sh in/lang -- target/debug/lang-fuzz
```
