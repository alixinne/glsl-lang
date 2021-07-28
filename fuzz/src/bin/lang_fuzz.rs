#[macro_use]
extern crate afl;

fn main() {
    fuzz!(|data: &[u8]| {
        glsl_lang_fuzz::main_lang(data);
    });
}
