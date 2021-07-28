use std::io::Read;

fn main() {
    let mut data = Vec::new();
    std::io::stdin().read_to_end(&mut data).unwrap();

    glsl_lang_fuzz::main(&data);
}
