use std::io::Read;

fn main() {
    let mut data = Vec::new();
    std::io::stdin().read_to_end(&mut data).unwrap();

    if let Ok(s) = std::str::from_utf8(&data) {
        let _ = glsl_lang_pp::parse(&s);
    }
}
