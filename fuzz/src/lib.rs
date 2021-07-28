use argh::FromArgs;

use glsl_lang_pp::processor::ProcessorState;

#[derive(FromArgs)]
/// glsl-lang fuzzer
struct Opts {
    #[argh(switch)]
    /// stop at the parsing stage
    parse_only: bool,
}

pub fn main(data: &[u8]) {
    if let Ok(s) = std::str::from_utf8(data) {
        let opts: Opts = argh::from_env();

        if opts.parse_only {
            let _ = glsl_lang_pp::processor::str::parse(&s);
        } else {
            let _: Vec<_> =
                glsl_lang_pp::processor::str::process(&s, ProcessorState::default()).collect();
        }
    }
}
