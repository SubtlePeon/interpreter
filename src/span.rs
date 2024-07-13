/// Contains all the information about where a token is. Probably too much
/// information.
#[derive(Clone, Debug)]
pub struct Span<'a> {
    /// The line number.
    pub line: usize,
    /// The source code exceprt.
    pub src: &'a str,
    /// The low (start) byte index.
    pub lo: usize,
    /// The high (ending) byte index.
    pub hi: usize,
}

impl<'a> Span<'a> {
    pub fn new(line: usize, text: &'a str, lo: usize, hi: usize) -> Self {
        let src = &text[lo..hi];
        Self { line, src, lo, hi }
    }
}
