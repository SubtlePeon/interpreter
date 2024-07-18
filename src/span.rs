/// Contains all the information about where a token is. Probably too much
/// information.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Span<'a> {
    /// The line number.
    pub line: usize,
    /// The source code excerpt. This field might be unnecessary, but excluding it will
    /// require passing in the source code as an argument whenever we want to print the
    /// source code covered by this span.
    pub src: &'a str,
    /// The low (start) byte index.
    pub lo: usize,
    /// The high (ending) byte index.
    pub hi: usize,
}

impl<'a> Span<'a> {
    /// Creates a new `Span`, providing the whole text to be sliced.
    pub fn new(line: usize, text: &'a str, lo: usize, hi: usize) -> Option<Self> {
        let src = text.get(lo..hi)?;
        Some(Self { line, src, lo, hi })
    }

    /// Creates a new `Span` using the pre-sliced `src`.
    pub fn with_src(line: usize, src: &'a str, lo: usize, hi: usize) -> Self {
        Self { line, src, lo, hi }
    }
}
