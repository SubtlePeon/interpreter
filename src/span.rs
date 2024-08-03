/// Contains all the information about where a token is. Probably too much
/// information.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Span {
    /// The low (start) byte index.
    pub lo: usize,
    /// The high (ending) byte index.
    pub hi: usize,
    /// The line number (usually of the end of the span)
    pub line: usize,
}

impl Span {
    /// Creates a new `Span`.
    pub fn new(lo: usize, hi: usize, line: usize) -> Self {
        Self { lo, hi, line }
    }

    /// Creates a new `Span`, checking with `src` that the span is valid.
    pub fn new_checked(lo: usize, hi: usize, line: usize, src: &str) -> Option<Self> {
        src.get(lo..hi)?;
        Some(Self { lo, hi, line })
    }

    /// Gets the underlying source text given the original source string the span was
    /// taken from.
    pub fn source<'a>(&self, src: &'a str) -> Option<&'a str> {
        src.get(self.lo .. self.hi)
    }

    /// Same as `source`, but panics on error.
    pub fn source_unchecked<'a>(&self, src: &'a str) -> &'a str {
        &src[self.lo .. self.hi]
    }
}
