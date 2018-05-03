/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

//! Fairly minimal recursive-descent parser helper functions and types.

/// Every bit set but the high bit in usize.
const OFF_MASK: usize = <usize>::max_value() / 2;

/// Only the high bit in usize.
const FATAL_MASK: usize = ! OFF_MASK;

/// An error produced by pipdl
#[derive(Debug)]
pub(crate) struct Error {
    pub(crate) expected: &'static str,
    off_fatal: usize,
}

impl Error {
    pub(crate) fn offset(&self) -> usize {
        self.off_fatal & OFF_MASK
    }

    pub(crate) fn is_fatal(&self) -> bool {
        (self.off_fatal & FATAL_MASK) != 0
    }

    pub(crate) fn make_fatal(mut self) -> Self {
        self.off_fatal |= FATAL_MASK;
        self
    }
}

/// Attempts to run each expression in order, recovering from any non-fatal
/// errors and attempting the next option.
macro_rules! any {
    ($i:ident, $expected:expr, $($e:expr => |$x:ident| $f:expr),+ $(,)*) => {
        // NOTE: This loop is exclusively used to allow using the break
        // statement to abort the block early.
        loop {
            $(match $e {
                Ok((i, $x)) => break Ok((i, $f)),
                Err(e) => {
                    // This assignment is used to help out with type inference.
                    let e: $crate::util::Error = e;
                    if e.is_fatal() {
                        break Err(e);
                    }
                }
            })+
            break $i.expected($expected);
        }
    };

    ($i:ident, $expected:expr, $($e:expr => $f:expr),+ $(,)*) => {
        any!($i, $expected, $($e => |_x| $f),+);
    }
}

/// Attempts to repeatedly run the expression, stopping on a non-fatal error,
/// and directly returning any fatal error.
macro_rules! drive {
    ($i:ident, $e:expr) => {
        let mut $i = $i;
        loop {
            match $e {
                Ok((j, _)) => $i = j,
                Err(e) => if e.is_fatal() {
                    return Err(e);
                } else {
                    break;
                }
            }
        }
    }
}

/// The type of error used by internal parsers
pub(crate) type PResult<'a, T> = Result<(In<'a>, T), Error>;

/// Specify that after this point, errors produced while parsing which are not
/// handled should instead be treated as fatal parsing errors.
macro_rules! commit {
    ($($e:tt)*) => {
        // Evaluate the inner expression, transforming errors into fatal errors.
        (|| { $($e)* })().map_err($crate::util::Error::make_fatal)
    }
}

/// This datastructure is used as the cursor type into the input source data. It
/// holds the full source string, and the current offset.
#[derive(Copy, Clone)]
pub(crate) struct In<'a> {
    src: &'a str,
    offset: usize,
}
impl<'a> In<'a> {
    pub(crate) fn new(s: &'a str) -> Self  {
        In { src: s, offset: 0 }
    }

    /// The remaining string in the source file.
    pub(crate) fn rest(&self) -> &'a str {
        &self.src[self.offset..]
    }

    /// Move the cursor forward by `n` bytes.
    pub(crate) fn advance(&self, n: usize) -> Self {
        let offset = self.offset.checked_add(n).unwrap();
        assert!(offset <= self.src.len());
        In { src: self.src, offset }
    }

    /// Produce a new non-fatal error result with the given expected value.
    pub(crate) fn expected<T>(&self, expected: &'static str) -> Result<T, Error> {
        assert!((self.offset & FATAL_MASK) == 0, "Offset is too large!");
        Err(Error {
            expected: expected,
            off_fatal: self.offset,
        })
    }
}

/// Repeatedly run f, collecting results into a vec. Returns an error if a fatal
/// error is produced while parsing.
pub(crate) fn many<F, R>(i: In, mut f: F) -> PResult<Vec<R>>
where
    F: FnMut(In) -> PResult<R>,
{
    let mut v = Vec::new();
    drive!(i, match f(i) {
        Ok((i, x)) => {
            v.push(x);
            Ok((i, ()))
        }
        Err(e) => Err(e),
    });
    Ok((i, v))
}

/// Repeatedly run f, followed by parsing the seperator sep. Returns an error if
/// a fatal error is produced while parsing.
pub(crate) fn sep<'a, F, R>(
    i: In<'a>,
    mut f: F,
    sep: &'static str,
) -> PResult<'a, Vec<R>>
where
    F: FnMut(In<'a>) -> PResult<'a, R>,
{
    let mut v = Vec::new();
    drive!(i, match f(i) {
        Ok((i, x)) => {
            v.push(x);
            match punct(i, sep) {
                Ok(o) => Ok(o),
                Err(_) => return Ok((i, v)),
            }
        }
        Err(e) => Err(e),
    });
    Ok((i, v))
}

/// Skip any leading whitespace, including comments
pub(crate) fn skip_ws(mut i: In) -> Result<In, Error> {
    loop {
        if i.rest().is_empty() {
            break;
        }

        let c = i.rest().chars().next().unwrap();
        if c.is_whitespace() {
            i = i.advance(c.len_utf8());
            continue;
        }

        // Line comments
        if i.rest().starts_with("//") {
            let x = i.rest().find('\n').unwrap_or(i.rest().len());
            i = i.advance(x);
            continue;
        }

        // Block comments
        if i.rest().starts_with("/*") {
            if let Some(x) = i.rest().find("*/") {
                i = i.advance(x + 2);
                continue;
            }
            return i.advance(i.rest().len())
                .expected("end of block comment (`*/`)");
        }
        break;
    }

    Ok(i)
}

/// Read an identifier as a string.
pub(crate) fn ident(i: In) -> PResult<String> {
    let i = skip_ws(i)?;
    let end = i.rest()
        .char_indices()
        .skip_while(|&(idx, c)| match c {
            '_' | 'a'...'z' | 'A'...'Z' => true,
            '0'...'9' if idx != 0 => true,
            _ => false,
        })
        .next()
        .map(|x| x.0)
        .unwrap_or(i.rest().len());

    if end == 0 {
        return i.expected("identifier");
    }

    Ok((i.advance(end), i.rest()[..end].to_owned()))
}

/// Parse a specific keyword.
pub(crate) fn kw<'a>(i: In<'a>, kw: &'static str) -> PResult<'a, ()> {
    let (j, id) = ident(i)?;
    if id == kw {
        Ok((j, ()))
    } else {
        i.expected(kw)
    }
}

/// Parse punctuation.
pub(crate) fn punct<'a>(i: In<'a>, p: &'static str) -> PResult<'a, ()> {
    let i = skip_ws(i)?;
    if i.rest().starts_with(p) {
        Ok((i.advance(p.len()), ()))
    } else {
        i.expected(p)
    }
}

/// Try to parse the inner value, and return Some() if it succeeded, None if it
/// failed non-fatally, and an error if it failed fatally.
pub(crate) fn maybe<'a, T>(
    i: In<'a>,
    r: PResult<'a, T>
) -> PResult<'a, Option<T>> {
    match r {
        Ok((i, x)) => Ok((i, Some(x))),
        Err(e) => if e.is_fatal() {
            Err(e)
        } else {
            Ok((i, None))
        }
    }
}

/// Parse a string literal.
pub(crate) fn string(i: In) -> PResult<String> {
    let mut s = String::new();
    let (i, _) = punct(i, "\"")?;
    let mut chars = i.rest().char_indices().peekable();
    while let Some((byte_offset, ch)) = chars.next() {
        match ch {
            '"' => return Ok((i.advance(byte_offset + 1), s)),
            '\\' => match chars.next() {
                Some((_, 'n')) => s.push('\n'),
                Some((_, 'r')) => s.push('\r'),
                Some((_, 't')) => s.push('\t'),
                Some((_, '\\')) => s.push('\\'),
                Some((_, '\'')) => s.push('\''),
                Some((_, '"')) => s.push('"'),
                Some((_, '0')) => s.push('\0'),
                _ => return i.advance(byte_offset)
                    .expected("valid escape (\\n, \\r, \\t, \\\\, \\', \\\", or \\0)"),
            }
            x => s.push(x),
        }
    }
    i.expected("end of string literal (\")")
}
