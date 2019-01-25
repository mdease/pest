// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use core::cmp::Ordering;
use core::fmt;
use core::hash::{Hash, Hasher};
use core::ops::Range;
use core::ptr;
use core::str;

use span;

/// A `struct` containing a position that is tied to a `&str` which provides useful methods to
/// manually parse it.
pub struct Position<'i> {
    input: &'i [u8],
    pos: usize,
    little_endian: bool
}

pub unsafe fn new(input: &[u8], pos: usize, little_endian: bool) -> Position {
    Position { input, pos, little_endian }
}

impl<'i> Position<'i> {
    /// Creates starting `Position` from an `&str`.
    ///
    /// # Examples
    ///
    /// ```
    /// # use pest::Position;
    /// let start = Position::from_start("".as_bytes());
    /// assert_eq!(start.pos(), 0);
    /// ```
    #[inline]
    pub fn from_start(input: &'i [u8]) -> Position<'i> {
        // Position 0 is always safe because it's always a valid UTF-8 border.
        unsafe { new(input, 0, true) }
    }

    /// Returns the current byte position as a `usize`.
    ///
    /// # Examples
    ///
    /// ```
    /// # use pest::Position;
    /// let input = "ab".as_bytes();
    /// let mut start = Position::from_start(input);
    ///
    /// assert_eq!(start.pos(), 0);
    /// ```
    #[inline]
    pub fn pos(&self) -> usize {
        self.pos
    }

    /// Creates a `Span` from two `Position`s.
    ///
    /// # Panics
    ///
    /// Panics when the positions come from different inputs.
    ///
    /// # Examples
    ///
    /// ```
    /// # use pest::Position;
    /// let input = "ab".as_bytes();
    /// let start = Position::from_start(input);
    /// let span = start.span(&start.clone());
    ///
    /// assert_eq!(span.start(), 0);
    /// assert_eq!(span.end(), 0);
    /// ```
    #[inline]
    pub fn span(&self, other: &Position<'i>) -> span::Span<'i> {
        if ptr::eq(self.input, other.input) {
            // Position's pos is always a UTF-8 border.
            unsafe { span::new(self.input, self.pos, other.pos, self.little_endian) }
        } else {
            panic!("span created from positions from different inputs")
        }
    }

    /// Returns the line - and column number pair of the current `Position`.
    ///
    /// # Examples
    ///
    /// ```
    /// # use pest;
    /// # #[allow(non_camel_case_types)]
    /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
    /// enum Rule {}
    ///
    /// let input = "\na".as_bytes();
    /// let mut state: Box<pest::ParserState<Rule>> = pest::ParserState::new(input);
    /// let mut result = state.match_string("\na");
    /// assert!(result.is_ok());
    /// assert_eq!(result.unwrap().position().line_col(), (2, 2));
    /// ```
    #[inline]
    pub fn line_col(&self) -> (usize, usize) {
        if self.pos > self.input.len() {
            panic!("position out of bounds");
        }

        let mut pos = self.pos;
        // Position's pos is always a UTF-8 border.
        let slice = unsafe { str::from_utf8_unchecked(&self.input[..pos]) };
        let mut chars = slice.chars().peekable();

        let mut line_col = (1, 1);

        while pos != 0 {
            match chars.next() {
                Some('\r') => {
                    if let Some(&'\n') = chars.peek() {
                        chars.next();

                        if pos == 1 {
                            pos -= 1;
                        } else {
                            pos -= 2;
                        }

                        line_col = (line_col.0 + 1, 1);
                    } else {
                        pos -= 1;
                        line_col = (line_col.0, line_col.1 + 1);
                    }
                }
                Some('\n') => {
                    pos -= 1;
                    line_col = (line_col.0 + 1, 1);
                }
                Some(c) => {
                    pos -= c.len_utf8();
                    line_col = (line_col.0, line_col.1 + 1);
                }
                None => unreachable!()
            }
        }

        line_col
    }

    /// Returns the actual line of the input represented by the current `Position`.
    ///
    /// # Examples
    ///
    /// ```
    /// # use pest;
    /// # #[allow(non_camel_case_types)]
    /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
    /// enum Rule {}
    ///
    /// let input = "\na".as_bytes();
    /// let mut state: Box<pest::ParserState<Rule>> = pest::ParserState::new(input);
    /// let mut result = state.match_string("\na");
    /// assert!(result.is_ok());
    /// assert_eq!(result.unwrap().position().line_of(), "a");
    /// ```
    #[inline]
    pub fn line_of(&self) -> &str {
        if self.pos > self.input.len() {
            panic!("position out of bounds");
        }

        let start = if self.pos == 0 {
            0
        } else {
            // Position's pos is always a UTF-8 border.
            let start = unsafe { str::from_utf8_unchecked(self.input) }
                .char_indices()
                .rev()
                .skip_while(|&(i, _)| i >= self.pos)
                .find(|&(_, c)| c == '\n');
            match start {
                Some((i, _)) => i + 1,
                None => 0
            }
        };

        let end = if self.input.is_empty() {
            0
        } else if self.pos == self.input.len() - 1 {
            let mut end = self.input.len();

            if end > 0 && self.input[end - 1] == b'\n' {
                end -= 1;
            }
            if end > 0 && self.input[end - 1] == b'\r' {
                end -= 1;
            }

            end
        } else {
            // Position's pos is always a UTF-8 border.
            let end = unsafe { str::from_utf8_unchecked(self.input) }
                .char_indices()
                .skip_while(|&(i, _)| i < self.pos)
                .find(|&(_, c)| c == '\n');
            let mut end = match end {
                Some((i, _)) => i,
                None => self.input.len()
            };

            if end > 0 && self.input[end - 1] == b'\r' {
                end -= 1;
            }

            end
        };

        // Safe since start and end can only be valid UTF-8 borders.
        unsafe { str::from_utf8_unchecked(&self.input[start..end]) }
    }

    /// Returns `true` when the `Position` points to the start of the input `&str`.
    #[inline]
    pub(crate) fn at_start(&self) -> bool {
        self.pos == 0
    }

    /// Returns `true` when the `Position` points to the end of the input `&str`.
    #[inline]
    pub(crate) fn at_end(&self) -> bool {
        self.pos == self.input.len()
    }

    /// Skips `n` `char`s from the `Position` and returns `true` if the skip was possible or `false`
    /// otherwise. If the return value is `false`, `pos` will not be updated.
    #[inline]
    pub(crate) fn skip(&mut self, n: usize) -> bool {
        let skipped = {
            let mut len = 0;
            // Position's pos is always a UTF-8 border.
            let mut chars = unsafe { str::from_utf8_unchecked(&self.input[self.pos..]) }.chars();

            for _ in 0..n {
                if let Some(c) = chars.next() {
                    len += c.len_utf8();
                } else {
                    return false;
                }
            }

            len
        };

        self.pos += skipped;
        true
    }

    /// Skips until one of the given `strings` is found. If none of the `strings` can be found,
    /// this function will return `false` and its `pos` will not be updated.
    #[inline]
    pub(crate) fn skip_until(&mut self, strings: &[&str]) -> bool {
        for from in self.pos..self.input.len() {
            for slice in strings.iter().map(|s| s.as_bytes()) {
                let to = from + slice.len();

                if to <= self.input.len() && slice == &self.input[from..to] {
                    self.pos = from;
                    return true;
                }
            }
        }

        false
    }

    /// Matches a i8 (any byte)
    #[inline]
    pub(crate) fn match_i8(&mut self) -> bool {
        self.pos += 1;
        true
    }

    /// Matches a u8 (any bytes)
    #[inline]
    pub(crate) fn match_u8(&mut self) -> bool {
        self.pos += 1;
        true
    }

    /// Matches a i16 (any two bytes). Returns false iff there aren't two bytes left
    #[inline]
    pub(crate) fn match_i16(&mut self) -> bool {
        if self.pos < self.input.len() - 1 {
            self.pos += 2;
            true
        } else {
            false
        }
    }

    /// Matches a u16 (any two bytes). Returns false iff there aren't two bytes left
    #[inline]
    pub(crate) fn match_u16(&mut self) -> bool {
        if self.pos < self.input.len() - 1 {
            self.pos += 2;
            true
        } else {
            false
        }
    }

    /// Matches a i32 (any four bytes). Returns false iff there aren't four bytes left
    #[inline]
    pub(crate) fn match_i32(&mut self) -> bool {
        if self.pos < self.input.len() - 3 {
            self.pos += 4;
            true
        } else {
            false
        }
    }

    /// Matches a u32 (any four bytes). Returns false iff there aren't four bytes left
    #[inline]
    pub(crate) fn match_u32(&mut self) -> bool {
        if self.pos < self.input.len() - 3 {
            self.pos += 4;
            true
        } else {
            false
        }
    }

    /// Matches a i64 (any eight bytes). Returns false iff there aren't eight bytes left
    #[inline]
    pub(crate) fn match_i64(&mut self) -> bool {
        if self.pos < self.input.len() - 7 {
            self.pos += 8;
            true
        } else {
            false
        }
    }

    /// Matches a u64 (any eight bytes). Returns false iff there aren't eight bytes left
    #[inline]
    pub(crate) fn match_u64(&mut self) -> bool {
        if self.pos < self.input.len() - 7 {
            self.pos += 8;
            true
        } else {
            false
        }
    }

    /// Matches a isize (any four bytes). Returns false iff there aren't eight bytes left
    /// TODO
    #[inline]
    pub(crate) fn match_isize(&mut self) -> bool {
        if self.pos < self.input.len() - 3 {
            self.pos += 4;
            true
        } else {
            false
        }
    }

    /// Matches a usize (any four bytes). Returns false iff there aren't eight bytes left
    /// TODO
    #[inline]
    pub(crate) fn match_usize(&mut self) -> bool {
        if self.pos < self.input.len() - 3 {
            self.pos += 4;
            true
        } else {
            false
        }
    }

    /// Matches a f32 (any four bytes). Returns false iff there aren't four bytes left
    #[inline]
    pub(crate) fn match_f32(&mut self) -> bool {
        if self.pos < self.input.len() - 3 {
            self.pos += 4;
            true
        } else {
            false
        }
    }

    /// Matches a f64 (any eight bytes). Returns false iff there aren't eight bytes left
    #[inline]
    pub(crate) fn match_f64(&mut self) -> bool {
        if self.pos < self.input.len() - 7 {
            self.pos += 8;
            true
        } else {
            false
        }
    }

    /// Matches a bool (0x0 or 0x1)
    #[inline]
    pub(crate) fn match_bool(&mut self) -> bool {
        let val = self.input[self.pos];

        if val == 0 || val == 1 {
            self.pos += 1;
            true
        } else {
            false
        }
    }

    /// Matches a char
    #[inline]
    pub(crate) fn match_char(&mut self) -> bool {
        let len = {
            // Cannot actually cause undefined behavior.
            let slice = unsafe { str::from_utf8_unchecked(&self.input[self.pos..]) };

            if let Some(c) = slice.chars().next() {
                Some(c.len_utf8())
            } else {
                None
            }
        };

        match len {
            Some(len) => {
                self.pos += len;
                true
            }
            None => false
        }
    }

    /// Sets the byte order to be little endian
    #[inline]
    pub(crate) fn set_le(&mut self) -> bool {
        self.little_endian = true;
        true
    }

    /// Sets the byte order to be big endian
    #[inline]
    pub(crate) fn set_be(&mut self) -> bool {
        self.little_endian = false;
        true
    }

    /// Matches `char` `range` from the `Position` and returns `true` if a match was made or `false`
    /// otherwise. If no match was made, `pos` will not be updated.
    #[inline]
    pub(crate) fn match_range(&mut self, range: Range<char>) -> bool {
        let len = {
            // Cannot actually cause undefined behavior.
            let slice = unsafe { str::from_utf8_unchecked(&self.input[self.pos..]) };

            if let Some(c) = slice.chars().next() {
                if range.start <= c && c <= range.end {
                    Some(c.len_utf8())
                } else {
                    None
                }
            } else {
                None
            }
        };

        match len {
            Some(len) => {
                self.pos += len;
                true
            }
            None => false
        }
    }
}

impl<'i> fmt::Debug for Position<'i> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Position").field("pos", &self.pos).finish()
    }
}

impl<'i> Clone for Position<'i> {
    fn clone(&self) -> Position<'i> {
        // Cloning a safe position is safe.
        unsafe { new(self.input, self.pos, self.little_endian) }
    }
}

impl<'i> PartialEq for Position<'i> {
    fn eq(&self, other: &Position<'i>) -> bool {
        ptr::eq(self.input, other.input) && self.pos == other.pos
    }
}

impl<'i> Eq for Position<'i> {}

impl<'i> PartialOrd for Position<'i> {
    fn partial_cmp(&self, other: &Position<'i>) -> Option<Ordering> {
        if ptr::eq(self.input, other.input) {
            self.pos.partial_cmp(&other.pos)
        } else {
            None
        }
    }
}

impl<'i> Ord for Position<'i> {
    fn cmp(&self, other: &Position<'i>) -> Ordering {
        self.partial_cmp(other)
            .expect("cannot compare positions from different strs")
    }
}

impl<'i> Hash for Position<'i> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        (self.input as *const [u8]).hash(state);
        self.pos.hash(state);
    }
}
