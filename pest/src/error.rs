// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

//! A `mod` containing error data structures.

use std::cmp;
use std::error;
use std::fmt;
use std::mem;

use RuleType;
use position::Position;
use span::Span;

/// A `struct` defining errors.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Error<R> {
    /// Variant of the error
    pub variant: ErrorVariant<R>,
    pub location: InputLocation,
    line: String,
    continued_line: Option<String>,
    start: (usize, usize),
    end: Option<(usize, usize)>
}

/// An `enum` describing `Error` variants.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum ErrorVariant<R> {
    /// Generated parsing error with expected and unexpected `Rule`s
    ParsingError {
        /// Positive attempts
        positives: Vec<R>,
        /// Negative attempts
        negatives: Vec<R>
    },
    /// Custom error with a message
    CustomError {
        /// Short explanation
        message: String
    }
}

/// An `enum` describing where the `Error` has occurred.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum InputLocation {
    /// `Error` was created by `Error::new_from_pos`
    Pos(usize),
    /// `Error` was created by `Error::new_from_span`
    Span((usize, usize))
}

impl<R: RuleType> Error<R> {
    pub fn new_from_pos(variant: ErrorVariant<R>, pos: Position) -> Error<R> {
        Error {
            variant,
            location: InputLocation::Pos(pos.pos()),
            line: pos.line_of().to_owned(),
            continued_line: None,
            start: pos.line_col(),
            end: None
        }
    }

    pub fn new_from_span(variant: ErrorVariant<R>, span: Span) -> Error<R> {
        let continued_line = if span.start_pos().line_col().0 != span.end_pos().line_col().0 {
            Some(span.end_pos().line_of().to_owned())
        } else {
            None
        };

        Error {
            variant,
            location: InputLocation::Span((span.start(), span.end())),
            line: span.start_pos().line_of().to_owned(),
            continued_line,
            start: span.start_pos().line_col(),
            end: Some(span.end_pos().line_col())
        }
    }

    /// Renames all `Rule`s from a `ParsingError` variant returning a `CustomErrorPos`. It does
    /// nothing when called on `CustomErrorPos` and `CustomErrorSpan` variants.
    ///
    /// Useful in order to rename verbose rules or have detailed per-`Rule` formatting.
    ///
    /// # Examples
    ///
    /// ```
    /// # use pest::error::{Error, ErrorVariant};
    /// # use pest::Position;
    /// # #[allow(non_camel_case_types)]
    /// # #[allow(dead_code)]
    /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
    /// # enum Rule {
    /// #     open_paren,
    /// #     closed_paren
    /// # }
    /// # let input = "".as_bytes();
    /// # let pos = Position::from_start(input);
    /// Error::new_from_pos(
    ///     ErrorVariant::ParsingError {
    ///         positives: vec![Rule::open_paren],
    ///         negatives: vec![Rule::closed_paren]
    ///     },
    ///     pos
    /// ).renamed_rules(|rule| {
    ///     match *rule {
    ///         Rule::open_paren => "(".to_owned(),
    ///         Rule::closed_paren => "closed paren".to_owned()
    ///     }
    /// });
    /// ```
    pub fn renamed_rules<F>(mut self, f: F) -> Error<R>
    where
        F: FnMut(&R) -> String
    {
        let variant = match self.variant {
            ErrorVariant::ParsingError {
                positives,
                negatives
            } => {
                let message = Error::parsing_error_message(&positives, &negatives, f);
                ErrorVariant::CustomError { message }
            }
            variant => variant
        };

        self.variant = variant;

        self
    }

    fn spacing(&self) -> String {
        let line = if let Some((line, _)) = self.end {
            cmp::max(self.start.0, line)
        } else {
            self.start.0
        };

        let line_str_len = format!("{}", line).len();

        let mut spacing = String::new();
        for _ in 0..line_str_len {
            spacing.push(' ');
        }

        spacing
    }

    fn underline(&self) -> String {
        let mut underline = String::new();

        let mut start = self.start.1;
        let end = if let Some((_, mut end)) = self.end {
            let inverted_cols = start > end;
            if inverted_cols {
                mem::swap(&mut start, &mut end);
                start -= 1;
                end += 1;
            }

            Some(end)
        } else {
            None
        };
        let offset = start - 1;

        for _ in 0..offset {
            underline.push(' ');
        }

        if let Some(end) = end {
            if end - start > 1 {
                underline.push('^');
                for _ in 2..(end - start) {
                    underline.push('-');
                }
                underline.push('^');
            } else {
                underline.push('^');
            }
        } else {
            underline.push_str("^---")
        }

        underline
    }

    fn message(&self) -> String {
        match self.variant {
            ErrorVariant::ParsingError {
                ref positives,
                ref negatives
            } => Error::parsing_error_message(positives, negatives, |r| format!("{:?}", r)),
            ErrorVariant::CustomError { ref message } => {
                message.clone()
            }
        }
    }

    fn parsing_error_message<F>(positives: &[R], negatives: &[R], mut f: F) -> String
    where
        F: FnMut(&R) -> String
    {
        match (negatives.is_empty(), positives.is_empty()) {
            (false, false) => format!(
                "unexpected {}; expected {}",
                Error::enumerate(negatives, &mut f),
                Error::enumerate(positives, &mut f)
            ),
            (false, true) => format!("unexpected {}", Error::enumerate(negatives, &mut f)),
            (true, false) => format!("expected {}", Error::enumerate(positives, &mut f)),
            (true, true) => "unknown parsing error".to_owned()
        }
    }

    fn enumerate<F>(rules: &[R], f: &mut F) -> String
    where
        F: FnMut(&R) -> String
    {
        match rules.len() {
            1 => f(&rules[0]),
            2 => format!("{} or {}", f(&rules[0]), f(&rules[1])),
            l => {
                let separated = rules
                    .iter()
                    .take(l - 1)
                    .map(|r| f(r))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{}, or {}", separated, f(&rules[l - 1]))
            }
        }
    }

    pub(crate) fn format(&self) -> String {
        let spacing = self.spacing();

        if let (Some(end), &Some(ref continued_line)) = (self.end, &self.continued_line) {
            let has_line_gap = end.0 - self.start.0 > 1;
            if has_line_gap {
                format!(
                    "{s    }--> {ls}:{c}\n\
                     {s    } |\n\
                     {ls:w$} | {line}\n\
                     {s    } | ...\n\
                     {le:w$} | {continued_line}\n\
                     {s    } | {underline}\n\
                     {s    } |\n\
                     {s    } = {message}",
                    s = spacing,
                    w = spacing.len(),
                    ls = self.start.0,
                    le = end.0,
                    c = self.start.1,
                    line = self.line,
                    continued_line = continued_line,
                    underline = self.underline(),
                    message = self.message()
                )
            } else {
                format!(
                    "{s    }--> {ls}:{c}\n\
                     {s    } |\n\
                     {ls:w$} | {line}\n\
                     {le:w$} | {continued_line}\n\
                     {s    } | {underline}\n\
                     {s    } |\n\
                     {s    } = {message}",
                    s = spacing,
                    w = spacing.len(),
                    ls = self.start.0,
                    le = end.0,
                    c = self.start.1,
                    line = self.line,
                    continued_line = continued_line,
                    underline = self.underline(),
                    message = self.message()
                )
            }
        } else {
            format!(
                "{s}--> {l}:{c}\n\
                 {s} |\n\
                 {l} | {line}\n\
                 {s} | {underline}\n\
                 {s} |\n\
                 {s} = {message}",
                s = spacing,
                l = self.start.0,
                c = self.start.1,
                line = self.line,
                underline = self.underline(),
                message = self.message()
            )
        }
    }
}

impl<R: RuleType> fmt::Display for Error<R> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.format())
    }
}

impl<'i, R: RuleType> error::Error for Error<R> {
    fn description(&self) -> &str {
        match self.variant {
            ErrorVariant::ParsingError { .. } => "parsing error",
            ErrorVariant::CustomError { ref message } => message
        }
    }
}
