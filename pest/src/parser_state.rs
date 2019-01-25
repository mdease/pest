// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use core::fmt;
use core::ops::Range;

use RuleType;
use error::Error;
use iterators::QueueableToken;
use position::{self, Position};

/// An `enum` specifying the current lookahead status of a `ParserState`.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Lookahead {
    Positive,
    Negative,
    None
}

/// An `enum` specifying the current atomicity of a `ParserState`.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Atomicity {
    Atomic,
    CompoundAtomic,
    NonAtomic
}

/// Type alias to simplify specifying the return value of the chained closures.
pub type ParseResult<S> = Result<S, S>;

/// A `struct` which contains the complete state of a `Parser`.
#[derive(Debug)]
pub struct ParserState<'i, R: RuleType> {
    position: Position<'i>,
    queue: Queue<R>,
    lookahead: Lookahead,
    pos_attempts_len: usize,
    neg_attempts_len: usize,
    attempt_pos: usize,
    atomicity: Atomicity,
    little_endian: bool
}

struct Queue<R: RuleType> {
    queue: [QueueableToken<R>; 50],
    queue_len: usize
}

impl<R: RuleType> Queue<R> {
    fn len(&self) -> usize {
        self.queue_len
    }

    fn push(&mut self, item: QueueableToken<R>) {
        self.queue[self.queue_len] = item;
        self.queue_len += 1;
    }

    fn truncate(&mut self, index: usize) {
        for i in index..50 {
            match self.queue[i] {
                QueueableToken::Uninitialized{} => { break; },
                _ => { self.queue[i] = QueueableToken::Uninitialized{}; }
            }
        }

        self.queue_len = index;
    }
}

impl<R: RuleType> fmt::Debug for Queue<R> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Format unavailable")
    }
}

/// Creates a `ParserState` from a `&str`, supplying it to a closure `f`.
///
/// # Examples
///
/// ```
/// # use pest;
/// let input = "".as_bytes();
/// pest::state::<(), _>(input, |s| Ok(s)).unwrap();
/// ```
pub fn state<'i, R: RuleType, F>(input: &'i [u8], f: F) -> Result<usize, Error>
where
    F: FnOnce(ParserState<'i, R>) -> ParseResult<ParserState<'i, R>>
{
    let state = ParserState::new(input);

    match f(state) {
        Ok(state) => {
            let len = state.queue.len();
            Ok(len)
        }
        Err(state) => {
            Err(Error::new_from_pos(
                unsafe { position::new(input, state.attempt_pos, state.little_endian) }
            ))
        }
    }
}

impl<'i, R: RuleType> ParserState<'i, R> {
    /// Allocates a fresh `ParserState` object to the heap and returns the owned `Box`. This `Box`
    /// will be passed from closure to closure based on the needs of the specified `Parser`.
    ///
    /// # Examples
    ///
    /// ```
    /// # use pest;
    /// let input = "".as_bytes();
    /// let state: Box<pest::ParserState<&str>> = pest::ParserState::new(input);
    /// ```
    pub fn new(input: &'i [u8]) -> Self {
        ParserState {
            position: Position::from_start(input),
            queue: Queue { queue: [QueueableToken::Uninitialized{}; 50], queue_len: 0 },
            lookahead: Lookahead::None,
            pos_attempts_len: 0,
            neg_attempts_len: 0,
            attempt_pos: 0,
            atomicity: Atomicity::NonAtomic,
            little_endian: true
        }
    }

    /// Returns a reference to the current `Position` of the `ParserState`.
    ///
    /// # Examples
    ///
    /// ```
    /// # use pest;
    /// # #[allow(non_camel_case_types)]
    /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
    /// enum Rule {
    ///     ab
    /// }
    ///
    /// let input = "ab".as_bytes();
    /// let mut state: Box<pest::ParserState<Rule>> = pest::ParserState::new(input);
    /// let position = state.position();
    /// assert_eq!(position.pos(), 0);
    /// ```
    pub fn position(&self) -> &Position<'i> {
        &self.position
    }

    /// Returns the current atomicity of the `ParserState`.
    ///
    /// # Examples
    ///
    /// ```
    /// # use pest;
    /// # use pest::Atomicity;
    /// # #[allow(non_camel_case_types)]
    /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
    /// enum Rule {
    ///     ab
    /// }
    ///
    /// let input = "ab".as_bytes();
    /// let mut state: Box<pest::ParserState<Rule>> = pest::ParserState::new(input);
    /// let atomicity = state.atomicity();
    /// assert_eq!(atomicity, Atomicity::NonAtomic);
    /// ```
    pub fn atomicity(&self) -> Atomicity {
        self.atomicity
    }

    /// Wrapper needed to generate tokens. This will associate the `R` type rule to the closure
    /// meant to match the rule.
    ///
    /// # Examples
    ///
    /// ```
    /// # use pest;
    /// # #[allow(non_camel_case_types)]
    /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
    /// enum Rule {
    ///     a
    /// }
    ///
    /// let input = "a".as_bytes();
    /// let pairs: Vec<_> = pest::state(input, |state| {
    ///     state.rule(Rule::a, |s| Ok(s))
    /// }).unwrap().collect();
    ///
    /// assert_eq!(pairs.len(), 1);
    /// ```
    #[inline]
    pub fn rule<F>(mut self: Self, rule: R, f: F) -> ParseResult<Self>
    where
        F: FnOnce(Self) -> ParseResult<Self>
    {
        let actual_pos = self.position.pos();
        let index = self.queue.len();

        let (pos_attempts_index, neg_attempts_index) = if actual_pos == self.attempt_pos {
            (self.pos_attempts_len, self.neg_attempts_len)
        } else {
            // Attempts have not been cleared yet since the attempt_pos is older.
            (0, 0)
        };

        if self.lookahead == Lookahead::None && self.atomicity != Atomicity::Atomic {
            // Pair's position will only be known after running the closure.
            self.queue.push(QueueableToken::Start {
                end_token_index: 0,
                input_pos: actual_pos as u8
            });
        }

        let attempts = self.pos_attempts_len + self.neg_attempts_len;

        let result = f(self);

        match result {
            Ok(mut new_state) => {
                if new_state.lookahead == Lookahead::Negative {
                    new_state.track(
                        actual_pos,
                        pos_attempts_index,
                        neg_attempts_index,
                        attempts
                    );
                }

                if new_state.lookahead == Lookahead::None
                    && new_state.atomicity != Atomicity::Atomic
                {
                    // Storing the pair's index in the first token that was added before the closure was
                    // run.
                    let new_index = new_state.queue.len();
                    match new_state.queue.queue[index] {
                        QueueableToken::Start {
                            ref mut end_token_index,
                            ..
                        } => *end_token_index = new_index as u8,
                        _ => unreachable!()
                    };

                    let new_pos = new_state.position.pos();

                    new_state.queue.push(QueueableToken::End {
                        start_token_index: index as u8,
                        rule,
                        input_pos: new_pos as u8
                    });
                }

                Ok(new_state)
            }
            Err(mut new_state) => {
                if new_state.lookahead != Lookahead::Negative {
                    new_state.track(
                        actual_pos,
                        pos_attempts_index,
                        neg_attempts_index,
                        attempts
                    );
                }

                if new_state.lookahead == Lookahead::None
                    && new_state.atomicity != Atomicity::Atomic
                {
                    new_state.queue.truncate(index);
                }

                Err(new_state)
            }
        }
    }

    fn track(
        &mut self,
        pos: usize,
        pos_attempts_index: usize,
        neg_attempts_index: usize,
        prev_attempts: usize
    ) {
        if self.atomicity == Atomicity::Atomic {
            return;
        }

        // If nested rules made no progress, there is no use to report them; it's only useful to
        // track the current rule, the exception being when only one attempt has been made during
        // the children rules.
        let curr_attempts = self.pos_attempts_len + self.neg_attempts_len;
        if curr_attempts > prev_attempts && curr_attempts - prev_attempts == 1 {
            return;
        }

        if pos == self.attempt_pos {
            // truncate
            self.pos_attempts_len = pos_attempts_index;
            self.neg_attempts_len = neg_attempts_index;
        }

        if pos > self.attempt_pos {
            self.pos_attempts_len = 0;
            self.neg_attempts_len = 0;

            self.attempt_pos = pos;
        }

        if self.lookahead != Lookahead::Negative {
            if pos == self.attempt_pos {
                self.pos_attempts_len += 1;
            }
        } else {
            if pos == self.attempt_pos {
                self.neg_attempts_len += 1;
            }
        }
    }

    /// Starts a sequence of transformations provided by `f` from the `Box<ParserState>`. It returns the
    /// same `Result` returned by `f` in the case of an `Ok` or `Err` with the current `Box<ParserState>`
    /// otherwise.
    ///
    /// This method is useful to parse sequences that only match together which usually come in the
    /// form of chained `Result`s with
    /// [`Result::and_then`](https://doc.rust-lang.org/std/result/enum.Result.html#method.and_then).
    ///
    ///
    /// # Examples
    ///
    /// ```
    /// # use pest;
    /// # #[allow(non_camel_case_types)]
    /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
    /// enum Rule {
    ///     a
    /// }
    ///
    /// let input = "a".as_bytes();
    /// let pairs: Vec<_> = pest::state(input, |state| {
    ///     state.sequence(|s| {
    ///         s.rule(Rule::a, |s| Ok(s)).and_then(|s| {
    ///             s.match_string("b")
    ///         })
    ///     }).or_else(|s| {
    ///         Ok(s)
    ///     })
    /// }).unwrap().collect();
    ///
    /// assert_eq!(pairs.len(), 0);
    /// ```
    #[inline]
    pub fn sequence<F>(self: Self, f: F) -> ParseResult<Self>
    where
        F: FnOnce(Self) -> ParseResult<Self>
    {
        let token_index = self.queue.len();
        let initial_pos = self.position.clone();

        let result = f(self);

        match result {
            Ok(new_state) => Ok(new_state),
            Err(mut new_state) => {
                // Restore the initial position and truncate the token queue.
                new_state.position = initial_pos;
                new_state.queue.truncate(token_index);

                Err(new_state)
            }
        }
    }

    /// Repeatedly applies the transformation provided by `f` from the `Box<ParserState>`. It
    /// returns `Ok` with the updated `Box<ParserState>` returned by `f` wrapped up in an `Err`.
    ///
    /// # Examples
    ///
    /// ```
    /// # use pest;
    /// # #[allow(non_camel_case_types)]
    /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
    /// enum Rule {
    ///     ab
    /// }
    ///
    /// let input = "aab".as_bytes();
    /// let mut state: Box<pest::ParserState<Rule>> = pest::ParserState::new(input);
    /// let mut result = state.repeat(|s| {
    ///     s.match_string("a")
    /// });
    /// assert!(result.is_ok());
    /// assert_eq!(result.unwrap().position().pos(), 2);
    ///
    /// state = pest::ParserState::new(input);
    /// result = state.repeat(|s| {
    ///     s.match_string("b")
    /// });
    /// assert!(result.is_ok());
    /// assert_eq!(result.unwrap().position().pos(), 0);
    /// ```
    #[inline]
    pub fn repeat<F>(self: Self, mut f: F) -> ParseResult<Self>
    where
        F: FnMut(Self) -> ParseResult<Self>
    {
        let mut result = f(self);

        loop {
            match result {
                Ok(state) => result = f(state),
                Err(state) => return Ok(state)
            };
        }
    }

    /// Optionally applies the transformation provided by `f` from the `Box<ParserState>`. It returns `Ok`
    /// with the updated `Box<ParserState>` returned by `f` regardless of the `Result`.
    ///
    /// # Examples
    ///
    /// ```
    /// # use pest;
    /// # #[allow(non_camel_case_types)]
    /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
    /// enum Rule {
    ///     ab
    /// }
    ///
    /// let input = "ab".as_bytes();
    /// let mut state: Box<pest::ParserState<Rule>> = pest::ParserState::new(input);
    /// let result = state.optional(|s| {
    ///     s.match_string("ab")
    /// });
    /// assert!(result.is_ok());
    ///
    /// state = pest::ParserState::new(input);
    /// let result = state.optional(|s| {
    ///     s.match_string("ac")
    /// });
    /// assert!(result.is_ok());
    /// ```
    #[inline]
    pub fn optional<F>(self: Self, f: F) -> ParseResult<Self>
    where
        F: FnOnce(Self) -> ParseResult<Self>
    {
        match f(self) {
            Ok(state) | Err(state) => Ok(state)
        }
    }

    /// Matches a i8
    #[inline]
    pub fn match_i8(mut self: Self) -> ParseResult<Self> {
        if self.position.match_i8() {
            Ok(self)
        } else {
            Err(self)
        }
    }

    /// Matches a u8
    #[inline]
    pub fn match_u8(mut self: Self) -> ParseResult<Self> {
        if self.position.match_u8() {
            Ok(self)
        } else {
            Err(self)
        }
    }

    /// Matches a i16
    #[inline]
    pub fn match_i16(mut self: Self) -> ParseResult<Self> {
        if self.position.match_i16() {
            Ok(self)
        } else {
            Err(self)
        }
    }

    /// Matches a u16
    #[inline]
    pub fn match_u16(mut self: Self) -> ParseResult<Self> {
        if self.position.match_u16() {
            Ok(self)
        } else {
            Err(self)
        }
    }

    /// Matches a i32
    #[inline]
    pub fn match_i32(mut self: Self) -> ParseResult<Self> {
        if self.position.match_i32() {
            Ok(self)
        } else {
            Err(self)
        }
    }

    /// Matches a u32
    #[inline]
    pub fn match_u32(mut self: Self) -> ParseResult<Self> {
        if self.position.match_u32() {
            Ok(self)
        } else {
            Err(self)
        }
    }

    /// Matches a i64
    #[inline]
    pub fn match_i64(mut self: Self) -> ParseResult<Self> {
        if self.position.match_i64() {
            Ok(self)
        } else {
            Err(self)
        }
    }

    /// Matches a u64
    #[inline]
    pub fn match_u64(mut self: Self) -> ParseResult<Self> {
        if self.position.match_u64() {
            Ok(self)
        } else {
            Err(self)
        }
    }

    /// Matches a isize
    #[inline]
    pub fn match_isize(mut self: Self) -> ParseResult<Self> {
        if self.position.match_isize() {
            Ok(self)
        } else {
            Err(self)
        }
    }

    /// Matches a usize
    #[inline]
    pub fn match_usize(mut self: Self) -> ParseResult<Self> {
        if self.position.match_usize() {
            Ok(self)
        } else {
            Err(self)
        }
    }

    /// Matches a f32
    #[inline]
    pub fn match_f32(mut self: Self) -> ParseResult<Self> {
        if self.position.match_f32() {
            Ok(self)
        } else {
            Err(self)
        }
    }

    /// Matches a f64
    #[inline]
    pub fn match_f64(mut self: Self) -> ParseResult<Self> {
        if self.position.match_f64() {
            Ok(self)
        } else {
            Err(self)
        }
    }

    /// Matches a bool
    #[inline]
    pub fn match_bool(mut self: Self) -> ParseResult<Self> {
        if self.position.match_bool() {
            Ok(self)
        } else {
            Err(self)
        }
    }

    /// Matches a char
    #[inline]
    pub fn match_char(mut self: Self) -> ParseResult<Self> {
        if self.position.match_char() {
            Ok(self)
        } else {
            Err(self)
        }
    }

    /// Sets the byte order to be little endian
    #[inline]
    pub fn set_le(mut self: Self) -> ParseResult<Self> {
        if self.position.set_le() {
            self.little_endian = true;
            Ok(self)
        } else {
            Err(self)
        }
    }

    /// Sets the byte order to be big endian
    #[inline]
    pub fn set_be(mut self: Self) -> ParseResult<Self> {
        if self.position.set_be() {
            self.little_endian = false;
            Ok(self)
        } else {
            Err(self)
        }
    }

    /// Asks the `ParserState` to match a `char` `range` from the given `string`. If the match is
    /// successful, this will return an `Ok` with the updated `Box<ParserState>`. If failed, an
    /// `Err` with the updated `Box<ParserState>` is returned.
    ///
    /// # Examples
    ///
    /// ```
    /// # use pest;
    /// # #[allow(non_camel_case_types)]
    /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
    /// enum Rule {}
    ///
    /// let input = "ab".as_bytes();
    /// let mut state: Box<pest::ParserState<Rule>> = pest::ParserState::new(input);
    /// let mut result = state.match_range('a'..'z');
    /// assert!(result.is_ok());
    /// assert_eq!(result.unwrap().position().pos(), 1);
    ///
    /// state = pest::ParserState::new(input);
    /// result = state.match_range('A'..'Z');
    /// assert!(result.is_err());
    /// assert_eq!(result.unwrap_err().position().pos(), 0);
    /// ```
    #[inline]
    pub fn match_range(mut self: Self, range: Range<char>) -> ParseResult<Self> {
        if self.position.match_range(range) {
            Ok(self)
        } else {
            Err(self)
        }
    }

    /// Asks the `ParserState` to skip `n` `char`s. If the match is successful, this will return an
    /// `Ok` with the updated `Box<ParserState>`. If failed, an `Err` with the updated
    /// `Box<ParserState>` is returned.
    ///
    /// # Examples
    ///
    /// ```
    /// # use pest;
    /// # #[allow(non_camel_case_types)]
    /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
    /// enum Rule {}
    ///
    /// let input = "ab".as_bytes();
    /// let mut state: Box<pest::ParserState<Rule>> = pest::ParserState::new(input);
    /// let mut result = state.skip(1);
    /// assert!(result.is_ok());
    /// assert_eq!(result.unwrap().position().pos(), 1);
    ///
    /// state = pest::ParserState::new(input);
    /// result = state.skip(3);
    /// assert!(result.is_err());
    /// assert_eq!(result.unwrap_err().position().pos(), 0);
    /// ```
    #[inline]
    pub fn skip(mut self: Self, n: usize) -> ParseResult<Self> {
        if self.position.skip(n) {
            Ok(self)
        } else {
            Err(self)
        }
    }

    /// Asks the `ParserState` to continue to skip until one of the given `strings` is found. If
    /// the match is successful, this will return an `Ok` with the updated `Box<ParserState>`. If
    /// failed, an `Err` with the updated `Box<ParserState>` is returned.
    ///
    /// # Examples
    ///
    /// ```
    /// # use pest;
    /// # #[allow(non_camel_case_types)]
    /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
    /// enum Rule {}
    ///
    /// let input = "abcd".as_bytes();
    /// let mut state: Box<pest::ParserState<Rule>> = pest::ParserState::new(input);
    /// let mut result = state.skip_until(&["c", "d"]);
    /// assert!(result.is_ok());
    /// assert_eq!(result.unwrap().position().pos(), 2);
    /// ```
    #[inline]
    pub fn skip_until(mut self: Self, strings: &[&str]) -> ParseResult<Self> {
        if self.position.skip_until(strings) {
            Ok(self)
        } else {
            Err(self)
        }
    }

    /// Asks the `ParserState` to match the start of the input. If the match is successful, this
    /// will return an `Ok` with the updated `Box<ParserState>`. If failed, an `Err` with the
    /// updated `Box<ParserState>` is returned.
    ///
    /// # Examples
    ///
    /// ```
    /// # use pest;
    /// # #[allow(non_camel_case_types)]
    /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
    /// enum Rule {}
    ///
    /// let input = "ab".as_bytes();
    /// let mut state: Box<pest::ParserState<Rule>> = pest::ParserState::new(input);
    /// let mut result = state.start_of_input();
    /// assert!(result.is_ok());
    ///
    /// state = pest::ParserState::new(input);
    /// state = state.match_string("ab").unwrap();
    /// result = state.start_of_input();
    /// assert!(result.is_err());
    /// ```
    #[inline]
    pub fn start_of_input(self: Self) -> ParseResult<Self> {
        if self.position.at_start() {
            Ok(self)
        } else {
            Err(self)
        }
    }

    /// Asks the `ParserState` to match the end of the input. If the match is successful, this will
    /// return an `Ok` with the updated `Box<ParserState>`. If failed, an `Err` with the updated
    /// `Box<ParserState>` is returned.
    ///
    /// # Examples
    ///
    /// ```
    /// # use pest;
    /// # #[allow(non_camel_case_types)]
    /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
    /// enum Rule {}
    ///
    /// let input = "ab".as_bytes();
    /// let mut state: Box<pest::ParserState<Rule>> = pest::ParserState::new(input);
    /// let mut result = state.end_of_input();
    /// assert!(result.is_err());
    ///
    /// state = pest::ParserState::new(input);
    /// state = state.match_string("ab").unwrap();
    /// result = state.end_of_input();
    /// assert!(result.is_ok());
    /// ```
    #[inline]
    pub fn end_of_input(self: Self) -> ParseResult<Self> {
        if self.position.at_end() {
            Ok(self)
        } else {
            Err(self)
        }
    }

    /// Starts a lookahead transformation provided by `f` from the `Box<ParserState>`. It returns
    /// `Ok` with the current `Box<ParserState>` if `f` also returns an `Ok` or `Err` with the current
    /// `Box<ParserState>` otherwise. If `is_positive` is `false`, it swaps the `Ok` and `Err`
    /// together, negating the `Result`.
    ///
    /// # Examples
    ///
    /// ```
    /// # use pest;
    /// # #[allow(non_camel_case_types)]
    /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
    /// enum Rule {
    ///     a
    /// }
    ///
    /// let input = "a".as_bytes();
    /// let pairs: Vec<_> = pest::state(input, |state| {
    ///     state.lookahead(true, |state| {
    ///         state.rule(Rule::a, |s| Ok(s))
    ///     })
    /// }).unwrap().collect();
    ///
    /// assert_eq!(pairs.len(), 0);
    /// ```
    #[inline]
    pub fn lookahead<F>(mut self: Self, is_positive: bool, f: F) -> ParseResult<Self>
    where
        F: FnOnce(Self) -> ParseResult<Self>
    {
        let initial_lookahead = self.lookahead;

        self.lookahead = if is_positive {
            match initial_lookahead {
                Lookahead::None | Lookahead::Positive => Lookahead::Positive,
                Lookahead::Negative => Lookahead::Negative
            }
        } else {
            match initial_lookahead {
                Lookahead::None | Lookahead::Positive => Lookahead::Negative,
                Lookahead::Negative => Lookahead::Positive
            }
        };

        let initial_pos = self.position.clone();

        let result = f(self.checkpoint());

        let result_state = match result {
            Ok(mut new_state) => {
                new_state.position = initial_pos;
                new_state.lookahead = initial_lookahead;
                Ok(new_state.restore())
            }
            Err(mut new_state) => {
                new_state.position = initial_pos;
                new_state.lookahead = initial_lookahead;
                Err(new_state.restore())
            }
        };

        if is_positive {
            result_state
        } else {
            match result_state {
                Ok(state) => Err(state),
                Err(state) => Ok(state)
            }
        }
    }

    /// Transformation which stops `Token`s from being generated according to `is_atomic`.
    ///
    /// # Examples
    ///
    /// ```
    /// # use pest::{self, Atomicity};
    /// # #[allow(non_camel_case_types)]
    /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
    /// enum Rule {
    ///     a
    /// }
    ///
    /// let input = "a".as_bytes();
    /// let pairs: Vec<_> = pest::state(input, |state| {
    ///     state.atomic(Atomicity::Atomic, |s| {
    ///         s.rule(Rule::a, |s| Ok(s))
    ///     })
    /// }).unwrap().collect();
    ///
    /// assert_eq!(pairs.len(), 0);
    /// ```
    #[inline]
    pub fn atomic<F>(mut self: Self, atomicity: Atomicity, f: F) -> ParseResult<Self>
    where
        F: FnOnce(Self) -> ParseResult<Self>
    {
        let initial_atomicity = self.atomicity;
        let should_toggle = self.atomicity != atomicity;

        if should_toggle {
            self.atomicity = atomicity;
        }

        let result = f(self);

        match result {
            Ok(mut new_state) => {
                if should_toggle {
                    new_state.atomicity = initial_atomicity;
                }
                Ok(new_state)
            }
            Err(mut new_state) => {
                if should_toggle {
                    new_state.atomicity = initial_atomicity;
                }
                Err(new_state)
            }
        }
    }

    /// Restores the original state of the `ParserState` when `f` returns an `Err`. Currently,
    /// this method only restores the stack.
    ///
    /// # Examples
    ///
    /// ```
    /// # use pest;
    /// # #[allow(non_camel_case_types)]
    /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
    /// enum Rule {}
    ///
    /// let input = "ab".as_bytes();
    /// let mut state: Box<pest::ParserState<Rule>> = pest::ParserState::new(input);
    /// let mut result = state.restore_on_err(|state| state.stack_push(|state|
    ///     state.match_string("a")).and_then(|state| state.match_string("a"))
    /// );
    ///
    /// assert!(result.is_err());
    ///
    /// // Since the the rule doesn't match, the "a" pushed to the stack will be removed.
    /// let catch_panic = std::panic::catch_unwind(|| result.unwrap_err().stack_pop());
    /// assert!(catch_panic.is_err());
    /// ```
    #[inline]
    pub fn restore_on_err<F>(self: Self, f: F) -> ParseResult<Self>
    where
        F: FnOnce(Self) -> ParseResult<Self>
    {
        match f(self.checkpoint()) {
            Ok(state) => Ok(state),
            Err(state) => Err(state.restore())
        }
    }

    // Mark the current state as a checkpoint and return the `Box`.
    #[inline]
    pub(crate) fn checkpoint(self: Self) -> Self {
        self
    }

    // Restore the current state to the most recent checkpoint.
    #[inline]
    pub(crate) fn restore(self: Self) -> Self {
        self
    }
}
