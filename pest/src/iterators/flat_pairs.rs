// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use alloc::vec::Vec;
use core::fmt;
use alloc::rc::Rc;

use super::pair::{self, Pair};
use super::queueable_token::QueueableToken;
use super::tokens::{self, Tokens};
use RuleType;

/// A `struct` containing `Pairs`. It is created by
/// [`Pairs::flatten`](struct.Pairs.html#method.flatten).
pub struct FlatPairs<'i, R> {
    queue: Rc<Vec<QueueableToken<R>>>,
    input: &'i [u8],
    start: usize,
    end: usize,
    little_endian: bool
}

pub fn new<R: RuleType>(
    queue: Rc<Vec<QueueableToken<R>>>,
    input: &[u8],
    start: usize,
    end: usize,
    little_endian: bool
) -> FlatPairs<R> {
    FlatPairs {
        queue,
        input,
        start,
        end,
        little_endian
    }
}

impl<'i, R: RuleType> FlatPairs<'i, R> {
    /// Converts the `FlatPairs` into a `TokenIterator`.
    ///
    /// # Examples
    ///
    /// ```
    /// # use std::rc::Rc;
    /// # use pest;
    /// # #[allow(non_camel_case_types)]
    /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
    /// enum Rule {
    ///     a
    /// }
    ///
    /// let input = "".as_bytes();
    /// let pairs = pest::state(input, |state| {
    ///     // generating Token pair with Rule::a ...
    /// #     state.rule(Rule::a, |s| Ok(s))
    /// }).unwrap();
    /// let tokens: Vec<_> = pairs.flatten().tokens().collect();
    ///
    /// assert_eq!(tokens.len(), 2);
    /// ```
    #[inline]
    pub fn tokens(self) -> Tokens<'i, R> {
        tokens::new(self.queue, self.input, self.start, self.end, self.little_endian)
    }

    fn next_start(&mut self) {
        self.start += 1;

        while self.start < self.end && !self.is_start(self.start) {
            self.start += 1;
        }
    }

    fn next_start_from_end(&mut self) {
        self.end -= 1;

        while self.end >= self.start && !self.is_start(self.end) {
            self.end -= 1;
        }
    }

    fn is_start(&self, index: usize) -> bool {
        match self.queue[index] {
            QueueableToken::Start { .. } => true,
            QueueableToken::End { .. } => false
        }
    }
}

impl<'i, R: RuleType> Iterator for FlatPairs<'i, R> {
    type Item = Pair<'i, R>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.start >= self.end {
            return None;
        }

        let pair = pair::new(Rc::clone(&self.queue), self.input, self.start, self.little_endian);

        self.next_start();

        Some(pair)
    }
}

impl<'i, R: RuleType> DoubleEndedIterator for FlatPairs<'i, R> {
    fn next_back(&mut self) -> Option<Self::Item> {
        if self.end <= self.start {
            return None;
        }

        self.next_start_from_end();

        let pair = pair::new(Rc::clone(&self.queue), self.input, self.end, self.little_endian);

        Some(pair)
    }
}

impl<'i, R: RuleType> fmt::Debug for FlatPairs<'i, R> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("FlatPairs")
            .field("pairs", &self.clone().collect::<Vec<_>>())
            .finish()
    }
}

impl<'i, R: Clone> Clone for FlatPairs<'i, R> {
    fn clone(&self) -> FlatPairs<'i, R> {
        FlatPairs {
            queue: Rc::clone(&self.queue),
            input: self.input,
            start: self.start,
            end: self.end,
            little_endian: self.little_endian
        }
    }
}
