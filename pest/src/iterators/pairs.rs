// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use alloc::string::String;
use alloc::vec::Vec;
use core::fmt;
use core::hash::{Hash, Hasher};
use core::ptr;
use alloc::rc::Rc;

use super::flat_pairs::{self, FlatPairs};
use super::pair::{self, Pair};
use super::queueable_token::QueueableToken;
use super::tokens::{self, Tokens};
use RuleType;

/// A `struct` containing `Pairs`. It is created by [`pest::state`](../fn.state.html) and
/// [`Pair::into_inner`](struct.Pair.html#method.into_inner).
#[derive(Clone)]
pub struct Pairs<'i, R> {
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
) -> Pairs<R> {
    Pairs {
        queue,
        input,
        start,
        end,
        little_endian
    }
}

impl<'i, R: RuleType> Pairs<'i, R> {
    /// Flattens the `Pairs`.
    ///
    /// # Examples
    ///
    /// ```
    /// # use std::rc::Rc;
    /// # use pest;
    /// # #[allow(non_camel_case_types)]
    /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
    /// enum Rule {
    ///     a,
    ///     b
    /// }
    ///
    /// let input = "".as_bytes();
    /// let pairs = pest::state(input, |state| {
    ///     // generating nested Token pair with Rule::b inside Rule::a
    /// #     state.rule(Rule::a, |state| {
    /// #         state.rule(Rule::b, |s| Ok(s))
    /// #     })
    /// }).unwrap();
    /// let tokens: Vec<_> = pairs.flatten().tokens().collect();
    ///
    /// assert_eq!(tokens.len(), 4);
    /// ```
    #[inline]
    pub fn flatten(self) -> FlatPairs<'i, R> {
        flat_pairs::new(self.queue, self.input, self.start, self.end, self.little_endian)
    }

    /// Converts the `Pairs` into a `TokenIterator`.
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
    /// let tokens: Vec<_> = pairs.tokens().collect();
    ///
    /// assert_eq!(tokens.len(), 2);
    /// ```
    #[inline]
    pub fn tokens(self) -> Tokens<'i, R> {
        tokens::new(self.queue, self.input, self.start, self.end, self.little_endian)
    }

    fn pair(&self) -> usize {
        match self.queue[self.start] {
            QueueableToken::Start {
                end_token_index, ..
            } => end_token_index,
            _ => unreachable!()
        }
    }

    fn pair_from_end(&self) -> usize {
        match self.queue[self.end - 1] {
            QueueableToken::End {
                start_token_index, ..
            } => start_token_index,
            _ => unreachable!()
        }
    }
}

impl<'i, R: RuleType> Iterator for Pairs<'i, R> {
    type Item = Pair<'i, R>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.start >= self.end {
            return None;
        }

        let pair = pair::new(Rc::clone(&self.queue), self.input, self.start, self.little_endian);

        self.start = self.pair() + 1;

        Some(pair)
    }
}

impl<'i, R: RuleType> DoubleEndedIterator for Pairs<'i, R> {
    fn next_back(&mut self) -> Option<Self::Item> {
        if self.end <= self.start {
            return None;
        }

        self.end = self.pair_from_end();

        let pair = pair::new(Rc::clone(&self.queue), self.input, self.end, self.little_endian);

        Some(pair)
    }
}

impl<'i, R: RuleType> fmt::Debug for Pairs<'i, R> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_list().entries(self.clone()).finish()
    }
}

impl<'i, R: RuleType> fmt::Display for Pairs<'i, R> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            let pairs_vec = self.clone()
                .map(|pair| format!("{}", pair))
                .collect::<Vec<_>>();

            let mut pairs_str = String::new();

            for (i, e) in pairs_vec.iter().enumerate() {
                pairs_str = format!("{}{}", pairs_str, e);

                if i < pairs_vec.len() - 1 {
                    pairs_str = format!("{}{}", pairs_str, ", ");
                }
            }

        write!(
            f,
            "[{}]",
            pairs_str
        )
    }
}

impl<'i, R: PartialEq> PartialEq for Pairs<'i, R> {
    fn eq(&self, other: &Pairs<'i, R>) -> bool {
        Rc::ptr_eq(&self.queue, &other.queue) && ptr::eq(self.input, other.input)
            && self.start == other.start && self.end == other.end
    }
}

impl<'i, R: Eq> Eq for Pairs<'i, R> {}

impl<'i, R: Hash> Hash for Pairs<'i, R> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        (&*self.queue as *const Vec<QueueableToken<R>>).hash(state);
        (self.input as *const [u8]).hash(state);
        self.start.hash(state);
        self.end.hash(state);
    }
}
