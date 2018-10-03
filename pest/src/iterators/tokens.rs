// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use std::fmt;
use std::rc::Rc;

use super::queueable_token::QueueableToken;
use RuleType;
use position;
use token::Token;

/// A `struct` containing `Token`s. It is returned by either
/// [`Pair::into_iter`](struct.Pair.html#method.into_iter) or
/// [`Pairs::into_iter`](struct.Pairs.html#method.into_iter)
#[derive(Clone)]
pub struct Tokens<'i, R> {
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
) -> Tokens<R> {
    Tokens {
        queue,
        input,
        start,
        end,
        little_endian
    }
}

impl<'i, R: RuleType> Tokens<'i, R> {
    fn create_token(&self, index: usize) -> Token<'i, R> {
        match self.queue[index] {
            QueueableToken::Start {
                end_token_index,
                input_pos
            } => {
                let rule = match self.queue[end_token_index] {
                    QueueableToken::End { rule, .. } => rule,
                    _ => unreachable!()
                };

                Token::Start {
                    rule,
                    // QueueableTokens are safely created.
                    // TODO here
                    pos: unsafe { position::new(self.input, input_pos, self.little_endian) }
                }
            }
            QueueableToken::End {
                rule, input_pos, ..
            } => {
                Token::End {
                    rule,
                    // QueueableTokens are safely created.
                    // TODO here
                    pos: unsafe { position::new(self.input, input_pos, self.little_endian) }
                }
            }
        }
    }
}

impl<'i, R: RuleType> Iterator for Tokens<'i, R> {
    type Item = Token<'i, R>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.start >= self.end {
            return None;
        }

        let token = self.create_token(self.start);

        self.start += 1;

        Some(token)
    }
}

impl<'i, R: RuleType> DoubleEndedIterator for Tokens<'i, R> {
    fn next_back(&mut self) -> Option<Self::Item> {
        if self.end <= self.start {
            return None;
        }

        let token = self.create_token(self.end - 1);

        self.end -= 1;

        Some(token)
    }
}

impl<'i, R: RuleType> fmt::Debug for Tokens<'i, R> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_list().entries(self.clone()).finish()
    }
}
