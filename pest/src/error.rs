// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

//! A `mod` containing error data structures.
use core::fmt;

use position::Position;

/// A `struct` defining errors.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Error {
    pub location: InputLocation,
    start: (usize, usize),
    end: Option<(usize, usize)>
}

/// An `enum` describing where the `Error` has occurred.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum InputLocation {
    /// `Error` was created by `Error::new_from_pos`
    Pos(usize),
    /// `Error` was created by `Error::new_from_span`
    Span((usize, usize))
}

impl Error {
    pub fn new_from_pos(pos: Position) -> Error {
        Error {
            location: InputLocation::Pos(pos.pos()),
            start: pos.line_col(),
            end: None
        }
    }

    #[allow(dead_code)]
    fn description(&self) -> &str {
        "Parsing error"
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "An error has occurred {:?}", self.location)
    }
}
