// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

#![no_std]

#![feature(alloc)]
#[macro_use]
extern crate alloc;

use core::fmt::Display;

pub mod ast;
pub mod optimizer;

pub fn unwrap_or_report<T, E>(result: Result<T, E>) -> T
where
    E: IntoIterator,
    E::Item: Display
{
    result.unwrap_or_else(|_| {
        panic!("grammar error");
    })
}
