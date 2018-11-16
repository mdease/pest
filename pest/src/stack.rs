// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

/// Implementation of a `Stack` which maintains an log of `StackOp`s in order to rewind the stack
/// to a previous state.

use alloc::vec::{Vec};

#[derive(Debug)]
pub struct Stack<T: Clone> {
    ops: Vec<StackOp<T>>,
    cache: Vec<T>,
    snapshots: Vec<usize>,
    current_snapshot: usize
}

impl<T: Clone> Stack<T> {
    /// Creates a new `Stack`.
    pub fn new() -> Self {
        Stack {
            ops: vec![],
            cache: vec![],
            snapshots: vec![0],
            current_snapshot: 0
        }
    }

    /// Returns `true` if the stack is currently empty.
    #[allow(dead_code)]
    pub fn is_empty(&self) -> bool {
        self.cache.is_empty()
    }

    /// Returns the top-most `&T` in the `Stack`.
    pub fn peek(&self) -> Option<&T> {
        self.cache.last()
    }

    /// Pushes a `T` onto the `Stack`.
    pub fn push(&mut self, elem: T) {
        self.ops.push(StackOp::Push(elem.clone()));
        self.cache.push(elem);
    }

    /// Pops the top-most `T` from the `Stack`.
    pub fn pop(&mut self) -> Option<T> {
        let popped = self.cache.pop();
        if let Some(ref val) = popped {
            self.ops.push(StackOp::Pop(val.clone()));
        }
        popped
    }

    /// Returns an iterator to the current state of the cache in fifo order.
    pub fn iter(&self) -> impl Iterator<Item = &T> {
        self.cache.iter().rev()
    }

    /// Takes a snapshot of the current `Stack`.
    pub fn snapshot(&mut self) {
        let ops_index = self.ops.len();
        if ops_index > self.current_snapshot {
            self.snapshots.push(ops_index);
            self.current_snapshot = ops_index;
        }
    }

    /// Rewinds the `Stack` to the most recent `snapshot()`. If no `snapshot()` has been taken, this
    /// function will do nothing.
    pub fn restore(&mut self) {
        if let Some(ops_index) = self.snapshots.pop() {
            self.rewind_to(ops_index);
            self.ops.truncate(ops_index);
            self.current_snapshot = self.ops.len();
        }
    }

    // Rewind the stack to a particular index
    fn rewind_to(&mut self, index: usize) {
        let ops_to_rewind = &self.ops[index..];
        for op in ops_to_rewind.iter().rev() {
            match op {
                &StackOp::Push(_) => {
                    self.cache.pop();
                }
                &StackOp::Pop(ref elem) => {
                    self.cache.push(elem.clone());
                }
            }
        }
    }
}

#[derive(Debug)]
enum StackOp<T> {
    Push(T),
    Pop(T)
}
