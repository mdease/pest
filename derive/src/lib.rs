// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

//! # pest. The Elegant Parser
//!
//! pest is a [PEG](https://en.wikipedia.org/wiki/Parsing_expression_grammar) parser built with
//! *simplicity* and *speed* in mind.
//!
//! This crate works in conjunction with the [`pest` crate](https://docs.rs/pest) by
//! deriving a grammar implementation based on a provided grammar.
//!
//! ## `.pest` files
//!
//! Grammar definitions reside in custom `.pest` files located in the `src` directory. Their path is
//! relative to `src` and is specified between the `derive` attribute and empty `struct` that
//! `Parser` will be derived on.
//!
//! Because of a limitation in procedural macros, there is no way for Cargo to know that a module
//! needs to be recompiled based on the file that the procedural macro is opening. This leads to the
//! case where modifying a `.pest` file without touching the file where the `derive` is does not
//! recompile it if it already has a working binary in the cache. To avoid this issue, the grammar
//! file can be included in a dummy `const` definition while debugging.
//!
//! ```ignore
//! #[cfg(debug_assertions)]
//! const _GRAMMAR: &'static str = include_str!("path/to/my_grammar.pest"); // relative to this file
//!
//! #[derive(Parser)]
//! #[grammar = "path/to/my_grammar.pest"] // relative to src
//! struct MyParser;
//! ```
//!
//! ## Grammar
//!
//! A grammar is a series of rules separated by whitespace, possibly containing comments.
//!
//! ### Comments
//!
//! Comments start with `//` and end at the end of the line.
//!
//! ```ignore
//! // a comment
//! ```
//!
//! ### Rules
//!
//! Rules have the following form:
//!
//! ```ignore
//! name = optional_modifier { expression }
//! ```
//!
//! The name of the rule is formed from alphanumeric characters or `_` with the condition that the
//! first character is not a digit and is used to create token pairs. When the rule starts being
//! parsed, the starting part of the token is being produced, with the ending part being produced
//! when the rule finishes parsing.
//!
//! The following token pair notation `a(b(), c())` denotes the tokens: start `a`, start `b`, end
//! `b`, start `c`, end `c`, end `a`.
//!
//! #### Modifiers
//!
//! Modifiers are optional and can be one of `_`, `@`, `$`, or `!`. These modifiers change the
//! behavior of the rules.
//!
//! 1. Silent (`_`)
//!
//!     Silent rules do not create token pairs during parsing, nor are they error-reported.
//!
//!     ```ignore
//!     a = _{ "a" }
//!     b =  { a ~ "b" }
//!     ```
//!
//!     Parsing `"ab"` produces the token pair `b()`.
//!
//! 2. Atomic (`@`)
//!
//!     Atomic rules do not accept whitespace or comments within their expressions and have a
//!     cascading effect on any rule they call. I.e. rules that are not atomic but are called by atomic
//!     rules behave atomically.
//!
//!     Any rules called by atomic rules do not generate token pairs.
//!
//!     ```ignore
//!     a =  { "a" }
//!     b = @{ a ~ "b" }
//!
//!     WHITESPACE = _{ " " }
//!     ```
//!
//!     Parsing `"ab"` produces the token pair `b()`, while `"a   b"` produces an error.
//!
//! 3. Compound-atomic (`$`)
//!
//!     Compound-atomic are identical to atomic rules with the exception that rules called by them are
//!     not forbidden from generating token pairs.
//!
//!     ```ignore
//!     a =  { "a" }
//!     b = ${ a ~ "b" }
//!
//!     WHITESPACE = _{ " " }
//!     ```
//!
//!     Parsing `"ab"` produces the token pairs `b(a())`, while `"a   b"` produces an error.
//!
//! 4. Non-atomic (`!`)
//!
//!     Non-atomic are identical to normal rules with the exception that they stop the cascading effect
//!     of atomic and compound-atomic rules.
//!
//!     ```ignore
//!     a =  { "a" }
//!     b = !{ a ~ "b" }
//!     c = @{ b }
//!
//!     WHITESPACE = _{ " " }
//!     ```
//!
//!     Parsing both `"ab"` and `"a   b"` produce the token pairs `c(a())`.
//!
//! #### Expressions
//!
//! Expressions can be either terminals or non-terminals.
//!
//! 1. Terminals
//!
//!     | Terminal   | Usage                                                          |
//!     |------------|----------------------------------------------------------------|
//!     | `"a"`      | matches the exact string `"a"`                                 |
//!     | `^"a"`     | matches the exact string `"a"` case insensitively (ASCII only) |
//!     | `'a'..'z'` | matches one character between `'a'` and `'z'`                  |
//!     | `a`        | matches rule `a`                                               |
//!
//! Strings and characters follow
//! [Rust's escape mechanisms](https://doc.rust-lang.org/reference/tokens.html#byte-escapes), while
//! identifiers can contain alpha-numeric characters and underscores (`_`), as long as they do not
//! start with a digit.
//!
//! 2. Non-terminals
//!
//!     | Non-terminal          | Usage                                                      |
//!     |-----------------------|------------------------------------------------------------|
//!     | `(e)`                 | matches `e`                                                |
//!     | `e1 ~ e2`             | matches the sequence `e1` `e2`                             |
//!     | <code>e1 \| e2</code> | matches either `e1` or `e2`                                |
//!     | `e*`                  | matches `e` zero or more times                             |
//!     | `e+`                  | matches `e` one or more times                              |
//!     | `e{n}`                | matches `e` exactly `n` times                              |
//!     | `e{, n}`              | matches `e` at most `n` times                              |
//!     | `e{n,} `              | matches `e` at least `n` times                             |
//!     | `e{m, n}`             | matches `e` between `m` and `n` times inclusively          |
//!     | `e?`                  | optionally matches `e`                                     |
//!     | `&e`                  | matches `e` without making progress                        |
//!     | `!e`                  | matches if `e` doesn't match without making progress       |
//!     | `PUSH(e)`             | matches `e` and pushes it's captured string down the stack |
//!
//!     where `e`, `e1`, and `e2` are expressions.
//!
//! Expressions can modify the stack only if they match the input. For example,
//! if `e1` in the compound expression `e1 | e2` does not match the input, then
//! it does not modify the stack, so `e2` sees the stack in the same state as
//! `e1` did. Repetitions and optionals (`e*`, `e+`, `e{, n}`, `e{n,}`,
//! `e{m,n}`, `e?`) can modify the stack each time `e` matches. The `!e` and `&e`
//! expressions are a special case; they never modify the stack.
//!
//! ## Special rules
//!
//! Special rules can be called within the grammar. They are:
//!
//! * `WHITESPACE` - runs between rules and sub-rules
//! * `COMMENT` - runs between rules and sub-rules
//! * `ANY` - matches exactly one `char`
//! * `SOI` - (start-of-input) matches only when a `Parser` is still at the starting position
//! * `EOI` - (end-of-input) matches only when a `Parser` has reached its end
//! * `POP` - pops a string from the stack and matches it
//! * `POP_ALL` - pops the entire state of the stack and matches it
//! * `PEEK` - peeks a string from the stack and matches it
//! * `PEEK_ALL` - peeks the entire state of the stack and matches it
//! * `DROP` - drops the top of the stack (fails to match if the stack is empty)
//!
//! `WHITESPACE` and `COMMENT` should be defined manually if needed. All other rules cannot be
//! overridden.
//!
//! ## `WHITESPACE` and `COMMENT`
//!
//! When defined, these rules get matched automatically in sequences (`~`) and repetitions
//! (`*`, `+`) between expressions. Atomic rules and those rules called by atomic rules are exempt
//! from this behavior.
//!
//! These rules should be defined so as to match one whitespace character and one comment only since
//! they are run in repetitions.
//!
//! If both `WHITESPACE` and `COMMENT` are defined, this grammar:
//!
//! ```ignore
//! a = { b ~ c }
//! ```
//!
//! is effectively transformed into this one behind the scenes:
//!
//! ```ignore
//! a = { b ~ WHITESPACE* ~ (COMMENT ~ WHITESPACE*)* ~ c }
//! ```
//!
//! ## `PUSH`, `POP`, `DROP`, and `PEEK`
//!
//! `PUSH(e)` simply pushes the captured string of the expression `e` down a stack. This stack can
//! then later be used to match grammar based on its content with `POP` and `PEEK`.
//!
//! `PEEK` always matches the string at the top of stack. So, if the stack contains `["a", "b"]`,
//! the this grammar:
//!
//! ```ignore
//! a = { PEEK }
//! ```
//!
//! is effectively transformed into at parse time:
//!
//! ```ignore
//! a = { "a" }
//! ```
//!
//! `POP` works the same way with the exception that it pops the string off of the stack if the
//! the match worked. With the stack from above, if `POP` matches `"a"`, the stack will be mutated
//! to `["b"]`.
//!
//! `DROP` makes it possible to remove the string at the top of the stack
//! without matching it. If the stack is nonempty, `DROP` drops the top of the
//! stack. If the stack is empty, then `DROP` fails to match.
//!
//! ## `Rule`
//!
//! All rules defined or used in the grammar populate a generated `enum` called `Rule`. This
//! implements `pest`'s `RuleType` and can be used throughout the API.
//!
//! ## `Built-in rules`
//!
//! Pest also comes with a number of built-in rules for convenience. They are:
//!
//! * `DIGIT` - matches a numeric character from 0..9
//! * `NONZERO_DIGIT` - matches a numeric character from 1..9
//! * `BIN_DIGIT` - matches a numeric character from 0..1
//! * `OCT_DIGIT` - matches a numeric character from 0..7
//! * `HEX_DIGIT` - matches a numeric character from 0..9 or a..f or A..F
//! * `ALPHA_LOWER` - matches a character from a..z
//! * `ALPHA_UPPER` - matches a character from A..Z
//! * `ALPHA` - matches a character from a..z or A..Z
//! * `ALPHANUMERIC` - matches a character from a..z or A..Z or 0..9
//! * `ASCII` - matches a character from \x00..\x7f
//! * `NEWLINE` - matches either "\n" or "\r\n" or "\r"

#![doc(html_root_url = "https://docs.rs/pest_derive")]
#![recursion_limit = "256"]

extern crate pest;
extern crate pest_meta;

extern crate proc_macro;
#[macro_use]
extern crate quote;
extern crate syn;

use std::env;
use std::ffi::OsStr;
use std::fs::File;
use std::io::{self, Read};
use std::path::Path;
use std::str::FromStr;

use proc_macro::TokenStream;
use syn::{DeriveInput, Generics, Ident};
use pest_meta::ast::{Expr, Rule as AstRule, RuleType};

mod builtins;

#[macro_use]
mod generator;

use pest_meta::optimizer;

const PRIMITIVES: &'static [&'static str] = &["i8", "u8", "i16", "u16", "i32", "u32", "i64", "u64", "isize", "usize", "f32", "f64", "bool", "char"];

#[proc_macro_derive(Parser, attributes(grammar))]
pub fn derive_parser(input: TokenStream) -> TokenStream {
    let ast: DeriveInput = syn::parse(input).unwrap();
    let (name, generics, fields, types, defaults, expr) = parse_derive(ast);

    // read the config
    let root = env::var("CARGO_MANIFEST_DIR").unwrap_or(".".into());
    let path = Path::new(&root).join("pest.conf");
    let file_name = match path.file_name() {
        Some(file_name) => file_name,
        None => OsStr::new("")
    };

    // architecture-specific
    // defaults
    let mut _is_little_endian = true;
    let mut _arch_size: u16 = 32;

    if !file_name.is_empty() {
        let data = match read_file(&path) {
            Ok(data) => data,
            Err(_) => String::new()
        };

        // todo, there's probably a better way to do this
        if data.len() > 0 {
            let lines: Vec<&str> = data.split("\n").collect();

            for _ in lines {
                let tokens: Vec<&str> = data.split("=").collect();
                match tokens[0] {
                    "is_little_endian" => { _is_little_endian = FromStr::from_str(tokens[1]).unwrap() },
                    "arch_size" => { _arch_size = u16::from_str(tokens[1]).unwrap() },
                    _ => {}
                }
            }
        }
    }

    let ast = vec![
        AstRule {
            name: name.to_string(),
            ty: RuleType::Normal,
            expr: expr
        }
    ];

    // String to &str
    let defaults = defaults.iter().map(|s| &**s).collect();

    let optimized = optimizer::optimize(ast);
    let generated = generator::generate(name, &generics, optimized, defaults, fields, types);

    generated.into()
}

fn read_file<P: AsRef<Path>>(path: P) -> io::Result<String> {
    let mut file = File::open(path.as_ref())?;
    let mut string = String::new();
    file.read_to_string(&mut string)?;
    Ok(string)
}

fn parse_derive(ast: DeriveInput) -> (Ident, Generics, Vec<String>, Vec<String>, Vec<String>, Expr) {
    let name = ast.ident;
    let generics = ast.generics;

    let mut field_idents = Vec::new();
    let mut types = Vec::new();
    let mut defaults = Vec::new();

    let primitives: Vec<String> = PRIMITIVES.to_vec().iter().map(|p| (*p).to_string()).collect();

    if let syn::Data::Struct(ref data) = ast.data {
        if let syn::Fields::Named(ref fields) = data.fields {
            for f in fields.named.iter() {
                // get the field name
                match f.ident {
                    Some(ident) => field_idents.push(ident.to_string()),
                    None => panic!("Could not get field name")
                }

                // get the type
                if let syn::Type::Path(ref path) = f.ty {
                    let t = path.path.segments[0].ident.to_string();

                    if primitives.contains(&t) && !defaults.contains(&t.to_uppercase()) {
                        defaults.push(t.to_uppercase());
                    }

                    types.push(t);
                }
            }
        }
    }

    let sequence = construct_sequence(&mut types.to_vec());

    (name, generics, field_idents, types, defaults, sequence)
}

fn construct_sequence(types: &mut Vec<String>) -> Expr {
    if types.len() == 1 {
        Expr::Ident(types.remove(0).to_uppercase())
    } else {
        let b0 = Box::new(Expr::Ident(types.remove(0).to_uppercase()));
        let b1 = Box::new(Expr::Ident(types.remove(0).to_uppercase()));
        let mut expr = Expr::Seq(b0, b1);

        for t in types {
            let b = Box::new(Expr::Ident(t.to_uppercase()));
            expr = Expr::Seq(Box::new(expr), b);
        }

        expr
    }
}
