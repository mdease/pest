// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use std::collections::HashMap;

use quote::Tokens;
use syn::{Generics, Ident};

use pest_meta::ast::*;
use pest_meta::optimizer::*;

pub fn generate(
    name: Ident,
    generics: &Generics,
    rules: Vec<OptimizedRule>,
    defaults: Vec<&str>
) -> Tokens {
    let uses_eoi = defaults.iter().any(|name| *name == "EOI");

    let builtins = generate_builtin_rules();
    let rule_enum = generate_enum(&rules, uses_eoi);
    let patterns = generate_patterns(&rules, uses_eoi);
    let skip = generate_skip(&rules);

    let mut rules: Vec<_> = rules.into_iter().map(|rule| generate_rule(rule)).collect();
    rules.extend(
        defaults
            .into_iter()
            .map(|name| builtins.get(name).unwrap().clone())
    );

    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let parser_impl = quote! {
        impl #impl_generics ::pest::Parser<Rule> for #name #ty_generics #where_clause {
            fn parse<'i>(
                rule: Rule,
                input: &'i [u8]
            ) -> ::std::result::Result<
                ::pest::iterators::Pairs<'i, Rule>,
                ::pest::error::Error<Rule>
            > {
                mod rules {
                    use super::Rule;

                    #( #rules )*
                    #skip
                }

                ::pest::state(input, |state| {
                    match rule {
                        #patterns
                    }
                })
            }
        }
    };

    quote! {
        #rule_enum
        #parser_impl
    }
}

// Note: All builtin rules should be validated as pest keywords in meta/src/validator.rs.
fn generate_builtin_rules() -> HashMap<&'static str, Tokens> {
    let mut builtins = HashMap::new();

    insert_public_builtin!(
        builtins,
        EOI,
        state.rule(Rule::EOI, |state| state.end_of_input())
    );
    insert_builtin!(builtins, ANY, state.skip(1));
    insert_builtin!(builtins, SOI, state.start_of_input());
    insert_builtin!(builtins, PEEK, state.stack_peek());
    insert_builtin!(builtins, PEEK_ALL, state.stack_match_peek());
    insert_builtin!(builtins, POP, state.stack_pop());
    insert_builtin!(builtins, POP_ALL, state.stack_match_pop());
    insert_builtin!(builtins, DROP, state.stack_drop());

    insert_builtin!(builtins, I8, state.match_i8());
    insert_builtin!(builtins, U8, state.match_u8());
    insert_builtin!(builtins, I16, state.match_i16());
    insert_builtin!(builtins, U16, state.match_u16());
    insert_builtin!(builtins, I32, state.match_i32());
    insert_builtin!(builtins, U32, state.match_u32());
    insert_builtin!(builtins, I64, state.match_i64());
    insert_builtin!(builtins, U64, state.match_u64());
    insert_builtin!(builtins, ISIZE, state.match_isize());
    insert_builtin!(builtins, USIZE, state.match_usize());
    insert_builtin!(builtins, F32, state.match_f32());
    insert_builtin!(builtins, F64, state.match_f64());
    insert_builtin!(builtins, BOOL, state.match_bool());
    insert_builtin!(builtins, CHAR, state.match_char());
    insert_builtin!(builtins, LE, state.set_le());
    insert_builtin!(builtins, BE, state.set_be());

    insert_builtin!(builtins, DIGIT, state.match_range('0'..'9'));
    insert_builtin!(builtins, NONZERO_DIGIT, state.match_range('1'..'9'));
    insert_builtin!(builtins, BIN_DIGIT, state.match_range('0'..'1'));
    insert_builtin!(builtins, OCT_DIGIT, state.match_range('0'..'7'));
    insert_builtin!(
        builtins,
        HEX_DIGIT,
        state.match_range('0'..'9')
            .or_else(|state| state.match_range('a'..'f'))
            .or_else(|state| state.match_range('A'..'F'))
    );
    insert_builtin!(builtins, ALPHA_LOWER, state.match_range('a'..'z'));
    insert_builtin!(builtins, ALPHA_UPPER, state.match_range('A'..'Z'));
    insert_builtin!(
        builtins,
        ALPHA,
        state.match_range('a'..'z')
            .or_else(|state| state.match_range('A'..'Z'))
    );
    insert_builtin!(
        builtins,
        ALPHANUMERIC,
        state.match_range('a'..'z')
            .or_else(|state| state.match_range('A'..'Z'))
            .or_else(|state| state.match_range('0'..'9'))
    );
    insert_builtin!(builtins, ASCII, state.match_range('\x00'..'\x7f'));
    insert_builtin!(
        builtins,
        NEWLINE,
        state.match_string("\n")
            .or_else(|state| state.match_string("\r\n"))
            .or_else(|state| state.match_string("\r"))
    );

    builtins
}

fn generate_enum(rules: &Vec<OptimizedRule>, uses_eoi: bool) -> Tokens {
    let rules = rules.iter().map(|rule| Ident::from(rule.name.as_str()));
    if uses_eoi {
        quote! {
            #[allow(dead_code, non_camel_case_types)]
            #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
            pub enum Rule {
                EOI,
                #( #rules ),*
            }
        }
    } else {
        quote! {
            #[allow(dead_code, non_camel_case_types)]
            #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
            pub enum Rule {
                #( #rules ),*
            }
        }
    }
}

fn generate_patterns(rules: &Vec<OptimizedRule>, uses_eoi: bool) -> Tokens {
    let mut rules: Vec<Tokens> = rules
        .iter()
        .map(|rule| {
            let rule = Ident::from(rule.name.as_str());
            quote! {
                Rule::#rule => rules::#rule(state)
            }
        })
        .collect();

    if uses_eoi {
        rules.push(quote! {
            Rule::EOI => rules::EOI(state)
        });
    }

    quote! {
        #( #rules ),*
    }
}

fn generate_rule(rule: OptimizedRule) -> Tokens {
    let name = Ident::from(rule.name);
    let expr = if { rule.ty == RuleType::Atomic || rule.ty == RuleType::CompoundAtomic } {
        generate_expr_atomic(rule.expr)
    } else {
        if name == "WHITESPACE" || name == "COMMENT" {
            let atomic = generate_expr_atomic(rule.expr);

            quote! {
                state.atomic(::pest::Atomicity::Atomic, |state| {
                    #atomic
                })
            }
        } else {
            generate_expr(rule.expr)
        }
    };

    match rule.ty {
        RuleType::Normal => quote! {
            #[inline]
            #[allow(non_snake_case, unused_variables)]
            pub fn #name(state: Box<::pest::ParserState<Rule>>) -> ::pest::ParseResult<Box<::pest::ParserState<Rule>>> {
                state.rule(Rule::#name, |state| {
                    #expr
                })
            }
        },
        RuleType::Silent => quote! {
            #[inline]
            #[allow(non_snake_case, unused_variables)]
            pub fn #name(state: Box<::pest::ParserState<Rule>>) -> ::pest::ParseResult<Box<::pest::ParserState<Rule>>> {
                #expr
            }
        },
        RuleType::Atomic => quote! {
            #[inline]
            #[allow(non_snake_case, unused_variables)]
            pub fn #name(state: Box<::pest::ParserState<Rule>>) -> ::pest::ParseResult<Box<::pest::ParserState<Rule>>> {
                state.rule(Rule::#name, |state| {
                    state.atomic(::pest::Atomicity::Atomic, |state| {
                        #expr
                    })
                })
            }
        },
        RuleType::CompoundAtomic => quote! {
            #[inline]
            #[allow(non_snake_case, unused_variables)]
            pub fn #name(state: Box<::pest::ParserState<Rule>>) -> ::pest::ParseResult<Box<::pest::ParserState<Rule>>> {
                state.atomic(::pest::Atomicity::CompoundAtomic, |state| {
                    state.rule(Rule::#name, |state| {
                        #expr
                    })
                })
            }
        },
        RuleType::NonAtomic => quote! {
            #[inline]
            #[allow(non_snake_case, unused_variables)]
            pub fn #name(state: Box<::pest::ParserState<Rule>>) -> ::pest::ParseResult<Box<::pest::ParserState<Rule>>> {
                state.atomic(::pest::Atomicity::NonAtomic, |state| {
                    state.rule(Rule::#name, |state| {
                        #expr
                    })
                })
            }
        }
    }
}

fn generate_skip(rules: &Vec<OptimizedRule>) -> Tokens {
    let whitespace = rules.iter().any(|rule| rule.name == "WHITESPACE");
    let comment = rules.iter().any(|rule| rule.name == "COMMENT");

    match (whitespace, comment) {
        (false, false) => generate_rule!(skip, Ok(state)),
        (true, false) => generate_rule!(
            skip,
            if state.atomicity() == ::pest::Atomicity::NonAtomic {
                state.repeat(|state| {
                    WHITESPACE(state)
                })
            } else {
                Ok(state)
            }
        ),
        (false, true) => generate_rule!(
            skip,
            if state.atomicity() == ::pest::Atomicity::NonAtomic {
                state.repeat(|state| {
                    COMMENT(state)
                })
            } else {
                Ok(state)
            }
        ),
        (true, true) => generate_rule!(
            skip,
            if state.atomicity() == ::pest::Atomicity::NonAtomic {
                state.sequence(|state| {
                    state.repeat(|state| {
                        WHITESPACE(state)
                    }).and_then(|state| {
                        state.repeat(|state| {
                            state.sequence(|state| {
                                COMMENT(state).and_then(|state| {
                                    state.repeat(|state| {
                                        WHITESPACE(state)
                                    })
                                })
                            })
                        })
                    })
                })
            } else {
                Ok(state)
            }
        )
    }
}

fn generate_expr(expr: OptimizedExpr) -> Tokens {
    match expr {
        OptimizedExpr::Str(string) => {
            quote! {
                state.match_string(#string)
            }
        }
        OptimizedExpr::Insens(string) => {
            quote! {
                state.match_insensitive(#string)
            }
        }
        OptimizedExpr::Range(start, end) => {
            let start = start.chars().next().unwrap();
            let end = end.chars().next().unwrap();

            quote! {
                state.match_range(#start..#end)
            }
        }
        OptimizedExpr::Ident(ident) => {
            let ident = Ident::from(ident);
            quote! { self::#ident(state) }
        }
        OptimizedExpr::PosPred(expr) => {
            let expr = generate_expr(*expr);

            quote! {
                state.lookahead(true, |state| {
                    #expr
                })
            }
        }
        OptimizedExpr::NegPred(expr) => {
            let expr = generate_expr(*expr);

            quote! {
                state.lookahead(false, |state| {
                    #expr
                })
            }
        }
        OptimizedExpr::Seq(lhs, rhs) => {
            let head = generate_expr(*lhs);
            let mut tail = vec![];
            let mut current = *rhs;

            while let OptimizedExpr::Seq(lhs, rhs) = current {
                tail.push(generate_expr(*lhs));
                current = *rhs;
            }
            tail.push(generate_expr(current));

            quote! {
                state.sequence(|state| {
                    #head
                    #(
                        .and_then(|state| {
                            self::skip(state)
                        }).and_then(|state| {
                            #tail
                        })
                    )*
                })
            }
        }
        OptimizedExpr::Choice(lhs, rhs) => {
            let head = generate_expr(*lhs);
            let mut tail = vec![];
            let mut current = *rhs;

            while let OptimizedExpr::Choice(lhs, rhs) = current {
                tail.push(generate_expr(*lhs));
                current = *rhs;
            }
            tail.push(generate_expr(current));

            quote! {
                #head
                #(
                    .or_else(|state| {
                        #tail
                    })
                )*
            }
        }
        OptimizedExpr::Opt(expr) => {
            let expr = generate_expr(*expr);

            quote! {
                state.optional(|state| {
                    #expr
                })
            }
        }
        OptimizedExpr::Rep(expr) => {
            let expr = generate_expr(*expr);

            quote! {
                state.sequence(|state| {
                    state.optional(|state| {
                        #expr.and_then(|state| {
                            state.repeat(|state| {
                                state.sequence(|state| {
                                    self::skip(
                                        state
                                    ).and_then(|state| {
                                        #expr
                                    })
                                })
                            })
                        })
                    })
                })
            }
        }
        OptimizedExpr::Skip(strings) => {
            quote! {
                let strings = [#(#strings),*];

                state.skip_until(&strings)
            }
        }
        OptimizedExpr::Push(expr) => {
            let expr = generate_expr(*expr);

            quote! {
                state.stack_push(|state| #expr)
            }
        }
        OptimizedExpr::RestoreOnErr(expr) => {
            let expr = generate_expr(*expr);

            quote! {
                state.restore_on_err(|state| #expr)
            }
        }
    }
}

fn generate_expr_atomic(expr: OptimizedExpr) -> Tokens {
    match expr {
        OptimizedExpr::Str(string) => {
            quote! {
                state.match_string(#string)
            }
        }
        OptimizedExpr::Insens(string) => {
            quote! {
                state.match_insensitive(#string)
            }
        }
        OptimizedExpr::Range(start, end) => {
            let start = start.chars().next().unwrap();
            let end = end.chars().next().unwrap();

            quote! {
                state.match_range(#start..#end)
            }
        }
        OptimizedExpr::Ident(ident) => {
            let ident = Ident::from(ident);
            quote! { self::#ident(state) }
        }
        OptimizedExpr::PosPred(expr) => {
            let expr = generate_expr_atomic(*expr);

            quote! {
                state.lookahead(true, |state| {
                    #expr
                })
            }
        }
        OptimizedExpr::NegPred(expr) => {
            let expr = generate_expr_atomic(*expr);

            quote! {
                state.lookahead(false, |state| {
                    #expr
                })
            }
        }
        OptimizedExpr::Seq(lhs, rhs) => {
            let head = generate_expr_atomic(*lhs);
            let mut tail = vec![];
            let mut current = *rhs;

            while let OptimizedExpr::Seq(lhs, rhs) = current {
                tail.push(generate_expr_atomic(*lhs));
                current = *rhs;
            }
            tail.push(generate_expr_atomic(current));

            quote! {
                state.sequence(|state| {
                    #head
                    #(
                        .and_then(|state| {
                            #tail
                        })
                    )*
                })
            }
        }
        OptimizedExpr::Choice(lhs, rhs) => {
            let head = generate_expr_atomic(*lhs);
            let mut tail = vec![];
            let mut current = *rhs;

            while let OptimizedExpr::Choice(lhs, rhs) = current {
                tail.push(generate_expr_atomic(*lhs));
                current = *rhs;
            }
            tail.push(generate_expr_atomic(current));

            quote! {
                #head
                #(
                    .or_else(|state| {
                        #tail
                    })
                )*
            }
        }
        OptimizedExpr::Opt(expr) => {
            let expr = generate_expr_atomic(*expr);

            quote! {
                state.optional(|state| {
                    #expr
                })
            }
        }
        OptimizedExpr::Rep(expr) => {
            let expr = generate_expr_atomic(*expr);

            quote! {
                state.repeat(|state| {
                    #expr
                })
            }
        }
        OptimizedExpr::Skip(strings) => {
            quote! {
                let strings = [#(#strings),*];

                state.skip_until(&strings)
            }
        }
        OptimizedExpr::Push(expr) => {
            let expr = generate_expr_atomic(*expr);

            quote! {
                state.stack_push(|state| #expr)
            }
        }
        OptimizedExpr::RestoreOnErr(expr) => {
            let expr = generate_expr_atomic(*expr);

            quote! {
                state.restore_on_err(|state| #expr)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn rule_enum_simple() {
        let rules = vec![
            OptimizedRule {
                name: "f".to_owned(),
                ty: RuleType::Normal,
                expr: OptimizedExpr::Ident("g".to_owned())
            },
        ];

        assert_eq!(
            generate_enum(&rules, false),
            quote! {
                #[allow(dead_code, non_camel_case_types)]
                #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
                pub enum Rule {
                    f
                }
            }
        );
    }

    #[test]
    fn sequence() {
        let expr = OptimizedExpr::Seq(
            Box::new(OptimizedExpr::Str("a".to_owned())),
            Box::new(OptimizedExpr::Seq(
                Box::new(OptimizedExpr::Str("b".to_owned())),
                Box::new(OptimizedExpr::Seq(
                    Box::new(OptimizedExpr::Str("c".to_owned())),
                    Box::new(OptimizedExpr::Str("d".to_owned()))
                ))
            ))
        );

        assert_eq!(
            generate_expr(expr),
            quote! {
                state.sequence(|state| {
                    state.match_string("a").and_then(|state| {
                        self::skip(state)
                    }).and_then(|state| {
                        state.match_string("b")
                    }).and_then(|state| {
                        self::skip(state)
                    }).and_then(|state| {
                        state.match_string("c")
                    }).and_then(|state| {
                        self::skip(state)
                    }).and_then(|state| {
                        state.match_string("d")
                    })
                })
            }
        );
    }

    #[test]
    fn sequence_atomic() {
        let expr = OptimizedExpr::Seq(
            Box::new(OptimizedExpr::Str("a".to_owned())),
            Box::new(OptimizedExpr::Seq(
                Box::new(OptimizedExpr::Str("b".to_owned())),
                Box::new(OptimizedExpr::Seq(
                    Box::new(OptimizedExpr::Str("c".to_owned())),
                    Box::new(OptimizedExpr::Str("d".to_owned()))
                ))
            ))
        );

        assert_eq!(
            generate_expr_atomic(expr),
            quote! {
                state.sequence(|state| {
                    state.match_string("a").and_then(|state| {
                        state.match_string("b")
                    }).and_then(|state| {
                        state.match_string("c")
                    }).and_then(|state| {
                        state.match_string("d")
                    })
                })
            }
        );
    }

    #[test]
    fn choice() {
        let expr = OptimizedExpr::Choice(
            Box::new(OptimizedExpr::Str("a".to_owned())),
            Box::new(OptimizedExpr::Choice(
                Box::new(OptimizedExpr::Str("b".to_owned())),
                Box::new(OptimizedExpr::Choice(
                    Box::new(OptimizedExpr::Str("c".to_owned())),
                    Box::new(OptimizedExpr::Str("d".to_owned()))
                ))
            ))
        );

        assert_eq!(
            generate_expr(expr),
            quote! {
                state.match_string("a").or_else(|state| {
                    state.match_string("b")
                }).or_else(|state| {
                    state.match_string("c")
                }).or_else(|state| {
                    state.match_string("d")
                })
            }
        );
    }

    #[test]
    fn choice_atomic() {
        let expr = OptimizedExpr::Choice(
            Box::new(OptimizedExpr::Str("a".to_owned())),
            Box::new(OptimizedExpr::Choice(
                Box::new(OptimizedExpr::Str("b".to_owned())),
                Box::new(OptimizedExpr::Choice(
                    Box::new(OptimizedExpr::Str("c".to_owned())),
                    Box::new(OptimizedExpr::Str("d".to_owned()))
                ))
            ))
        );

        assert_eq!(
            generate_expr_atomic(expr),
            quote! {
                state.match_string("a").or_else(|state| {
                    state.match_string("b")
                }).or_else(|state| {
                    state.match_string("c")
                }).or_else(|state| {
                    state.match_string("d")
                })
            }
        );
    }

    #[test]
    fn skip() {
        let expr = OptimizedExpr::Skip(vec!["a".to_owned(), "b".to_owned()]);

        assert_eq!(
            generate_expr_atomic(expr),
            quote! {
                let strings = ["a", "b"];

                state.skip_until(&strings)
            }
        );
    }

    #[test]
    fn expr_complex() {
        let expr = OptimizedExpr::Choice(
            Box::new(OptimizedExpr::Ident("a".to_owned())),
            Box::new(OptimizedExpr::Seq(
                Box::new(OptimizedExpr::Range("a".to_owned(), "b".to_owned())),
                Box::new(OptimizedExpr::Seq(
                    Box::new(OptimizedExpr::NegPred(Box::new(OptimizedExpr::Rep(
                        Box::new(OptimizedExpr::Insens("b".to_owned()))
                    )))),
                    Box::new(OptimizedExpr::PosPred(Box::new(OptimizedExpr::Opt(
                        Box::new(OptimizedExpr::Rep(Box::new(OptimizedExpr::Choice(
                            Box::new(OptimizedExpr::Str("c".to_owned())),
                            Box::new(OptimizedExpr::Str("d".to_owned()))
                        ))))
                    ))))
                ))
            ))
        );

        let sequence = quote! {
            state.sequence(|state| {
                self::skip(state).and_then(
                    |state| {
                        state.match_insensitive("b")
                    }
                )
            })
        };
        let repeat = quote! {
            state.repeat(|state| {
                state.sequence(|state| {
                    self::skip(state).and_then(|state| {
                        state.match_string("c")
                            .or_else(|state| {
                                state.match_string("d")
                            })
                     })
                })
            })
        };
        assert_eq!(
            generate_expr(expr),
            quote! {
                self::a(state).or_else(|state| {
                    state.sequence(|state| {
                        state.match_range('a'..'b').and_then(|state| {
                            self::skip(state)
                        }).and_then(|state| {
                            state.lookahead(false, |state| {
                                state.sequence(|state| {
                                    state.optional(|state| {
                                        state.match_insensitive(
                                            "b"
                                        ).and_then(|state| {
                                            state.repeat(|state| {
                                                #sequence
                                            })
                                        })
                                    })
                                })
                            })
                        }).and_then(|state| {
                            self::skip(state)
                        }).and_then(|state| {
                            state.lookahead(true, |state| {
                                state.optional(|state| {
                                    state.sequence(|state| {
                                        state.optional(|state| {
                                            state.match_string("c")
                                            .or_else(|state| {
                                                state.match_string("d")
                                            }).and_then(|state| {
                                                #repeat
                                            })
                                        })
                                    })
                                })
                            })
                        })
                    })
                })
            }
        );
    }

    #[test]
    fn expr_complex_atomic() {
        let expr = OptimizedExpr::Choice(
            Box::new(OptimizedExpr::Ident("a".to_owned())),
            Box::new(OptimizedExpr::Seq(
                Box::new(OptimizedExpr::Range("a".to_owned(), "b".to_owned())),
                Box::new(OptimizedExpr::Seq(
                    Box::new(OptimizedExpr::NegPred(Box::new(OptimizedExpr::Rep(
                        Box::new(OptimizedExpr::Insens("b".to_owned()))
                    )))),
                    Box::new(OptimizedExpr::PosPred(Box::new(OptimizedExpr::Opt(
                        Box::new(OptimizedExpr::Rep(Box::new(OptimizedExpr::Choice(
                            Box::new(OptimizedExpr::Str("c".to_owned())),
                            Box::new(OptimizedExpr::Str("d".to_owned()))
                        ))))
                    ))))
                ))
            ))
        );

        assert_eq!(
            generate_expr_atomic(expr),
            quote! {
                self::a(state).or_else(|state| {
                    state.sequence(|state| {
                        state.match_range('a'..'b').and_then(|state| {
                            state.lookahead(false, |state| {
                                state.repeat(|state| {
                                    state.match_insensitive("b")
                                })
                            })
                        }).and_then(|state| {
                            state.lookahead(true, |state| {
                                state.optional(|state| {
                                    state.repeat(|state| {
                                        state.match_string("c")
                                           .or_else(|state| {
                                            state.match_string("d")
                                        })
                                    })
                                })
                            })
                        })
                    })
                })
            }
        );
    }

    #[test]
    fn generate_complete() {
        let name = Ident::from("MyParser");
        let generics = Generics::default();
        let rules = vec![
            OptimizedRule {
                name: "a".to_owned(),
                ty: RuleType::Silent,
                expr: OptimizedExpr::Str("b".to_owned())
            },
        ];
        let defaults = vec!["ANY"];

        assert_eq!(
            generate(name, &generics, rules, defaults),
            quote! {
                #[allow(dead_code, non_camel_case_types)]
                #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
                pub enum Rule {
                    a
                }

            impl ::pest::Parser<Rule> for MyParser {
                fn parse<'i>(
                    rule: Rule,
                    input: &'i [u8]
                ) -> ::std::result::Result<
                    ::pest::iterators::Pairs<'i, Rule>,
                    ::pest::error::Error<Rule>
                > {
                    mod rules {
                        use super::Rule;

                            #[inline]
                            #[allow(non_snake_case, unused_variables)]
                            pub fn a(state: Box<::pest::ParserState<Rule>>) -> ::pest::ParseResult<Box<::pest::ParserState<Rule>>> {
                                state.match_string("b")
                            }

                            #[inline]
                            #[allow(dead_code, non_snake_case, unused_variables)]
                            fn ANY(state: Box<::pest::ParserState<Rule>>) -> ::pest::ParseResult<Box<::pest::ParserState<Rule>>> {
                                state.skip(1)
                            }

                            #[inline]
                            #[allow(dead_code, non_snake_case, unused_variables)]
                            fn skip(state: Box<::pest::ParserState<Rule>>) -> ::pest::ParseResult<Box<::pest::ParserState<Rule>>> {
                                Ok(state)
                            }
                        }

                        ::pest::state(input, |state| {
                            match rule {
                                Rule::a => rules::a(state)
                            }
                        })
                    }
                }
            }
        );
    }
}
