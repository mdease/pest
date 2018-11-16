// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use alloc::prelude::ToString;
use alloc::collections::BTreeMap;
use alloc::string::String;
use alloc::vec::Vec;

use quote::Tokens;
use syn::{Generics, Ident};

use pest_meta::ast::*;
use pest_meta::optimizer::*;

use builtins;

pub fn generate(
    name: Ident,
    generics: &Generics,
    rules: Vec<OptimizedRule>,
    defaults: Vec<&str>,
    fields: Vec<String>,
    types: Vec<String>
) -> Tokens {
    let uses_eoi = defaults.iter().any(|name| *name == "EOI");

    let mut types_set: Vec<String> = Vec::new();

    for t in &types {
        if !types_set.contains(&t) {
            types_set.push(t.to_string());
        }
    }

    let mut rules_string: String = name.to_string();
    rules_string.push_str("RuleEnum");
    let rules_enum_ident = Ident::from(rules_string);

    let builtins = generate_builtin_rules(rules_enum_ident);
    let rule_enum = generate_enum(rules_enum_ident, &rules, uses_eoi);
    let skip = generate_skip(rules_enum_ident, &rules);

    let mut rule_str = rules[0].name.clone();
    rule_str.push_str("Rule");
    let rule_ident = Ident::from(rule_str);

    let mut rules: Vec<_> = rules.into_iter().map(|rule| generate_rule(rules_enum_ident, rule)).collect();

    rules.extend(
        types_set.iter().map(|t| {
            let upper = t.to_uppercase();
            // primitive or struct?
            if defaults.contains(&upper.as_str()) {
                builtins.get(&upper.as_str()).unwrap().clone()
            } else {
                let upper_ident = Ident::from(upper);
                let ident = Ident::from(t.as_str());
                quote! {
                    #[inline]
                    #[allow(dead_code, non_snake_case, unused_variables)]
                    pub fn #upper_ident(state: Box<::pest::ParserState<#rules_enum_ident>>) -> ::pest::ParseResult<Box<::pest::ParserState<#rules_enum_ident>>> {
                        state.skip(::#ident::size())
                    }
                }
            }
        })
    );

    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let parser_impl = quote! {
        impl #impl_generics ::pest::Parser<#rules_enum_ident> for #name #ty_generics #where_clause {
            fn parse<'i>(
                input: &'i [u8]
            ) -> ::std::result::Result<
                ::pest::iterators::Pairs<'i, #rules_enum_ident>,
                ::pest::error::Error<#rules_enum_ident>
            > {
                mod rules {
                    use super::#rules_enum_ident;

                    #( #rules )*
                    #skip
                }

                ::pest::state(input, |state| {
                    rules::#rule_ident(state)
                })
            }
        }
    };

    // calculate struct size
    let mut size: usize = 0;
    let mut struct_size = Vec::new();

    for t in &types {
        if defaults.contains(&t.to_uppercase().as_str()) {
            size += ::pest::reader::size(&t.as_str());
        } else {
            let t_ident = Ident::from(t.as_str());
            let addition = quote! {
                + #t_ident::size()
            };
            struct_size.push(addition);
        }
    }

    // assign a casting function to each type
    let field_idents: Vec<Ident> = fields.iter().map(|f| Ident::from(f.as_str())).collect();
    let casters: Vec<Tokens> = types.iter().map(|t| {
        let t_ident = Ident::from(t.as_str());
        if defaults.contains(&t.to_uppercase().as_str()) {
            quote!{::pest::reader::#t_ident(le, bytes)}
        } else {
            quote!{#t_ident::parse_and_create(le, bytes)}
        }
    }).collect();

    quote! {
        #rule_enum
        #parser_impl

        impl #name {
            fn create(le: bool, bytes: &mut Vec<u8>) -> #name {
                #name {
                    #(#field_idents: #casters),*,
                }
            }

            fn size() -> usize {
                #size #(#struct_size)*
            }

            fn parse_and_create<'i>(
                le: bool,
                input: &'i mut Vec<u8>
            ) -> #name {
                #name::parse(input.as_slice()).unwrap_or_else(|e| panic!("{}", e));
                #name::create(le, input)
            }

            fn validate(
                le: bool,
                ptr: *const u8
            ) {
                unsafe {
                    let slice = std::slice::from_raw_parts(ptr, #name::size());

                    #name::parse_and_create(le, &mut slice.to_vec());
                }
            }
        }
    }
}

// Note: All builtin rules should be validated as pest keywords in meta/src/validator.rs.
fn generate_builtin_rules(rules_enum_ident: Ident) -> BTreeMap<&'static str, Tokens> {
    let mut builtins = BTreeMap::new();
    // force ref to go out of scope
    {
        let builtins_ref = &mut builtins;

        builtins::insert_public_builtin(
            rules_enum_ident,
            builtins_ref,
            "EOI",
            quote!{state.rule(#rules_enum_ident::EOI, |state| state.end_of_input())}
        );
        builtins::insert_builtin(rules_enum_ident, builtins_ref, "ANY", quote!{state.skip(1)});
        builtins::insert_builtin(rules_enum_ident, builtins_ref, "SOI", quote!{state.start_of_input()});
        builtins::insert_builtin(rules_enum_ident, builtins_ref, "PEEK", quote!{state.stack_peek()});
        builtins::insert_builtin(rules_enum_ident, builtins_ref, "PEEK_ALL", quote!{state.stack_match_peek()});
        builtins::insert_builtin(rules_enum_ident, builtins_ref, "POP", quote!{state.stack_pop()});
        builtins::insert_builtin(rules_enum_ident, builtins_ref, "POP_ALL", quote!{state.stack_match_pop()});
        builtins::insert_builtin(rules_enum_ident, builtins_ref, "DROP", quote!{state.stack_drop()});

        builtins::insert_builtin(rules_enum_ident, builtins_ref, "I8", quote!{state.match_i8()});
        builtins::insert_builtin(rules_enum_ident, builtins_ref, "U8", quote!{state.match_u8()});
        builtins::insert_builtin(rules_enum_ident, builtins_ref, "I16", quote!{state.match_i16()});
        builtins::insert_builtin(rules_enum_ident, builtins_ref, "U16", quote!{state.match_u16()});
        builtins::insert_builtin(rules_enum_ident, builtins_ref, "I32", quote!{state.match_i32()});
        builtins::insert_builtin(rules_enum_ident, builtins_ref, "U32", quote!{state.match_u32()});
        builtins::insert_builtin(rules_enum_ident, builtins_ref, "I64", quote!{state.match_i64()});
        builtins::insert_builtin(rules_enum_ident, builtins_ref, "U64", quote!{state.match_u64()});
        builtins::insert_builtin(rules_enum_ident, builtins_ref, "ISIZE", quote!{state.match_isize()});
        builtins::insert_builtin(rules_enum_ident, builtins_ref, "USIZE", quote!{state.match_usize()});
        builtins::insert_builtin(rules_enum_ident, builtins_ref, "F32", quote!{state.match_f32()});
        builtins::insert_builtin(rules_enum_ident, builtins_ref, "F64", quote!{state.match_f64()});
        builtins::insert_builtin(rules_enum_ident, builtins_ref, "BOOL", quote!{state.match_bool()});
        builtins::insert_builtin(rules_enum_ident, builtins_ref, "CHAR", quote!{state.match_char()});
        builtins::insert_builtin(rules_enum_ident, builtins_ref, "LE", quote!{state.set_le()});
        builtins::insert_builtin(rules_enum_ident, builtins_ref, "BE", quote!{state.set_be()});

        builtins::insert_builtin(rules_enum_ident, builtins_ref, "DIGIT", quote!{state.match_range('0'..'9')});
        builtins::insert_builtin(rules_enum_ident, builtins_ref, "NONZERO_DIGIT", quote!{state.match_range('1'..'9')});
        builtins::insert_builtin(rules_enum_ident, builtins_ref, "BIN_DIGIT", quote!{state.match_range('0'..'1')});
        builtins::insert_builtin(rules_enum_ident, builtins_ref, "OCT_DIGIT", quote!{state.match_range('0'..'7')});
        builtins::insert_builtin(
            rules_enum_ident,
            builtins_ref,
            "HEX_DIGIT",
            quote! {
                state.match_range('0'..'9')
                .or_else(|state| state.match_range('a'..'f'))
                .or_else(|state| state.match_range('A'..'F'))
            }
        );
        builtins::insert_builtin(rules_enum_ident, builtins_ref, "ALPHA_LOWER", quote!{state.match_range('a'..'z')});
        builtins::insert_builtin(rules_enum_ident, builtins_ref, "ALPHA_UPPER", quote!{state.match_range('A'..'Z')});
        builtins::insert_builtin(
            rules_enum_ident,
            builtins_ref,
            "ALPHA",
            quote! {
                state.match_range('a'..'z')
                .or_else(|state| state.match_range('A'..'Z'))
            }
        );
        builtins::insert_builtin(
            rules_enum_ident,
            builtins_ref,
            "ALPHANUMERIC",
            quote! {
                state.match_range('a'..'z')
                .or_else(|state| state.match_range('A'..'Z'))
                .or_else(|state| state.match_range('0'..'9'))
            }
        );
        builtins::insert_builtin(rules_enum_ident, builtins_ref, "ASCII", quote!{state.match_range('\x00'..'\x7f')});
        builtins::insert_builtin(
            rules_enum_ident,
            builtins_ref,
            "NEWLINE",
            quote! {
                state.match_string("\n")
                .or_else(|state| state.match_string("\r\n"))
                .or_else(|state| state.match_string("\r"))
            }
        );
    }

    builtins
}

fn generate_enum(rules_enum_ident: Ident, rules: &Vec<OptimizedRule>, uses_eoi: bool) -> Tokens {
    let mut rule_string = rules[0].name.clone();
    rule_string.push_str("Rule");
    let rule_enum_ident = Ident::from(rule_string);

    if uses_eoi {
        quote! {
            #[allow(dead_code, non_camel_case_types)]
            #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
            pub enum #rules_enum_ident {
                EOI,
                #rule_enum_ident
            }
        }
    } else {
        quote! {
            #[allow(dead_code, non_camel_case_types)]
            #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
            pub enum #rules_enum_ident {
                #rule_enum_ident
            }
        }
    }
}

fn generate_rule(rules_enum_ident: Ident, rule: OptimizedRule) -> Tokens {
    let mut name_str = rule.name;
    name_str.push_str("Rule");
    let name = Ident::from(name_str);
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
            pub fn #name(state: Box<::pest::ParserState<#rules_enum_ident>>) -> ::pest::ParseResult<Box<::pest::ParserState<#rules_enum_ident>>> {
                state.rule(#rules_enum_ident::#name, |state| {
                    #expr
                })
            }
        },
        RuleType::Silent => quote! {
            #[inline]
            #[allow(non_snake_case, unused_variables)]
            pub fn #name(state: Box<::pest::ParserState<#rules_enum_ident>>) -> ::pest::ParseResult<Box<::pest::ParserState<#rules_enum_ident>>> {
                #expr
            }
        },
        RuleType::Atomic => quote! {
            #[inline]
            #[allow(non_snake_case, unused_variables)]
            pub fn #name(state: Box<::pest::ParserState<#rules_enum_ident>>) -> ::pest::ParseResult<Box<::pest::ParserState<#rules_enum_ident>>> {
                state.rule(#rules_enum_ident::#name, |state| {
                    state.atomic(::pest::Atomicity::Atomic, |state| {
                        #expr
                    })
                })
            }
        },
        RuleType::CompoundAtomic => quote! {
            #[inline]
            #[allow(non_snake_case, unused_variables)]
            pub fn #name(state: Box<::pest::ParserState<#rules_enum_ident>>) -> ::pest::ParseResult<Box<::pest::ParserState<#rules_enum_ident>>> {
                state.atomic(::pest::Atomicity::CompoundAtomic, |state| {
                    state.rule(#rules_enum_ident::#name, |state| {
                        #expr
                    })
                })
            }
        },
        RuleType::NonAtomic => quote! {
            #[inline]
            #[allow(non_snake_case, unused_variables)]
            pub fn #name(state: Box<::pest::ParserState<#rules_enum_ident>>) -> ::pest::ParseResult<Box<::pest::ParserState<#rules_enum_ident>>> {
                state.atomic(::pest::Atomicity::NonAtomic, |state| {
                    state.rule(#rules_enum_ident::#name, |state| {
                        #expr
                    })
                })
            }
        }
    }
}

fn generate_skip(rules_enum_ident: Ident, rules: &Vec<OptimizedRule>) -> Tokens {
    let whitespace = rules.iter().any(|rule| rule.name == "WHITESPACE");
    let comment = rules.iter().any(|rule| rule.name == "COMMENT");

    match (whitespace, comment) {
        (false, false) => builtins::generate_rule(rules_enum_ident, "skip", quote!{Ok(state)}),
        (true, false) => builtins::generate_rule(
            rules_enum_ident,
            "skip",
            quote! {
                if state.atomicity() == ::pest::Atomicity::NonAtomic {
                    state.repeat(|state| {
                        WHITESPACE(state)
                    })
                } else {
                    Ok(state)
                }
            }
        ),
        (false, true) => builtins::generate_rule(
            rules_enum_ident,
            "skip",
            quote! {
                if state.atomicity() == ::pest::Atomicity::NonAtomic {
                    state.repeat(|state| {
                        COMMENT(state)
                    })
                } else {
                    Ok(state)
                }
            }
        ),
        (true, true) => builtins::generate_rule(
            rules_enum_ident,
            "skip",
            quote! {
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
