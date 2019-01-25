// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use ast::*;
use alloc::boxed::Box;
use alloc::collections::BTreeMap;
use alloc::string::String;
use alloc::vec::Vec;

mod concatenator;
mod factorizer;
mod restorer;
mod rotater;
mod skipper;
mod unroller;

pub fn optimize(rules: Vec<Rule>) -> Vec<OptimizedRule> {
    let optimized: Vec<OptimizedRule> = rules
        .into_iter()
        .map(rotater::rotate)
        .map(skipper::skip)
        .map(unroller::unroll)
        .map(concatenator::concatenate)
        .map(factorizer::factor)
        .map(rule_to_optimized_rule)
        .collect();

    let rules_to_exprs = populate_rules_to_exprs(&optimized);
    optimized
        .into_iter()
        .map(|rule| restorer::restore_on_err(rule, &rules_to_exprs))
        .collect()
}

fn rule_to_optimized_rule(rule: Rule) -> OptimizedRule {
    fn to_optimized(expr: Expr) -> OptimizedExpr {
        match expr {
            Expr::Range(start, end) => OptimizedExpr::Range(start, end),
            Expr::Ident(ident) => OptimizedExpr::Ident(ident),
            Expr::PosPred(expr) => OptimizedExpr::PosPred(Box::new(to_optimized(*expr))),
            Expr::NegPred(expr) => OptimizedExpr::NegPred(Box::new(to_optimized(*expr))),
            Expr::Seq(lhs, rhs) => {
                OptimizedExpr::Seq(Box::new(to_optimized(*lhs)), Box::new(to_optimized(*rhs)))
            }
            Expr::Choice(lhs, rhs) => {
                OptimizedExpr::Choice(Box::new(to_optimized(*lhs)), Box::new(to_optimized(*rhs)))
            }
            Expr::Opt(expr) => OptimizedExpr::Opt(Box::new(to_optimized(*expr))),
            Expr::Rep(expr) => OptimizedExpr::Rep(Box::new(to_optimized(*expr))),
            Expr::Skip(strings) => OptimizedExpr::Skip(strings),
            _ => unreachable!("No valid transformation to OptimizedRule")
        }
    }

    OptimizedRule {
        name: rule.name,
        ty: rule.ty,
        expr: to_optimized(rule.expr)
    }
}

fn populate_rules_to_exprs(rules: &[OptimizedRule]) -> BTreeMap<String, OptimizedExpr> {
    rules
        .iter()
        .map(|r| (r.name.clone(), r.expr.clone()))
        .collect()
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct OptimizedRule {
    pub name: String,
    pub ty: RuleType,
    pub expr: OptimizedExpr
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum OptimizedExpr {
    Range(String, String),
    Ident(String),
    PosPred(Box<OptimizedExpr>),
    NegPred(Box<OptimizedExpr>),
    Seq(Box<OptimizedExpr>, Box<OptimizedExpr>),
    Choice(Box<OptimizedExpr>, Box<OptimizedExpr>),
    Opt(Box<OptimizedExpr>),
    Rep(Box<OptimizedExpr>),
    Skip(Vec<String>),
    RestoreOnErr(Box<OptimizedExpr>)
}

impl OptimizedExpr {
    pub fn iter_top_down(&self) -> OptimizedExprTopDownIterator {
        OptimizedExprTopDownIterator::new(self)
    }

    pub fn map_top_down<F>(self, mut f: F) -> OptimizedExpr
    where
        F: FnMut(OptimizedExpr) -> OptimizedExpr
    {
        fn map_internal<F>(expr: OptimizedExpr, f: &mut F) -> OptimizedExpr
        where
            F: FnMut(OptimizedExpr) -> OptimizedExpr
        {
            let expr = f(expr);

            match expr {
                // TODO: Use box syntax when it gets stabilized.
                OptimizedExpr::PosPred(expr) => {
                    let mapped = Box::new(map_internal(*expr, f));
                    OptimizedExpr::PosPred(mapped)
                }
                OptimizedExpr::NegPred(expr) => {
                    let mapped = Box::new(map_internal(*expr, f));
                    OptimizedExpr::NegPred(mapped)
                }
                OptimizedExpr::Seq(lhs, rhs) => {
                    let mapped_lhs = Box::new(map_internal(*lhs, f));
                    let mapped_rhs = Box::new(map_internal(*rhs, f));
                    OptimizedExpr::Seq(mapped_lhs, mapped_rhs)
                }
                OptimizedExpr::Choice(lhs, rhs) => {
                    let mapped_lhs = Box::new(map_internal(*lhs, f));
                    let mapped_rhs = Box::new(map_internal(*rhs, f));
                    OptimizedExpr::Choice(mapped_lhs, mapped_rhs)
                }
                OptimizedExpr::Rep(expr) => {
                    let mapped = Box::new(map_internal(*expr, f));
                    OptimizedExpr::Rep(mapped)
                }
                OptimizedExpr::Opt(expr) => {
                    let mapped = Box::new(map_internal(*expr, f));
                    OptimizedExpr::Opt(mapped)
                }
                expr => expr
            }
        }

        map_internal(self, &mut f)
    }

    pub fn map_bottom_up<F>(self, mut f: F) -> OptimizedExpr
    where
        F: FnMut(OptimizedExpr) -> OptimizedExpr
    {
        fn map_internal<F>(expr: OptimizedExpr, f: &mut F) -> OptimizedExpr
        where
            F: FnMut(OptimizedExpr) -> OptimizedExpr
        {
            let mapped = match expr {
                OptimizedExpr::PosPred(expr) => {
                    // TODO: Use box syntax when it gets stabilized.
                    let mapped = Box::new(map_internal(*expr, f));
                    OptimizedExpr::PosPred(mapped)
                }
                OptimizedExpr::NegPred(expr) => {
                    let mapped = Box::new(map_internal(*expr, f));
                    OptimizedExpr::NegPred(mapped)
                }
                OptimizedExpr::Seq(lhs, rhs) => {
                    let mapped_lhs = Box::new(map_internal(*lhs, f));
                    let mapped_rhs = Box::new(map_internal(*rhs, f));
                    OptimizedExpr::Seq(mapped_lhs, mapped_rhs)
                }
                OptimizedExpr::Choice(lhs, rhs) => {
                    let mapped_lhs = Box::new(map_internal(*lhs, f));
                    let mapped_rhs = Box::new(map_internal(*rhs, f));
                    OptimizedExpr::Choice(mapped_lhs, mapped_rhs)
                }
                OptimizedExpr::Rep(expr) => {
                    let mapped = Box::new(map_internal(*expr, f));
                    OptimizedExpr::Rep(mapped)
                }
                OptimizedExpr::Opt(expr) => {
                    let mapped = Box::new(map_internal(*expr, f));
                    OptimizedExpr::Opt(mapped)
                }
                expr => expr
            };

            f(mapped)
        }

        map_internal(self, &mut f)
    }
}

pub struct OptimizedExprTopDownIterator {
    current: Option<OptimizedExpr>,
    next: Option<OptimizedExpr>,
    right_branches: Vec<OptimizedExpr>
}

impl OptimizedExprTopDownIterator {
    pub fn new(expr: &OptimizedExpr) -> Self {
        let mut iter = OptimizedExprTopDownIterator {
            current: None,
            next: None,
            right_branches: vec![]
        };
        iter.iterate_expr(expr.clone());
        iter
    }

    fn iterate_expr(&mut self, expr: OptimizedExpr) {
        self.current = Some(expr.clone());
        match expr {
            OptimizedExpr::Seq(lhs, rhs) => {
                self.right_branches.push(*rhs);
                self.next = Some(*lhs);
            }
            OptimizedExpr::Choice(lhs, rhs) => {
                self.right_branches.push(*rhs);
                self.next = Some(*lhs);
            }
            OptimizedExpr::PosPred(expr)
            | OptimizedExpr::NegPred(expr)
            | OptimizedExpr::Rep(expr)
            | OptimizedExpr::Opt(expr) => {
                self.next = Some(*expr);
            }
            _ => {
                self.next = None;
            }
        }
    }
}

impl Iterator for OptimizedExprTopDownIterator {
    type Item = OptimizedExpr;

    fn next(&mut self) -> Option<Self::Item> {
        let result = self.current.take();

        if let Some(expr) = self.next.take() {
            self.iterate_expr(expr);
        } else if let Some(expr) = self.right_branches.pop() {
            self.iterate_expr(expr);
        }

        result
    }
}
