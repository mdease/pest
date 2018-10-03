// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use pest::Span;

use ast::RuleType;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ParserRule<'i> {
    pub name: String,
    pub span: Span<'i>,
    pub ty: RuleType,
    pub node: ParserNode<'i>
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ParserNode<'i> {
    pub expr: ParserExpr<'i>,
    pub span: Span<'i>
}

impl<'i> ParserNode<'i> {
    pub fn filter_map_top_down<F, T>(self, mut f: F) -> Vec<T>
    where
        F: FnMut(ParserNode<'i>) -> Option<T>
    {
        pub fn filter_internal<'i, F, T>(node: ParserNode<'i>, f: &mut F, result: &mut Vec<T>)
        where
            F: FnMut(ParserNode<'i>) -> Option<T>
        {
            if let Some(value) = f(node.clone()) {
                result.push(value);
            }

            match node.expr {
                // TODO: Use box syntax when it gets stabilized.
                ParserExpr::PosPred(node) => {
                    filter_internal(*node, f, result);
                }
                ParserExpr::NegPred(node) => {
                    filter_internal(*node, f, result);
                }
                ParserExpr::Seq(lhs, rhs) => {
                    filter_internal(*lhs, f, result);
                    filter_internal(*rhs, f, result);
                }
                ParserExpr::Choice(lhs, rhs) => {
                    filter_internal(*lhs, f, result);
                    filter_internal(*rhs, f, result);
                }
                ParserExpr::Rep(node) => {
                    filter_internal(*node, f, result);
                }
                ParserExpr::RepOnce(node) => {
                    filter_internal(*node, f, result);
                }
                ParserExpr::RepExact(node, _) => {
                    filter_internal(*node, f, result);
                }
                ParserExpr::RepMin(node, _) => {
                    filter_internal(*node, f, result);
                }
                ParserExpr::RepMax(node, _) => {
                    filter_internal(*node, f, result);
                }
                ParserExpr::RepMinMax(node, ..) => {
                    filter_internal(*node, f, result);
                }
                ParserExpr::Opt(node) => {
                    filter_internal(*node, f, result);
                }
                ParserExpr::Push(node) => {
                    filter_internal(*node, f, result);
                }
                _ => ()
            }
        }

        let mut result = vec![];

        filter_internal(self, &mut f, &mut result);

        result
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ParserExpr<'i> {
    Str(String),
    Insens(String),
    Range(String, String),
    Ident(String),
    PosPred(Box<ParserNode<'i>>),
    NegPred(Box<ParserNode<'i>>),
    Seq(Box<ParserNode<'i>>, Box<ParserNode<'i>>),
    Choice(Box<ParserNode<'i>>, Box<ParserNode<'i>>),
    Opt(Box<ParserNode<'i>>),
    Rep(Box<ParserNode<'i>>),
    RepOnce(Box<ParserNode<'i>>),
    RepExact(Box<ParserNode<'i>>, u32),
    RepMin(Box<ParserNode<'i>>, u32),
    RepMax(Box<ParserNode<'i>>, u32),
    RepMinMax(Box<ParserNode<'i>>, u32, u32),
    Push(Box<ParserNode<'i>>)
}
