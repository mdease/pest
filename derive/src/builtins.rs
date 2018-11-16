use quote::Tokens;
use syn::Ident;
use alloc::collections::BTreeMap;

pub fn insert_builtin(rules_enum_ident: Ident, builtins: &mut BTreeMap<&'static str, Tokens>, name: &'static str, pattern: Tokens) {
	builtins.insert(name, generate_rule(rules_enum_ident, name, pattern));
}

pub fn insert_public_builtin(rules_enum_ident: Ident, builtins: &mut BTreeMap<&'static str, Tokens>, name: &'static str, pattern: Tokens) {
	builtins.insert(name, generate_public_rule(rules_enum_ident, name, pattern));
}

pub fn generate_rule(rules_enum_ident: Ident, name: &'static str, pattern: Tokens) -> Tokens {
	let ident = Ident::from(name);

	quote!{
		#[inline]
        #[allow(dead_code, non_snake_case, unused_variables)]

        fn #ident(state: Box<::pest::ParserState<#rules_enum_ident>>) -> ::pest::ParseResult<Box<::pest::ParserState<#rules_enum_ident>>> {
            #pattern
        }
	}
}

pub fn generate_public_rule(rules_enum_ident: Ident, name: &'static str, pattern: Tokens) -> Tokens {
	let ident = Ident::from(name);

	quote!{
		#[inline]
        #[allow(dead_code, non_snake_case, unused_variables)]
        pub fn #ident(state: Box<::pest::ParserState<#rules_enum_ident>>) -> ::pest::ParseResult<Box<::pest::ParserState<#rules_enum_ident>>> {
            #pattern
        }
	}
}
