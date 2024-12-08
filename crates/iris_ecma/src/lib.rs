mod traverse;

pub mod toolchain {
    pub use oxc::*;
    pub use oxc_resolver as resolver;
    pub use oxc_traverse as traverse;
    pub use oxc_ecmascript as utils;
    pub use oxc_index as index;
}

#[cfg(test)]
mod tests {
    use oxc::allocator::Allocator;
    use oxc::parser::Parser;
    use oxc::semantic::SemanticBuilder;
    use oxc::span::SourceType;

    #[test]
    fn test() {
        let code = r#"
            'use strict';
            function foo() {
                return 1;
                if (true) {
                    return 2;
                }
                do {
                    return 3;
                } while (false);
                for (let i = 0; i < 10; i++) {
                    return 4;
                }
                (function() {
                    return 5;
                })();
                (() => {
                    return 6;
                })();
                for (a in Object.keys({a: 1, b: 2})) {
                    return 7;
                }
                using f = s ? (v ?? x) : y;
                sdo.dsk2['s']?.ds.toString?.();
                `hello ${world}`;
            }
        "#;
        let allocator = Allocator::default();
        let mut program = Parser::new(&allocator, code, SourceType::mjs()).parse().program;
        let ret = SemanticBuilder::new()
            // Estimate transformer will triple scopes, symbols, references
            .with_excess_capacity(2.0)
            .build(&program);
        let (symbols, scopes) = ret.semantic.into_symbol_table_and_scope_tree();
        let mut traverser = crate::traverse::IrisTraverseToIR::new();
        traverser.build(&allocator, &mut program, symbols, scopes);
        println!("{:#?}", traverser.ir);
    }
}
