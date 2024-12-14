
#[cfg(test)]
mod tests {
    use std::path::Path;
    use oxc::allocator::Allocator;
    use oxc::parser::{ParseOptions, Parser};
    use oxc::semantic::SemanticBuilder;
    use oxc::span::SourceType;
    use iris_python::PythonCharacterization;
    use ruff::codegen::Stylist;
    use ruff::parser::Mode;

    #[test]
    fn test() {
        let code = r#"
'use strict';
function a(d, e=1, ...b) { return d + e }
const amplifier = (x) => x * 2;
function hello() {
    return 1;
    if (true) {
        return 2;
    }
    do {
        return 3;
    } while (false);
    for (i = 0; i < 10; i++) {
        return 4;
    }
    console.log(a instanceof String);
    console.log(typeof 1234);
    console.log(void 0);
    delete x;
    d = a ? b : c;
    for (a in Object.keys({a: 1, b: 2})) {
        return 7;
    }
    sdo.dsk2['s'].ds.toString();
    `hello ${world}`;
}
        "#;
        println!("Original Code:\n{}", code);
        let allocator = Allocator::default();
        let mut options = ParseOptions::default();
        options.allow_return_outside_function = true;
        let mut program = Parser::new(&allocator, code, SourceType::mjs()).with_options(options).parse().program;
        let ret = SemanticBuilder::new()
            // Estimate transformer will triple scopes, symbols, references
            .with_excess_capacity(2.0)
            .build(&program);
        let (symbols, scopes) = ret.semantic.into_symbol_table_and_scope_tree();
        let mut transformer = iris_ecma::EcmaLower::new(&allocator, Path::new("test.js"));
        let transformed = transformer.transformer.build_with_symbols_and_scopes(symbols, scopes, &mut program);
        // println!("{:#?}", program.body);
        let mut traverser = iris_ecma::EcmaGeneralization::new();
        traverser.build(&allocator, &mut program, transformed.symbols, transformed.scopes);
        // println!("{:#?}", traverser.ir);
        let ir = traverser.ir.unwrap();
        let mut chara = PythonCharacterization::new();
        chara.build(ir);
        // println!("{:#?}", chara.ast);
        let tokens = ruff::parser::parse("print('Hello, world!')", Mode::Module).unwrap();
        let stylist = Stylist::from_tokens(&tokens.tokens(), "print('Hello, world!')");
        let mut final_code = String::new();
        for stmt in chara.ast.unwrap().body.iter() {
            let mut generator = ruff::codegen::Generator::from(&stylist);
            final_code += generator.stmt(&stmt).as_str();
            final_code += "\n";
        }
        println!("Transformed Code:\n{}", final_code);
    }

    #[test]
    fn bin_search_code() {
        let code = r#"
function binarySearch(arr, target) {
    let left = 0;
    let right = arr.length - 1;

    while (left <= right) {
        const mid = Math.floor((left + right) / 2);

        if (arr[mid] === target) {
            return mid; // Target found, return its index
        } else if (arr[mid] < target) {
            left = mid + 1; // Move to the right half
        } else {
            right = mid - 1; // Move to the left half
        }
    }

    return -1; // Target not found
}

// Example usage
const sortedArray = [1, 3, 5, 7, 9, 11, 13];
const target = 7;
const result = binarySearch(sortedArray, target);

if (result !== -1) {
    console.log(`Target ${target} found at index ${result}`);
} else {
    console.log(`Target ${target} not found in the array`);
}
        "#;
        println!("Original Code:\n{}", code);
        let allocator = Allocator::default();
        let mut options = ParseOptions::default();
        options.allow_return_outside_function = true;
        let mut program = Parser::new(&allocator, code, SourceType::mjs()).with_options(options).parse().program;
        let ret = SemanticBuilder::new()
            // Estimate transformer will triple scopes, symbols, references
            .with_excess_capacity(2.0)
            .build(&program);
        let (symbols, scopes) = ret.semantic.into_symbol_table_and_scope_tree();
        let mut transformer = iris_ecma::EcmaLower::new(&allocator, Path::new("test.js"));
        let transformed = transformer.transformer.build_with_symbols_and_scopes(symbols, scopes, &mut program);
        // println!("{:#?}", program.body);
        let mut traverser = iris_ecma::EcmaGeneralization::new();
        traverser.build(&allocator, &mut program, transformed.symbols, transformed.scopes);
        // println!("{:#?}", traverser.ir);
        let ir = traverser.ir.unwrap();
        let mut chara = PythonCharacterization::new();
        chara.build(ir);
        // println!("{:#?}", chara.ast);
        let tokens = ruff::parser::parse("print('Hello, world!')", Mode::Module).unwrap();
        let stylist = Stylist::from_tokens(&tokens.tokens(), "print('Hello, world!')");
        let mut final_code = String::new();
        for stmt in chara.ast.unwrap().body.iter() {
            let mut generator = ruff::codegen::Generator::from(&stylist);
            final_code += generator.stmt(&stmt).as_str();
            final_code += "\n";
        }
        println!("Transformed Code:\n{}", final_code);
    }
}
