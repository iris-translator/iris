#![cfg(feature = "test")]

use std::path::Path;

use iris_python::PythonCharacterization;
use oxc::allocator::Allocator;
use oxc::parser::{ParseOptions, Parser};
use oxc::semantic::SemanticBuilder;
use oxc::span::SourceType;
use ruff::codegen::{Stylist, round_trip};
use ruff::parser::Mode;

pub fn handle_ecma_to_python(alloc: &Allocator, origin: &str) -> Option<String> {
    let options = ParseOptions { allow_return_outside_function: true, ..Default::default() };
    let mut program = Parser::new(alloc, origin, SourceType::mjs()).with_options(options).parse();
    if program.panicked {
        panic!("Parser panicked. Please check your code.")
    }
    if program.errors.len() > 0 {
        println!("Parser errors: {:?}", program.errors);
        panic!("Parser errors. Please check your code.")
    }
    let ret = SemanticBuilder::new()
        // Estimate transformer will triple scopes, symbols, references
        .with_excess_capacity(2.0)
        .build(&program.program);
    let (symbols, scopes) = ret.semantic.into_symbol_table_and_scope_tree();
    let mut transformer = iris_ecma::EcmaLower::new(alloc, Path::new("test.js"));
    let transformed = transformer.transformer.build_with_symbols_and_scopes(
        symbols,
        scopes,
        &mut program.program,
    );
    let mut traverser = iris_ecma::EcmaGeneralization::new();
    traverser.build(alloc, &mut program.program, transformed.symbols, transformed.scopes);
    let ir = traverser.ir.unwrap();
    let mut characterization = PythonCharacterization::new();
    characterization.build(ir);
    let tokens = ruff::parser::parse("", Mode::Module).unwrap();
    let stylist = Stylist::from_tokens(&tokens.tokens(), "");
    let mut final_code = String::new();
    for stmt in characterization.ast.unwrap().body.iter() {
        let mut generator = ruff::codegen::Generator::from(&stylist);
        final_code += generator.stmt(&stmt).as_str();
        final_code += "\n";
    }
    Some(final_code)
}

pub fn handle_python_round(origin: &str) -> Option<String> {
    round_trip(origin).ok()
}

pub fn check_transformed_code(origin: &str, transformed: &str) {
    let origin = origin.trim();
    let transformed = transformed.trim();
    assert_eq!(
        handle_ecma_to_python(&Allocator::default(), origin).unwrap().trim(),
        handle_python_round(transformed).unwrap().trim()
    );
}

pub fn check_function_wrapped(origin: &str, transformed: &str) {
    let origin = "async function *test() {\n".to_string() + origin + "\n}";
    let transformed = "async def test():\n".to_string()
        + transformed
            .split("\n")
            .map(|line| "    ".to_string() + line)
            .collect::<Vec<String>>()
            .join("\n")
            .as_str()
        + "\n";
    // println!("{}", transformed);
    check_transformed_code(origin.as_str(), transformed.as_str());
}
