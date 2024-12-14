use crate::toolchain::{
    allocator::Allocator,
    transformer::{TransformOptions, Transformer},
    traverse::Traverse,
};
use std::path::Path;

/// Syntax lowering before transforming into IR.
///
pub struct Lower<'a> {
    options: TransformOptions,
    pub transformer: Transformer<'a>,
}

impl<'a> Lower<'a> {
    pub fn new(allocator: &'a Allocator, source_path: &Path) -> Self {
        let mut options = TransformOptions::from_target("es2015").unwrap();
        options.env.es2017.async_to_generator = false;
        options.env.es2018.async_generator_functions = false;
        Lower {
            transformer: Transformer::new(allocator, source_path, &options),
            options,
        }
    }
}

impl<'a> Traverse<'a> for Lower<'a> {}