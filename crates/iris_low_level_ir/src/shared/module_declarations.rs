use either::Either;

use crate::shared::{Expression, Identifier, Span, Statement, StringLiteral, StringLiteralPrefix};

#[derive(Debug, Clone)]
pub struct ImportDeclaration {
    pub span: Span,
    pub specifiers: Option<Vec<ImportDeclarationSpecifier>>,
    pub source: ImportExportSource,
}

#[derive(Debug, Clone)]
pub enum ImportDeclarationSpecifier {
    ImportSpecifier(Box<ImportSpecifier>),
    ImportDefaultSpecifier(Box<ImportDefaultSpecifier>),
    ImportNamespaceSpecifier(Box<ImportNamespaceSpecifier>),
}

#[derive(Debug, Clone)]
pub struct ImportSpecifier {
    pub span: Span,
    pub imported: Expression,
    pub local: Identifier,
}

#[derive(Debug, Clone)]
pub struct ImportDefaultSpecifier {
    pub span: Span,
    pub local: Identifier,
}

#[derive(Debug, Clone)]
pub struct ImportNamespaceSpecifier {
    pub span: Span,
    pub local: Identifier,
}

#[derive(Debug, Clone)]
pub struct ExportNamedDeclaration {
    pub span: Span,
    // For the sake of simplicity, the `statement` contains the entire declaration.
    pub declaration: Option<Statement>,
    pub specifiers: Vec<ExportSpecifier>,
    pub source: Option<ImportExportSource>,
}

#[derive(Debug, Clone)]
pub struct ImportExportSource {
    pub span: Span,
    pub source: StringLiteral,
    pub cooked: Vec<CookedImportExportSourcePart>,
    pub r#type: ImportExportType,
    pub resolved: ImportExportResolvedType,
}

#[derive(Debug, Clone)]
pub enum CookedImportExportSourcePart {
    Super,
    Directory(String),
    Module(String),
    File { name: String, extension: String },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ImportExportType {
    Ecma,
    Python,
    Rust,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub enum ImportExportResolvedType {
    /// The `inline` is module inlined in the source code.
    /// For example, in Rust, the `inline` is
    /// ```rust
    /// mod foo {
    ///     pub fn bar() {}
    /// }
    /// ```
    Inline,
    /// The `file` is a file path.
    /// In Python, it can be relative or absolute path (relate to the cwd).
    /// In ECMAScript, it is a file path string.
    File,
    /// The `module` is a module name.
    Module,
    /// The `remote` is imported from url, especially in early version of Deno and Golang.
    /// To make it clarified, modules that refer to git repository is not `remote`. It should
    /// be considered as `module`.
    Remote,
    #[default]
    Unknown,
}

impl ImportExportSource {
    /// Ecma like, which is path instead of module specifier.
    /// For now, for parsing
    /// ```javascript
    /// export { foo } from './foo.js';
    /// ```
    /// We can use `ImportExportSource` to represent the source.
    /// It should be parsed into [Super, File { name: "foo", extension: "js" }]
    pub fn from_ecma(path: String) -> ImportExportSource {
        let mut parts = vec![];
        if path.starts_with("..") {
            parts.push(CookedImportExportSourcePart::Super);
        }
        let mut dir_parts = path.split('/').collect::<Vec<_>>();
        if path.starts_with(".") {
            parts.push(CookedImportExportSourcePart::Directory(dir_parts.remove(0).to_string()))
        }
        for part in dir_parts {
            if part == ".." {
                parts.push(CookedImportExportSourcePart::Super);
            } else {
                parts.push(CookedImportExportSourcePart::Directory(part.to_string()));
            }
        }
        ImportExportSource {
            span: Span { start: 0, end: 0 },
            source: StringLiteral {
                span: Span { start: 0, end: 0 },
                value: path,
                prefix: StringLiteralPrefix::Empty,
            },
            cooked: parts,
            r#type: ImportExportType::Ecma,
            resolved: ImportExportResolvedType::File,
        }
    }

    pub fn as_python(&self) -> (Identifier, u32) {
        let level = self
            .cooked
            .iter()
            .filter(|part| matches!(part, CookedImportExportSourcePart::Super))
            .count();
        let mut module = String::new();
        for part in &self.cooked {
            match part {
                CookedImportExportSourcePart::Super => {}
                CookedImportExportSourcePart::Directory(dir) => {
                    module.push_str(dir);
                    module.push_str(".");
                }
                CookedImportExportSourcePart::Module(name) => {
                    module.push_str(name);
                }
                CookedImportExportSourcePart::File { name, extension } => {
                    module.push_str(name);
                    module.push_str(".");
                    module.push_str(extension);
                }
            }
        }
        (
            Identifier {
                span: Span { start: 0, end: 0 },
                name: module,
                symbol_id: None,
                private: false,
            },
            level as u32,
        )
    }
}

#[derive(Debug, Clone)]
pub struct ExportSpecifier {
    pub span: Span,
    pub exported: Expression,
    pub local: Expression,
}

#[derive(Debug, Clone)]
pub struct ExportDefaultDeclaration {
    pub span: Span,
    // Here, the statement represents FunctionDeclaration and ClassDeclaration.
    pub declaration: Box<Either<Expression, Statement>>,
    pub local: Expression,
}

#[derive(Debug, Clone)]
pub struct ExportAllDeclaration {
    pub span: Span,
    pub exported: Option<Expression>,
    pub source: ImportExportSource,
}
