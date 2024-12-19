use iris_low_level_ir::shared::Program;

#[derive(Debug, Clone)]
pub struct PythonGeneralization {
    pub ir: Option<Program>
}

impl PythonGeneralization {
    pub fn new() -> Self {
        PythonGeneralization {
            ir: None
        }
    }
}