mod characterization;
mod generalization;
mod lower;
mod resolve;

pub use generalization::EcmaGeneralization;
pub use lower::EcmaLower;

pub mod toolchain {
    pub use oxc::*;
    pub use oxc_ecmascript as utils;
    pub use oxc_index as index;
    pub use oxc_resolver as resolver;
    pub use oxc_traverse as traverse;
}
