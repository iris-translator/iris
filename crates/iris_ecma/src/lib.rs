mod characterization;
mod generalization;
mod lower;
mod resolve;

pub use characterization::EcmaCharacterization;
pub use generalization::EcmaGeneralization;
pub use lower::EcmaLower;

pub mod toolchain {
    pub use oxc::*;
    pub use {
        oxc_ecmascript as utils, oxc_index as index, oxc_resolver as resolver,
        oxc_traverse as traverse,
    };
}
