mod definitions;
mod magics;
mod generator;

pub use definitions::{Flag, Move, MoveList, ScoredMove, ScoredMoveList};
#[allow(unused_imports)]
pub use definitions::MAX_MOVES;
pub use generator::generate_legal_moves;
#[allow(unused_imports)]
pub use generator::PinInfo;
pub use magics::{magics, Magics, MAGICS};
#[allow(unused_imports)]
pub use magics::MagicTable;
