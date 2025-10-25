//pub fn traverse_mutable()

use crate::Letter;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TraversalItem<L: Letter> {
    //Go up one level, or stop if this is the top level
    Backtrack,
    Letter { letter: L, can_be_end_of_word: bool },
}
