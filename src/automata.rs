use std::iter::FusedIterator;
use crate::character::AutomataCharacter;

pub trait Automata<C: AutomataCharacter>{
    fn iter<'a> (&'a self) -> impl FusedIterator<Item = C::String>;

    fn contains<'a>(&'a self, iter: impl IntoIterator<Item = C>) -> bool;
}