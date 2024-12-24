use std::marker::PhantomData;
use crate::character::AutomataCharacter;

#[derive(Debug)]
pub struct BytesAutomata<'s>{
    slice: &'s [u32],
}