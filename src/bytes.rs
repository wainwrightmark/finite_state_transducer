use const_sized_bit_set::BitSet32;
use std::{iter::FusedIterator, marker::PhantomData};

use crate::{character::AutomataCharacter, slab_index::SlabIndex};

#[derive(Debug)]
pub struct BytesAutomata<'s, C: AutomataCharacter> {
    slice: &'s [u8],
    phantom: PhantomData<C>,
}

const CAN_TERMINATE_KEY: u32 = 31;

#[allow(dead_code)]
impl<'s, C: AutomataCharacter> BytesAutomata<'s, C> {
    pub const fn new(slice: &'s [u8]) -> Self {
        Self {
            slice,
            phantom: PhantomData,
        }
    }

    pub fn iter(&self) -> BytesAutomataIter<C> {
        BytesAutomataIter {
            automata: Self {
                slice: self.slice,
                phantom: self.phantom,
            },
            index_stack: vec![SlabIndex(0)],
            character_stack: vec![],
        }
    }

    pub fn contains(&self, iter: impl Iterator<Item = C>) -> bool {
        let mut current_key: SlabIndex = SlabIndex::ZERO;
        let mut set = self.get_set(current_key);

        for c in iter {
            let c = c.to_u32();
            if set.contains_const(c) {
                current_key = self.get_next_key(current_key, set.count_lesser_elements_const(c));
                set = self.get_set(current_key);
            } else {
                return false;
            }
        }
        set.contains_const(CAN_TERMINATE_KEY)
    }

    const fn get_set(&self, key: SlabIndex) -> BitSet32 {
        let index = (key.0 as usize) * 4;
        let integer = u32::from_le_bytes([
            self.slice[index],
            self.slice[index + 1],
            self.slice[index + 2],
            self.slice[index + 3],
        ]);

        BitSet32::from_inner_const(integer)
    }

    const fn get_next_key(&self, set_key: SlabIndex, element_index: u32) -> SlabIndex {
        let index = ((set_key.0 + 1 + element_index) as usize) * 4;
        let integer = u32::from_le_bytes([
            self.slice[index],
            self.slice[index + 1],
            self.slice[index + 2],
            self.slice[index + 3],
        ]);

        SlabIndex(integer)
    }
}

pub struct BytesAutomataIter<'a, C: AutomataCharacter> {
    automata: BytesAutomata<'a, C>,
    index_stack: Vec<SlabIndex>,
    character_stack: Vec<C>,
}

impl<'a, C: AutomataCharacter> FusedIterator for BytesAutomataIter<'a, C> {}

impl<'a, C: AutomataCharacter> Iterator for BytesAutomataIter<'a, C> {
    type Item = C::String;

    fn next(&mut self) -> Option<Self::Item> {
        fn increment_last<C: AutomataCharacter>(
            index_stack: &mut Vec<SlabIndex>,
            character_stack: &mut Vec<C>,
        ) {
            loop {
                match character_stack.last_mut() {
                    Some(other) => {
                        let next_index = other.to_u32() + 1;
                        if let Some(next) = C::try_from_u32(next_index) {
                            *other = next;
                            return;
                        //todo go to the next valid character instead
                        } else {
                            index_stack.pop();
                            character_stack.pop();
                        }
                    }
                    None => return,
                }
            }
        }

        loop {
            let top_state_index = *self.index_stack.last()?;

            let top_set = self.automata.get_set(top_state_index);
            //let top = &self.automata[top_state_index];

            if let Some(character) = self.character_stack.get(self.index_stack.len() - 1) {
                if top_set.contains_const(character.to_u32()) {
                    let nsi = self.automata.get_next_key(
                        top_state_index,
                        top_set.count_lesser_elements_const(character.to_u32()),
                    );
                    self.index_stack.push(nsi);
                } else {
                    increment_last(&mut self.index_stack, &mut self.character_stack);
                }
            } else {
                let result: Option<C::String> = if top_set.contains_const(CAN_TERMINATE_KEY){
                    let word = C::join(self.character_stack.iter());
                    Some(word)
                } else {
                    None
                };

                if let Some(next_char) = top_set.first_const().filter(|x| *x < CAN_TERMINATE_KEY) .and_then(|x| C::try_from_u32(x) ){
                    self.character_stack.push(next_char);
                    let next_index = self.automata.get_next_key(top_state_index, 0);
                    self.index_stack.push(next_index);
                }else{
                    self.index_stack.pop();
                    increment_last(&mut self.index_stack, &mut self.character_stack);
                }

                if result.is_some() {
                    return result;
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::str::FromStr;

    use super::BytesAutomata;
    use crate::mutable::tests::CharVec;
    use crate::mutable::tests::Character;

    #[test]
    fn test_contains() {
        let planets: BytesAutomata<'_, Character> = BytesAutomata::new(&PLANETS_BYTES);

        let pluto = CharVec::from_str("pluto").unwrap();
        let goofy = CharVec::from_str("goofy").unwrap();

        assert!(planets.contains(pluto.iter()));
        assert!(!planets.contains(goofy.iter()));
    }

    #[test]
    fn test_iter(){
        let planets: BytesAutomata<'_, Character> = BytesAutomata::new(&PLANETS_BYTES);

        let v: Vec<_> = planets.iter().collect();

        let joined = v.join(", ");

        assert_eq!(
            joined,
            "EARTH, MARS, NEPTUNE, PLUTO, RANDOM, SATURN, SOME, URANUS, VENUS, WORD"
        );
    }

    const PLANETS_BYTES: &'static [u8] = &[
        16, 176, 118, 0, 10, 0, 0, 0, 19, 0, 0, 0, 25, 0, 0, 0, 37, 0, 0, 0, 68, 0, 0, 0, 45, 0, 0,
        0, 56, 0, 0, 0, 64, 0, 0, 0, 78, 0, 0, 0, 1, 0, 0, 0, 12, 0, 0, 0, 0, 0, 2, 0, 14, 0, 0, 0,
        0, 0, 8, 0, 16, 0, 0, 0, 128, 0, 0, 0, 18, 0, 0, 0, 0, 0, 0, 128, 1, 0, 0, 0, 21, 0, 0, 0,
        0, 0, 2, 0, 23, 0, 0, 0, 0, 0, 4, 0, 18, 0, 0, 0, 16, 0, 0, 0, 27, 0, 0, 0, 0, 128, 0, 0,
        29, 0, 0, 0, 0, 0, 8, 0, 31, 0, 0, 0, 0, 0, 16, 0, 33, 0, 0, 0, 0, 32, 0, 0, 35, 0, 0, 0,
        16, 0, 0, 0, 18, 0, 0, 0, 0, 8, 0, 0, 39, 0, 0, 0, 0, 0, 16, 0, 41, 0, 0, 0, 0, 0, 8, 0,
        43, 0, 0, 0, 0, 64, 0, 0, 18, 0, 0, 0, 1, 64, 0, 0, 48, 0, 0, 0, 66, 0, 0, 0, 0, 0, 8, 0,
        50, 0, 0, 0, 0, 0, 16, 0, 52, 0, 0, 0, 0, 0, 2, 0, 54, 0, 0, 0, 0, 32, 0, 0, 18, 0, 0, 0,
        0, 0, 2, 0, 58, 0, 0, 0, 1, 0, 0, 0, 60, 0, 0, 0, 0, 32, 0, 0, 62, 0, 0, 0, 0, 0, 16, 0,
        23, 0, 0, 0, 16, 0, 0, 0, 60, 0, 0, 0, 0, 16, 0, 0, 35, 0, 0, 0, 1, 0, 0, 0, 70, 0, 0, 0,
        0, 32, 0, 0, 72, 0, 0, 0, 8, 0, 0, 0, 74, 0, 0, 0, 0, 64, 0, 0, 76, 0, 0, 0, 0, 16, 0, 0,
        18, 0, 0, 0, 0, 64, 0, 0, 80, 0, 0, 0, 0, 0, 2, 0, 82, 0, 0, 0, 8, 0, 0, 0, 18, 0, 0, 0,
    ];
}
