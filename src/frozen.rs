use const_sized_bit_set::BitSet32;
use std::{iter::FusedIterator, marker::PhantomData};

use crate::{character::AutomataCharacter, slab_index::SlabIndex};

#[derive(Debug)]
pub struct FrozenAutomata<'s, C: AutomataCharacter> {
    slice: &'s [u8],
    phantom: PhantomData<C>,
}

pub const CAN_TERMINATE_KEY: u32 = 31;

#[allow(dead_code)]
impl<'s, C: AutomataCharacter> FrozenAutomata<'s, C> {
    pub const fn new(slice: &'s [u8]) -> Self {
        Self {
            slice,
            phantom: PhantomData,
        }
    }

    pub fn iter(&self) -> impl FusedIterator<Item = C::String> + use<'_, C> {
        AutomataIter {
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

    pub(crate) const fn get_set(&self, key: SlabIndex) -> BitSet32 {
        let index = (key.0 as usize) * 4;
        let integer = u32::from_le_bytes([
            self.slice[index],
            self.slice[index + 1],
            self.slice[index + 2],
            self.slice[index + 3],
        ]);

        BitSet32::from_inner_const(integer)
    }

    pub(crate) const fn get_next_key(&self, set_key: SlabIndex, element_index: u32) -> SlabIndex {
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

struct AutomataIter<'a, C: AutomataCharacter> {
    automata: FrozenAutomata<'a, C>,
    index_stack: Vec<SlabIndex>,
    character_stack: Vec<C>,
}

impl<C: AutomataCharacter> FusedIterator for AutomataIter<'_, C> {}

impl<C: AutomataCharacter> Iterator for AutomataIter<'_, C> {
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
                let result: Option<C::String> = if top_set.contains_const(CAN_TERMINATE_KEY) {
                    let word = C::join(self.character_stack.iter());
                    Some(word)
                } else {
                    None
                };

                if let Some(next_char) = top_set
                    .first_const()
                    .filter(|x| *x < CAN_TERMINATE_KEY)
                    .and_then(|x| C::try_from_u32(x))
                {
                    self.character_stack.push(next_char);
                    let next_index = self.automata.get_next_key(top_state_index, 0);
                    self.index_stack.push(next_index);
                } else {
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

    use super::FrozenAutomata;
    use crate::test_helpers::CharVec;
    use crate::test_helpers::Character;

    #[test]
    fn test_contains() {
        let planets: FrozenAutomata<'_, Character> =
            FrozenAutomata::new(crate::test_helpers::PLANETS_BYTES);

        let pluto = CharVec::from_str("pluto").unwrap();
        let goofy = CharVec::from_str("goofy").unwrap();

        assert!(planets.contains(pluto.iter()));
        assert!(!planets.contains(goofy.iter()));
    }

    #[test]
    fn test_iter() {
        let planets: FrozenAutomata<'_, Character> =
            FrozenAutomata::new(crate::test_helpers::PLANETS_BYTES);

        let v: Vec<_> = planets.iter().map(|x| x.to_string()).collect();

        let joined = v.join(", ");

        assert_eq!(
            joined,
            "EARTH, MARS, NEPTUNE, PLUTO, RANDOM, SATURN, SOME, URANUS, VENUS, WORD"
        );
    }
}
