use std::{
    collections::{BTreeMap, HashMap, HashSet},
    iter::FusedIterator,
    marker::PhantomData,
};

use crate::{automata::Automata, character::*, slab_index::SlabIndex};

#[derive(Debug, PartialEq)]
pub struct MutableAutomata<C: AutomataCharacter> {
    slab: Vec<State>,
    phantom: PhantomData<C>,
}

impl<C: AutomataCharacter> std::ops::Index<SlabIndex> for MutableAutomata<C> {
    type Output = State;

    fn index(&self, index: SlabIndex) -> &Self::Output {
        self.slab.index(index.0 as usize)
    }
}

impl<C: AutomataCharacter> std::ops::IndexMut<SlabIndex> for MutableAutomata<C> {
    fn index_mut(&mut self, index: SlabIndex) -> &mut Self::Output {
        self.slab.index_mut(index.0 as usize)
    }
}

impl<C: AutomataCharacter> Default for MutableAutomata<C> {
    fn default() -> Self {
        Self {
            slab: vec![State::default()],
            phantom: PhantomData,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Default)]
pub struct State {
    pub map: BTreeMap<u32, SlabIndex>,
    pub can_terminate: bool,
}

#[derive(Debug)]
struct AutomataIterator<'a, C: AutomataCharacter> {
    automata: &'a MutableAutomata<C>,
    index_stack: Vec<SlabIndex>,
    character_stack: Vec<C>,
}

impl<C: AutomataCharacter> FusedIterator for AutomataIterator<'_, C> {}

impl<C: AutomataCharacter> Iterator for AutomataIterator<'_, C> {
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

            let top = &self.automata[top_state_index];

            if let Some(character) = self.character_stack.get(self.index_stack.len() - 1) {
                if let Some(nsi) = top.map.get(&character.to_u32()) {
                    self.index_stack.push(*nsi);
                } else {
                    increment_last(&mut self.index_stack, &mut self.character_stack);
                }
            } else {
                let result: Option<C::String> = if top.can_terminate {
                    let word = C::join(self.character_stack.iter());
                    Some(word)
                } else {
                    None
                };

                if let Some((next_char, next_index)) = top
                    .map
                    .first_key_value()
                    .and_then(|(c, i)| C::try_from_u32(*c).map(|c| (c, i)))
                {
                    self.character_stack.push(next_char);
                    self.index_stack.push(*next_index);
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

impl<C: AutomataCharacter> Automata<C> for MutableAutomata<C> {
    fn iter(&self) -> impl FusedIterator<Item = C::String> {
        AutomataIterator {
            automata: self,
            index_stack: vec![SlabIndex(0)],
            character_stack: vec![],
        }
    }

    fn contains(&self, iter: impl IntoIterator<Item = C>) -> bool {
        let mut state = self.slab.first().unwrap();

        for c in iter {
            match state.map.get(&c.to_u32()) {
                Some(a) => state = &self[*a],
                None => return false,
            }
        }
        state.can_terminate
    }
}

impl<C: AutomataCharacter> MutableAutomata<C> {
    /// Returns true if the word was added
    pub fn add_word(&mut self, iterator: impl IntoIterator<Item = C>) -> bool {
        let mut state_index: SlabIndex = SlabIndex(0);

        for c in iterator {
            let c = c.to_u32();
            match self[state_index].map.get(&c) {
                Some(a) => {
                    state_index = *a;
                }
                None => {
                    let new_state = State::default();
                    let new_state_index: SlabIndex = self.slab.len().into();
                    self.slab.push(new_state);

                    self[state_index].map.insert(c, new_state_index);
                    state_index = new_state_index;
                }
            }
        }

        let prev = self[state_index].can_terminate;
        self[state_index].can_terminate = true;
        !prev
    }

    #[must_use]
    pub fn compress(self) -> MutableAutomata<C> {
        let MutableAutomata { mut slab, .. } = self;

        let mut replacements: HashMap<SlabIndex, SlabIndex> = Default::default();
        let mut removed: HashSet<SlabIndex> = Default::default();
        loop {
            let mut leaves: HashMap<&State, SlabIndex> = Default::default();
            replacements.clear();

            for (index, state) in slab.iter().enumerate() {
                let index: SlabIndex = SlabIndex(index as u32); //index can't be zero here

                match leaves.entry(state) {
                    std::collections::hash_map::Entry::Occupied(occupied_entry) => {
                        replacements.insert(index, *occupied_entry.get());
                        removed.insert(index);
                    }
                    std::collections::hash_map::Entry::Vacant(vacant_entry) => {
                        vacant_entry.insert(index);
                    }
                }
            }

            if replacements.is_empty() {
                break;
            }
            let mut changed = false;
            for state in slab.iter_mut() {
                for old_index in state.map.values_mut() {
                    if let Some(new_index) = replacements.get(old_index) {
                        *old_index = *new_index;
                        changed = true;
                    }
                }
            }
            if !changed {
                break;
            }
        }

        if removed.is_empty() {
            return MutableAutomata {
                slab,
                phantom: PhantomData,
            };
        }

        replacements.clear();
        let mut new_slab: Vec<State> = Default::default();
        let mut next_index = SlabIndex(1);

        for (old_index, state) in slab.drain(..).enumerate() {
            if old_index == 0 {
                new_slab.push(state);
            } else {
                let old_index: SlabIndex = old_index.into();
                if removed.contains(&old_index) {
                    continue;
                }

                new_slab.push(state);
                if old_index != next_index {
                    replacements.insert(old_index, next_index);
                }
                next_index.increment();
            }
        }

        for state in new_slab.iter_mut() {
            for old_index in state.map.values_mut() {
                if let Some(new_index) = replacements.get(old_index) {
                    *old_index = *new_index;
                }
            }
        }

        MutableAutomata {
            slab: new_slab,
            phantom: PhantomData,
        }
    }

    pub fn freeze(&self) -> Vec<u8> {
        let mut bytes = vec![];
        let mut mappings: BTreeMap<u32, u32> = Default::default(); //todo replace with vecs
        let mut next_key = 0u32;

        // const SET_BYTES: u32 = 4;
        // const KEY_BYTES: u32 = 4;
        for (old_key, slab) in self.slab.iter().enumerate() {
            mappings.insert(old_key as u32, next_key);

            let size = 1 + (slab.map.len() as u32);
            next_key += size
        }

        for state in self.slab.iter() {
            let mut set = const_sized_bit_set::BitSet32::EMPTY;
            if state.can_terminate {
                set.insert_const(31);
            }
            for key in state.map.keys() {
                set.insert_const(*key);
            }
            bytes.extend_from_slice(&set.inner_const().to_le_bytes());

            for slab_index in state.map.values() {
                let mapped = *mappings.get(&slab_index.0).unwrap();
                bytes.extend_from_slice(&mapped.to_le_bytes());
            }
        }

        bytes
    }
}

#[cfg(test)]
mod tests {
    use crate::test_helpers::*;
    use std::str::FromStr;

    use super::*;
    #[test]
    pub fn test_word_automata() {
        let mark = CharVec::from_str("Mark").unwrap();
        let mar = CharVec::from_str("Mar").unwrap();

        let mut wa = MutableAutomata::<Character>::default();

        assert!(!wa.contains(mar.iter()));
        assert!(!wa.contains(mark.iter()));

        assert!(wa.add_word(mar.iter()));

        assert!(wa.contains(mar.iter()));
        assert!(!wa.contains(mark.iter()));

        assert!(wa.add_word(mark.iter()));
        assert!(
            !wa.add_word(mark.iter()),
            "Should return false when adding duplicate word"
        );

        assert!(wa.contains(mar.iter()));
        assert!(wa.contains(mark.iter()));
    }

    fn make_planets() -> MutableAutomata<Character> {
        let mut wa: MutableAutomata<Character> = MutableAutomata::default();

        for word in [
            "Earth", "Mars", "Neptune", "Pluto", "Saturn", "Uranus", "Venus", "Some", "Random",
            "Word",
        ] {
            let word = CharVec::from_str(word).unwrap();
            wa.add_word(word.iter());
        }
        wa
    }

    #[test]
    pub fn test_iter() {
        let wa = make_planets();

        let v: Vec<_> = wa.iter().map(|x| x.to_string()).collect();

        let joined = v.join(", ");

        assert_eq!(
            joined,
            "EARTH, MARS, NEPTUNE, PLUTO, RANDOM, SATURN, SOME, URANUS, VENUS, WORD"
        );
    }

    #[test]
    pub fn test_compress_then_iter() {
        let wa = make_planets();

        assert_eq!(wa.slab.len(), 52);
        let wa = wa.compress();
        assert_eq!(wa.slab.len(), 38);

        let v: Vec<_> = wa.iter().map(|x| x.to_string()).collect();

        let joined = v.join(", ");

        assert_eq!(
            joined,
            "EARTH, MARS, NEPTUNE, PLUTO, RANDOM, SATURN, SOME, URANUS, VENUS, WORD"
        );
    }

    #[test]
    pub fn test_to_bytes() {
        let wa = make_planets();
        let wa = wa.compress();
        let bytes = wa.freeze();

        assert_eq!(bytes, PLANETS_BYTES)
    }
}
