use std::{
    collections::{BTreeMap, HashMap, HashSet},
    marker::PhantomData,
};

use crate::{index::FSTIndex, Letter, State, FST};

#[derive(Debug, PartialEq)]
pub struct MutableFST<L: Letter> {
    slab: Vec<MutableState>,
    phantom: PhantomData<L>,
}

impl<L: Letter> std::ops::Index<FSTIndex> for MutableFST<L> {
    type Output = MutableState;

    fn index(&self, index: FSTIndex) -> &Self::Output {
        self.slab.index(index.0 as usize)
    }
}

impl<L: Letter> std::ops::IndexMut<FSTIndex> for MutableFST<L> {
    fn index_mut(&mut self, index: FSTIndex) -> &mut Self::Output {
        self.slab.index_mut(index.0 as usize)
    }
}

impl<C: Letter> Default for MutableFST<C> {
    fn default() -> Self {
        Self {
            slab: vec![MutableState::default()],
            phantom: PhantomData,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Default)]
pub struct MutableState {
    pub map: BTreeMap<u32, FSTIndex>,
    pub can_terminate: bool,
}

impl<L: Letter> State<L> for &MutableState {
    fn can_terminate(&self) -> bool {
        self.can_terminate
    }

    fn try_accept(&self, letter: &L) -> Option<FSTIndex> {
        self.map.get(&letter.to_u32()).copied()
    }

    fn try_first(&self) -> Option<(L, FSTIndex)> {
        self.map
            .first_key_value()
            .and_then(|(c, i)| L::try_from_u32(*c).map(|c| (c, *i)))
    }
}

impl<L: Letter> FST<L> for MutableFST<L> {
    type State<'s>
        = &'s MutableState
    where
        Self: 's;

    fn get_state(&self, index: FSTIndex) -> Self::State<'_> {
        self.slab.get(index.0 as usize).unwrap()
    }
}

impl<C: Letter> MutableFST<C> {
    /// Returns true if the word was added
    pub fn add_word(&mut self, iterator: impl IntoIterator<Item = C>) -> bool {
        let mut state_index: FSTIndex = FSTIndex(0);

        for c in iterator {
            let c = c.to_u32();
            match self[state_index].map.get(&c) {
                Some(a) => {
                    state_index = *a;
                }
                None => {
                    let new_state = MutableState::default();
                    let new_state_index: FSTIndex = self.slab.len().into();
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
    pub fn compress(self) -> MutableFST<C> {
        let MutableFST { mut slab, .. } = self;

        let mut replacements: HashMap<FSTIndex, FSTIndex> = Default::default();
        let mut removed: HashSet<FSTIndex> = Default::default();
        loop {
            let mut leaves: HashMap<&MutableState, FSTIndex> = Default::default();
            replacements.clear();

            for (index, state) in slab.iter().enumerate() {
                let index: FSTIndex = FSTIndex(index as u32); //index can't be zero here

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
            return MutableFST {
                slab,
                phantom: PhantomData,
            };
        }

        replacements.clear();
        let mut new_slab: Vec<MutableState> = Default::default();
        let mut next_index = FSTIndex(1);

        for (old_index, state) in slab.drain(..).enumerate() {
            if old_index == 0 {
                new_slab.push(state);
            } else {
                let old_index: FSTIndex = old_index.into();
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

        MutableFST {
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
    pub fn test_fst() {
        let mark = CharVec::from_str("Mark").unwrap();
        let mar = CharVec::from_str("Mar").unwrap();

        let mut wa = MutableFST::<Character>::default();

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

    fn make_planets() -> MutableFST<Character> {
        let mut wa: MutableFST<Character> = MutableFST::default();

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

        let v: Vec<_> = wa.iter::<CharacterJoiner>().map(|x| x.to_string()).collect();

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

        let v: Vec<_> = wa.iter::<CharacterJoiner>().map(|x| x.to_string()).collect();

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
