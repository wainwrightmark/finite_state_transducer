use std::{
    collections::{BTreeMap, HashMap, HashSet},
    iter::FusedIterator,
    marker::PhantomData,
};

use crate::character::*;

#[derive(Debug, PartialEq)]
pub struct WordAutomata<C: AutomataCharacter> {
    slab: Vec<State>,
    phantom: PhantomData<C>,
}

impl<C: AutomataCharacter> std::ops::Index<SlabIndex> for WordAutomata<C> {
    type Output = State;

    fn index(&self, index: SlabIndex) -> &Self::Output {
        &self.slab.index(index.0 as usize)
    }
}

impl<C: AutomataCharacter> std::ops::IndexMut<SlabIndex> for WordAutomata<C> {
    fn index_mut(&mut self, index: SlabIndex) -> &mut Self::Output {
        self.slab.index_mut(index.0 as usize)
    }
}

impl<C: AutomataCharacter> Default for WordAutomata<C> {
    fn default() -> Self {
        Self {
            slab: vec![State::default()],
            phantom: PhantomData,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct State {
    pub map: BTreeMap<u32, SlabIndex>,
    pub can_terminate: bool,
}

impl Default for State {
    fn default() -> Self {
        Self {
            map: Default::default(),
            can_terminate: Default::default(),
        }
    }
}

pub struct AutomataIterator<'a, C: AutomataCharacter> {
    automata: &'a WordAutomata<C>,
    index_stack: Vec<SlabIndex>,
    character_stack: Vec<C>,
}

impl<'a, C: AutomataCharacter> FusedIterator for AutomataIterator<'a, C> {}

impl<'a, C: AutomataCharacter> Iterator for AutomataIterator<'a, C> {
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
                    self.index_stack.push((*nsi).into());
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

#[allow(dead_code)]
impl<C: AutomataCharacter> WordAutomata<C> {
    pub fn iter(&self) -> AutomataIterator<C> {
        AutomataIterator {
            automata: self,
            index_stack: vec![SlabIndex(0)],
            character_stack: vec![],
        }
    }

    pub fn contains(&self, iter: impl Iterator<Item = C>) -> bool {
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

impl<C: AutomataCharacter> WordAutomata<C> {
    /// Returns true if the word was added
    pub fn add_word(&mut self, iterator: impl Iterator<Item = C>) -> bool {
        let mut state_index: SlabIndex = SlabIndex(0);

        for c in iterator {
            let c = c.to_u32();
            match self[state_index].map.get(&c) {
                Some(a) => {
                    state_index = (*a).into();
                }
                None => {
                    let new_state = State::default();
                    let new_state_index: SlabIndex = self.slab.len().into();
                    self.slab.push(new_state);

                    self[state_index].map.insert(c, new_state_index);
                    state_index = new_state_index.into();
                }
            }
        }

        let prev = self[state_index].can_terminate;
        self[state_index].can_terminate = true;
        !prev
    }

    #[must_use]
    pub fn compress(self) -> WordAutomata<C> {
        let WordAutomata { mut slab, .. } = self;

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
            return WordAutomata {
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

        return WordAutomata {
            slab: new_slab,
            phantom: PhantomData,
        };
    }


    // fn to_bytes()-> Vec<u8>{
    //     let mut bytes = vec![];

    //     bytes

    // }

}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SlabIndex(pub u32);

impl From<usize> for SlabIndex {
    fn from(value: usize) -> Self {
        Self(value as u32)
    }
}

impl SlabIndex {
    pub fn increment(&mut self) {
        self.0 += 1;
    }
}

#[cfg(test)]
mod tests {
    use std::{
        convert::Infallible,
        fmt::{Display, Write},
        str::FromStr,
    };

    use super::*;
    #[test]
    pub fn test_word_automata() {
        let mark = CharVec::from_str("Mark").unwrap();
        let mar = CharVec::from_str("Mar").unwrap();

        let mut wa = WordAutomata::<Character>::default();

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

    #[test]
    pub fn test_iter() {
        let mut wa: WordAutomata<Character> = WordAutomata::default();

        for word in [
            "Earth", "Mars", "Neptune", "Pluto", "Saturn", "Uranus", "Venus", "Some", "Random",
            "Word",
        ] {
            let word = CharVec::from_str(word).unwrap();
            wa.add_word(word.iter());
        }

        //let wa = wa.compress();

        let v: Vec<_> = wa.iter().collect();

        let joined = v.join(", ");

        assert_eq!(
            joined,
            "EARTH, MARS, NEPTUNE, PLUTO, RANDOM, SATURN, SOME, URANUS, VENUS, WORD"
        );
    }

    #[test]
    pub fn test_compress_then_iter() {
        let mut wa: WordAutomata<Character> = WordAutomata::default();

        for word in [
            "Earth", "Mars", "Neptune", "Pluto", "Saturn", "Uranus", "Venus", "Some", "Random",
            "Word",
        ] {
            let word = CharVec::from_str(word).unwrap();
            wa.add_word(word.iter());
        }

        assert_eq!(wa.slab.len(),52);

        let wa = wa.compress();

        assert_eq!(wa.slab.len(),38);

        let v: Vec<_> = wa.iter().collect();

        let joined = v.join(", ");

        assert_eq!(
            joined,
            "EARTH, MARS, NEPTUNE, PLUTO, RANDOM, SATURN, SOME, URANUS, VENUS, WORD"
        );
    }

    // #[test]
    // pub fn test_on_grid() {
    //     let mut wa = WordAutomata::default();

    //     for word in [
    //         "Earth", "Mars", "Neptune", "Pluto", "Saturn", "Uranus", "Venus", "Some", "Random",
    //         "Word",
    //     ] {
    //         let word = RawWord::from_str(word).unwrap();
    //         wa.add_word(&word);
    //     }

    //     assert_eq!(wa.slab.len(), 52);
    //     //println!("Uncompressed - {} states", wa.slab.len());
    //     let wa = wa.compress();
    //     //println!("Compressed - {} states", wa.slab.len());
    //     assert_eq!(wa.slab.len(), 38);

    //     let grid = try_make_grid("VENMOUAULTRSHPEN").unwrap();

    //     let grid_words = wa.find_all_words(&grid);

    //     let found_words = grid_words
    //         .iter()
    //         .map(|x| x.iter().map(|c| c.as_char()).join(""))
    //         .join(", ");

    //     assert_eq!(
    //         found_words,
    //         "EARTH, MARS, NEPTUNE, PLUTO, SATURN, URANUS, VENUS", //Should be in alphabetical order
    //     )
    // }

    pub struct CharVec(Vec<Character>);

    impl CharVec {
        pub fn iter<'a>(&'a self) -> impl Iterator<Item = Character> + 'a {
            self.0.iter().cloned()
        }
    }

    impl FromStr for CharVec {
        type Err = Infallible;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            let mut v = vec![];

            for c in s.chars() {
                if let Some(char) = Character::try_from_char(c) {
                    v.push(char);
                }
            }

            Ok(Self(v))
        }
    }

    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]

    pub enum Character {
        A,
        B,
        C,
        D,
        E,
        F,
        G,
        H,
        I,
        J,
        K,
        L,
        M,
        N,
        O,
        P,
        Q,
        R,
        S,
        T,
        U,
        V,
        W,
        X,
        Y,
        Z,
    }

    impl Display for Character {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            f.write_char(self.as_char())?;

            Ok(())
        }
    }

    impl Character {
        pub fn as_char(&self) -> char {
            (('A' as u8) + (self.clone() as u8)) as char
        }

        pub fn try_from_char(c: char) -> Option<Self> {
            let c = c as u8;

            for offset in ['A', 'a'] {
                match c.checked_sub(offset as u8) {
                    Some(x) => {
                        if let Some(c) = AutomataCharacter::try_from_u32(x as u32) {
                            return Some(c);
                        }
                    }
                    None => {}
                }
            }

            return None;
        }
    }

    impl crate::character::AutomataCharacter for Character {
        type String = String;

        fn to_u32(&self) -> u32 {
            *self as u32
        }

        fn try_from_u32(index: u32) -> Option<Self> {
            use Character::*;
            const ARRAY: [Character; 26] = [
                A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z,
            ];

            ARRAY.get(index as usize).cloned()
        }

        fn join<'a>(items: impl Iterator<Item = &'a Self>) -> Self::String {
            let mut r = String::with_capacity(items.size_hint().0);

            for item in items {
                r.push(item.as_char());
            }

            r
        }
    }
}
