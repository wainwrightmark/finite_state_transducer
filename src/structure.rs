use crate::{
    frozen::{self, FrozenAutomata},
    character::AutomataCharacter,
    mutable::MutableAutomata,
    slab_index::SlabIndex,
};

pub trait Structure {
    type String: Ord;
    type Character: AutomataCharacter<String = Self::String> + Clone;

    type Index;
    type IndexSet: Default;

    fn try_get_character(&self, index: &Self::Index) -> Option<Self::Character>;

    fn set_insert(set: &Self::IndexSet, index: &Self::Index) -> Self::IndexSet;

    fn set_contains(set: &Self::IndexSet, index: &Self::Index) -> bool;

    fn iter_adjacent_unused(
        index: &Self::Index,
        used_tiles: &Self::IndexSet,
    ) -> impl Iterator<Item = Self::Index>;

    fn iter_indices() -> impl Iterator<Item = Self::Index>;

    fn find_words_inner(
        wa: &MutableAutomata<Self::Character>,
        results: &mut Vec<<Self::Character as AutomataCharacter>::String>,
        current_index: SlabIndex,
        grid: &Self,
        new_tile: Self::Index,
        used_tiles: &Self::IndexSet,
        previous_chars: &Vec<Self::Character>,
    ) {
        let Some(character) = grid.try_get_character(&new_tile) else {
            return;
        };

        if let Some(next_index) = wa[current_index].map.get(&character.to_u32()) {
            let state = &wa[*next_index];
            let mut next_chars = previous_chars.clone();
            next_chars.push(character);

            let next_used_tiles = Self::set_insert(used_tiles, &new_tile);

            if state.can_terminate {
                let string = Self::Character::join(next_chars.iter());
                results.push(string);
            }

            for tile in Self::iter_adjacent_unused(&new_tile, used_tiles) {
                Self::find_words_inner(
                    wa,
                    results,
                    (*next_index).into(),
                    grid,
                    tile,
                    &next_used_tiles,
                    &next_chars,
                );
            }
        }
    }

    fn find_all_words(
        &self,
        wa: &MutableAutomata<Self::Character>,
    ) -> Vec<<Self::Character as AutomataCharacter>::String> {
        let mut result: Vec<<Self::Character as AutomataCharacter>::String> = vec![];
        let empty_used_tiles = Self::IndexSet::default();
        for tile in Self::iter_indices() {
            Self::find_words_inner(
                wa,
                &mut result,
                SlabIndex(0),
                self,
                tile,
                &empty_used_tiles,
                &Vec::new(),
            )
        }

        result.sort();
        result.dedup();

        result
    }

    fn find_words_inner_frozen(
        wa: &FrozenAutomata<Self::Character>,
        results: &mut Vec<<Self::Character as AutomataCharacter>::String>,
        current_index: SlabIndex,
        structure: &Self,
        new_tile: Self::Index,
        used_tiles: &Self::IndexSet,
        previous_chars: &Vec<Self::Character>,
    ) {
        let Some(character) = structure.try_get_character(&new_tile) else {
            return;
        };

        let set = wa.get_set(current_index);
        let c = character.to_u32();

        if !set.contains_const(c) {
            return;
        }

        let next_index = wa.get_next_key(current_index, set.count_lesser_elements_const(c));
        let mut next_chars = previous_chars.clone();
        next_chars.push(character);

        let next_set = wa.get_set(next_index);

        if next_set.contains_const(frozen::CAN_TERMINATE_KEY) {
            let string = Self::Character::join(next_chars.iter());
            results.push(string);
        }

        let next_used_tiles = Self::set_insert(used_tiles, &new_tile);

        for tile in Self::iter_adjacent_unused(&new_tile, used_tiles) {
            Self::find_words_inner_frozen(
                wa,
                results,
                next_index,
                structure,
                tile,
                &next_used_tiles,
                &next_chars,
            );
        }
    }

    fn find_all_words_frozen(
        &self,
        wa: &FrozenAutomata<Self::Character>,
    ) -> Vec<<Self::Character as AutomataCharacter>::String> {
        let mut result: Vec<<Self::Character as AutomataCharacter>::String> = vec![];
        let empty_used_tiles = Self::IndexSet::default();
        for tile in Self::iter_indices() {
            Self::find_words_inner_frozen(
                wa,
                &mut result,
                SlabIndex(0),
                self,
                tile,
                &empty_used_tiles,
                &Vec::new(),
            )
        }

        result.sort();
        result.dedup();

        result
    }
}

#[cfg(test)]
pub mod tests {
    use super::Structure;
    use crate::{frozen::FrozenAutomata, test_helpers::*};

    #[test]
    pub fn test_structure() {
        use std::str::FromStr;

        use crate::mutable::MutableAutomata;

        let structure = GridStructure([
            Character::V,
            Character::E,
            Character::N,
            Character::M,
            Character::O,
            Character::U,
            Character::A,
            Character::U,
            Character::L,
            Character::T,
            Character::R,
            Character::S,
            Character::H,
            Character::P,
            Character::E,
            Character::N,
        ]);

        let mut wa: MutableAutomata<Character> = MutableAutomata::default();

        for word in [
            "Earth", "Mars", "Neptune", "Pluto", "Saturn", "Uranus", "Venus", "Some", "Random",
            "Word",
        ] {
            let word = CharVec::from_str(word).unwrap();
            wa.add_word(word.iter());
        }

        let words = structure.find_all_words(&mut wa);

        let found_words: Vec<_> = words.iter().map(|x| x.to_string()).collect();

        assert_eq!(
            found_words,
            //Should be in alphabetical order
            vec!["EARTH", "MARS", "NEPTUNE", "PLUTO", "SATURN", "URANUS", "VENUS"],
        )
    }

    #[test]
    pub fn test_frozen_structure() {
        let structure = GridStructure([
            Character::V,
            Character::E,
            Character::N,
            Character::M,
            Character::O,
            Character::U,
            Character::A,
            Character::U,
            Character::L,
            Character::T,
            Character::R,
            Character::S,
            Character::H,
            Character::P,
            Character::E,
            Character::N,
        ]);

        let wa: FrozenAutomata<'_, Character> = FrozenAutomata::new(&PLANETS_BYTES);

        let words = structure.find_all_words_frozen(&wa);

        let found_words: Vec<_> = words.iter().map(|x| x.to_string()).collect();

        assert_eq!(
            found_words,
            //Should be in alphabetical order
            vec!["EARTH", "MARS", "NEPTUNE", "PLUTO", "SATURN", "URANUS", "VENUS"],
        )
    }

    #[derive(Clone)]
    pub struct StructureIndex(u8);

    impl std::fmt::Display for StructureIndex {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{}", self.0)
        }
    }

    impl StructureIndex {
        pub fn x(&self) -> u8 {
            self.0 % 4
        }

        pub fn y(&self) -> u8 {
            self.0 / 4
        }
    }

    #[derive(Default)]
    pub struct StructureIndexSet(u16);

    impl std::fmt::Debug for StructureIndexSet {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{:016b}", self.0)
        }
    }

    struct GridStructure([Character; 16]);

    impl Structure for GridStructure {
        type String = CharVec;

        type Character = Character;

        type Index = StructureIndex;

        type IndexSet = StructureIndexSet;

        fn try_get_character(&self, index: &Self::Index) -> Option<Self::Character> {
            Some(self.0[index.0 as usize])
        }

        fn set_insert(set: &Self::IndexSet, index: &Self::Index) -> Self::IndexSet {
            let mask = 1 << index.0 as u32;
            StructureIndexSet(set.0 | mask)
        }

        fn set_contains(set: &Self::IndexSet, index: &Self::Index) -> bool {
            (set.0 >> (index.0 as u32)) & 1 == 1
        }

        fn iter_adjacent_unused(
            index: &Self::Index,
            used_tiles: &Self::IndexSet,
        ) -> impl Iterator<Item = Self::Index> {
            let x = index.x();
            let y = index.y();

            Self::iter_indices()
                .filter(move |index| index.x().abs_diff(x) <= 1 && index.y().abs_diff(y) <= 1)
                .filter(|index| !Self::set_contains(used_tiles, index))
        }

        fn iter_indices() -> impl Iterator<Item = Self::Index> {
            (0..16).map(|x| StructureIndex(x))
        }
    }
}
