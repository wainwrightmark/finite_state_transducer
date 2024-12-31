use const_sized_bit_set::BitSet32;
use std::marker::PhantomData;

use crate::{index::FSTIndex, Letter, State, FST};

#[derive(Debug)]
pub struct FrozenFST<'s, L: Letter> {
    slice: &'s [u8],
    phantom: PhantomData<L>,
}

#[derive(Debug)]
pub struct FrozenState<'s, L: Letter> {
    index: FSTIndex,
    slice: &'s [u8],
    phantom: PhantomData<L>,
}

impl<L: Letter> FrozenState<'_, L> {
    const CAN_TERMINATE_KEY: u32 = 31;

    const fn get_set(&self) -> BitSet32 {
        let index = (self.index.0 as usize) * 4;
        let integer = u32::from_le_bytes([
            self.slice[index],
            self.slice[index + 1],
            self.slice[index + 2],
            self.slice[index + 3],
        ]);

        BitSet32::from_inner_const(integer)
    }

    const fn get_next_key(&self, set_key: FSTIndex, element_index: u32) -> FSTIndex {
        let index = ((set_key.0 + 1 + element_index) as usize) * 4;
        let integer = u32::from_le_bytes([
            self.slice[index],
            self.slice[index + 1],
            self.slice[index + 2],
            self.slice[index + 3],
        ]);

        FSTIndex(integer)
    }
}

impl<L: Letter> State<L> for FrozenState<'_, L> {
    fn can_terminate(&self) -> bool {
        self.get_set().contains_const(Self::CAN_TERMINATE_KEY)
    }

    fn try_accept(&self, letter: &L) -> Option<FSTIndex> {
        let set = self.get_set();
        if set.contains_const(letter.to_u32()) {
            let offset = set.count_lesser_elements_const(letter.to_u32());

            Some(self.get_next_key(self.index, offset))
        } else {
            None
        }
    }

    fn try_first(&self) -> Option<(L, FSTIndex)> {
        self.get_set()
            .first_const()
            .filter(|x| *x < Self::CAN_TERMINATE_KEY)
            .and_then(|x| L::try_from_u32(x))
            .map(|x| (x, self.get_next_key(self.index, 0)))
    }
}

impl<L: Letter> FST<L> for FrozenFST<'_, L> {
    type State<'state>
        = FrozenState<'state, L>
    where
        Self: 'state;

    fn get_state(&self, index: FSTIndex) -> Self::State<'_> {
        FrozenState {
            index,
            slice: self.slice,
            phantom: PhantomData,
        }
    }
}

#[allow(dead_code)]
impl<'s, C: Letter> FrozenFST<'s, C> {
    pub const fn new(slice: &'s [u8]) -> Self {
        Self {
            slice,
            phantom: PhantomData,
        }
    }
}

#[cfg(test)]
mod tests {
    use std::str::FromStr;

    use super::FrozenFST;
    use crate::test_helpers::CharVec;
    use crate::test_helpers::Character;
    use crate::FST;

    #[test]
    fn test_contains() {
        let planets: FrozenFST<'_, Character> = FrozenFST::new(crate::test_helpers::PLANETS_BYTES);

        let pluto = CharVec::from_str("pluto").unwrap();
        let goofy = CharVec::from_str("goofy").unwrap();

        assert!(planets.contains(pluto.iter()));
        assert!(!planets.contains(goofy.iter()));
    }

    #[test]
    fn test_iter() {
        let planets: FrozenFST<'_, Character> = FrozenFST::new(crate::test_helpers::PLANETS_BYTES);

        let v: Vec<_> = planets.iter().map(|x| x.to_string()).collect();

        let joined = v.join(", ");

        assert_eq!(
            joined,
            "EARTH, MARS, NEPTUNE, PLUTO, RANDOM, SATURN, SOME, URANUS, VENUS, WORD"
        );
    }
}
