pub mod frozen;
pub mod index;
pub mod iterator;
pub mod mutable;
pub mod either_fst;

use index::FSTIndex;
use iterator::FSTIterator;
use std::iter::FusedIterator;

pub trait Letter: Sized {
    type String;

    fn try_from_u32(key: u32) -> Option<Self>;
    fn to_u32(&self) -> u32;

    fn join<'a>(items: impl Iterator<Item = &'a Self>) -> Self::String
    where
        Self: 'a;
}

pub trait FST<L: Letter>: Sized {
    type State<'s>: State<L>
    where
        Self: 's;

    fn iter(&self) -> impl FusedIterator<Item = L::String> {
        FSTIterator::new(self)
    }
    fn contains(&self, iter: impl IntoIterator<Item = L>) -> bool {
        let mut state = self.get_state(FSTIndex::ZERO);

        for l in iter {
            if let Some(next_key) = state.try_accept(&l) {
                state = self.get_state(next_key);
            } else {
                return false;
            }
        }
        state.can_terminate()
    }

    fn get_state(&self, index: FSTIndex) -> Self::State<'_>;
}

pub trait State<L: Letter> {
    fn can_terminate(&self) -> bool;
    fn try_accept(&self, letter: &L) -> Option<FSTIndex>;
    fn try_first(&self) -> Option<(L, FSTIndex)>;
}

#[cfg(test)]
pub(crate) mod test_helpers {
    use crate::Letter;
    use std::{fmt::Write, str::FromStr};

    #[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
    pub struct CharVec(Vec<Character>);

    impl CharVec {
        pub fn iter(&self) -> impl Iterator<Item = Character> + '_ {
            self.0.iter().cloned()
        }
    }

    impl std::fmt::Display for CharVec {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            for x in self.iter() {
                f.write_char(x.as_char())?;
            }

            Ok(())
        }
    }

    impl FromStr for CharVec {
        type Err = std::convert::Infallible;

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

    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]

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

    impl std::fmt::Display for Character {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            std::fmt::Write::write_char(f, self.as_char())?;

            Ok(())
        }
    }

    impl Character {
        pub fn as_char(&self) -> char {
            (b'A' + (*self as u8)) as char
        }

        pub fn try_from_char(c: char) -> Option<Self> {
            let c = c as u8;

            for offset in ['A', 'a'] {
                if let Some(x) = c.checked_sub(offset as u8) {
                    if let Some(c) = Letter::try_from_u32(x as u32) {
                        return Some(c);
                    }
                }
            }

            None
        }
    }

    impl crate::Letter for Character {
        type String = CharVec;

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
            CharVec(items.copied().collect())
        }
    }

    pub const PLANETS_BYTES: &[u8] = &[
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
