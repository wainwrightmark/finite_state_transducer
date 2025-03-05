use std::{iter::FusedIterator, marker::PhantomData};

use crate::{index::FSTIndex, Letter, LetterJoiner, State, FST};

pub struct FSTIterator<'f, L: Letter, F: FST<L>, LJ: LetterJoiner<L>> {
    fst: &'f F,
    index_stack: Vec<FSTIndex>,
    letter_stack: Vec<L>,
    phantom: PhantomData<LJ>
}

impl<'f, L: Letter, F: FST<L>, LJ: LetterJoiner<L>> FSTIterator<'f, L, F, LJ> {
    pub fn new(fst: &'f F) -> Self {
        Self {
            fst,
            index_stack: vec![FSTIndex(0)],
            letter_stack: vec![],
            phantom: PhantomData
        }
    }
}

impl<L: Letter, F: FST<L>, LJ: LetterJoiner<L>> FusedIterator for FSTIterator<'_, L, F, LJ> {}

impl<L: Letter, F: FST<L>, LJ: LetterJoiner<L>> Iterator for FSTIterator<'_, L, F, LJ> {
    type Item = LJ::String;

    fn next(&mut self) -> Option<Self::Item> {
        fn increment_last<C: Letter>(
            index_stack: &mut Vec<FSTIndex>,
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

            let top = &self.fst.get_state(top_state_index);

            if let Some(letter) = self.letter_stack.get(self.index_stack.len() - 1) {
                if let Some(nsi) = top.try_accept(letter) {
                    self.index_stack.push(nsi);
                } else {
                    increment_last(&mut self.index_stack, &mut self.letter_stack);
                }
            } else {
                let result: Option<LJ::String> = if top.can_terminate() {
                    let word = LJ::join(self.letter_stack.iter());
                    Some(word)
                } else {
                    None
                };

                if let Some((next_char, next_index)) = top.try_first() {
                    self.letter_stack.push(next_char);
                    self.index_stack.push(next_index);
                } else {
                    self.index_stack.pop();
                    increment_last(&mut self.index_stack, &mut self.letter_stack);
                }

                if result.is_some() {
                    return result;
                }
            }
        }
    }
}
