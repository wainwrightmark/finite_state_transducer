use crate::{frozen::FrozenFST, mutable::MutableFST, Letter};

pub enum EitherFST<'a, L: Letter>{
    Mutable(MutableFST<L>),
    Frozen(FrozenFST<'a, L>)
}