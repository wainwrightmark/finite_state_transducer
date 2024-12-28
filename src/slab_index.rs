#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SlabIndex(pub u32);

impl From<usize> for SlabIndex {
    fn from(value: usize) -> Self {
        Self(value as u32)
    }
}

impl SlabIndex {

    pub const ZERO: Self =Self(0);

    pub fn increment(&mut self) {
        self.0 += 1;
    }
}