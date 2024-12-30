#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FSTIndex(pub u32);

impl From<usize> for FSTIndex {
    fn from(value: usize) -> Self {
        Self(value as u32)
    }
}

impl FSTIndex {
    pub const ZERO: Self = Self(0);

    pub fn increment(&mut self) {
        self.0 += 1;
    }
}
