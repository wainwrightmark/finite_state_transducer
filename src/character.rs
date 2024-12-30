pub trait AutomataCharacter: Sized {
    type String;

    fn try_from_u32(key: u32) -> Option<Self>;
    fn to_u32(&self) -> u32;

    fn join<'a>(items: impl Iterator<Item = &'a Self>) -> Self::String
    where
        Self: 'a;
}
