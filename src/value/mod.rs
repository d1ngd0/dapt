use std::mem;

// Serialize is used to
pub trait Serialize {
    fn size_of(&self) -> usize;
    fn serialize(&self, buf: &mut [u8]);
}

// Deserialize is used to turn the type back into the
//
pub trait Deserialize {
    type Item;
    fn deserialize(buf: &[u8]) -> Self::Item;
}

impl Serialize for u8 {
    fn size_of(&self) -> usize {
        mem::size_of::<u8>()
    }

    fn serialize(&self, buf: &mut [u8]) {
        buf[0] = *self
    }
}

impl Deserialize for u8 {
    type Item = u8;

    fn deserialize(buf: &[u8]) -> Self::Item {
        buf[0]
    }
}
