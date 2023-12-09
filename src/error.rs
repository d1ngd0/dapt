pub type DaptResult<T> = Result<T, Error>;

#[derive(Debug)]
pub enum Error {
    TypeMismatch(u8, String),
    IncorrectSize(String),
    InvalidIndex(String),
}
