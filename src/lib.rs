use std::{num::NonZeroUsize, ops::{Add, Div, Mul, Rem}};

pub trait ParseError {
    fn additional_required_bytes(&self) -> Option<NonZeroUsize>;
}

#[derive(Debug)]
pub struct BasicParseError {
    pub bytes_got: usize,
    pub bytes_required: usize,
}

impl ParseError for BasicParseError {
    fn additional_required_bytes(&self) -> Option<NonZeroUsize> {
        NonZeroUsize::new(self.bytes_required - self.bytes_got)
    }
}

pub trait Parser<'a, Target> {
    type Error: ParseError;

    // XXX: Const generics could possibly eliminate this associated type.
    type Data;

    fn parse(data: Self::Data, bytes: &'a [u8]) -> Result<(Target, usize), Self::Error>;
}

pub trait ParserKnownSize<'a, Target>: Parser<'a, Target> {
    const LEN: usize;
}

pub trait Parse<'a>: Parser<'a, Self>
where
    Self: Sized,
{
}

pub trait ParseKnownSize<'a>: ParserKnownSize<'a, Self> + Parse<'a> {}

pub struct Take(usize);

impl<'a> Parser<'a, &'a [u8]> for Take {
    type Error = BasicParseError;
    type Data = usize;

    fn parse(amount: usize, bytes: &'a [u8]) -> Result<(&[u8], usize), Self::Error> {
        if bytes.len() < amount {
            return Err(BasicParseError {
                bytes_got: bytes.len(),
                bytes_required: amount,
            });
        }
        Ok((&bytes[..amount], amount))
    }
}

pub struct Le;
pub struct Be;

macro_rules! parser_impl(
    ($target:ty, $parser:ty, $function:ident) => {
        impl<'a> Parser<'a, $target> for $parser {
            type Error = BasicParseError;
            type Data = ();

            fn parse(_: Self::Data, bytes: &'a [u8]) -> Result<($target, usize), Self::Error> {
                const SIZE: usize = ::std::mem::size_of::<$target>();

                if bytes.len() < SIZE {
                    return Err(BasicParseError { bytes_got: bytes.len(), bytes_required: SIZE });
                }

                let mut array = [0u8; SIZE];
                array.copy_from_slice(&bytes[..SIZE]);

                Ok((<$target>::$function(array), SIZE))
            }
        }
    };
);

parser_impl!(u8, Le, from_le_bytes);
parser_impl!(u16, Le, from_le_bytes);
parser_impl!(u32, Le, from_le_bytes);
parser_impl!(u64, Le, from_le_bytes);
parser_impl!(u128, Le, from_le_bytes);

parser_impl!(i8, Le, from_le_bytes);
parser_impl!(i16, Le, from_le_bytes);
parser_impl!(i32, Le, from_le_bytes);
parser_impl!(i64, Le, from_le_bytes);
parser_impl!(i128, Le, from_le_bytes);

parser_impl!(u8, Be, from_be_bytes);
parser_impl!(u16, Be, from_be_bytes);
parser_impl!(u32, Be, from_be_bytes);
parser_impl!(u64, Be, from_be_bytes);
parser_impl!(u128, Be, from_be_bytes);

parser_impl!(i8, Be, from_be_bytes);
parser_impl!(i16, Be, from_be_bytes);
parser_impl!(i32, Be, from_be_bytes);
parser_impl!(i64, Be, from_be_bytes);
parser_impl!(i128, Be, from_be_bytes);

pub fn align<T>(number: T, alignment: T) -> T
where
    T: Add<Output = T>
        + Copy
        + Div<Output = T>
        + Rem<Output = T>
        + From<u8>
        + Mul<Output = T>
        + PartialEq,
{
    (if number % alignment != T::from(0u8) {
        number / alignment + T::from(1u8)
    } else {
        number / alignment
    }) * alignment
}
