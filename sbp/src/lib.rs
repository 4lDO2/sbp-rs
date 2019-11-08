use std::{
    num::NonZeroUsize,
    ops::{Add, Div, Mul, Rem},
};

#[cfg(feature = "derive")]
pub use sbp_derive::sbp;

pub trait OutOfSpaceError {
    fn additional_required_bytes(&self) -> Option<NonZeroUsize>;
}

#[derive(Debug)]
pub struct BasicOutOfSpaceError {
    pub bytes_got: usize,
    pub bytes_required: usize,
}

impl OutOfSpaceError for BasicOutOfSpaceError {
    fn additional_required_bytes(&self) -> Option<NonZeroUsize> {
        NonZeroUsize::new(self.bytes_required - self.bytes_got)
    }
}

pub trait Parser<'a, Target> {
    type Error: OutOfSpaceError;

    // XXX: Const generics could possibly eliminate this associated type, its only real use is the
    // Take combinator, which byte count could be specified using const generics.
    type Meta;

    fn parse(meta: Self::Meta, bytes: &'a [u8]) -> Result<(Target, usize), Self::Error>;
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

impl<'a, T> Parser<'a, T> for Take
where
    T: From<&'a [u8]>,
{
    type Error = BasicOutOfSpaceError;
    type Meta = usize;

    fn parse(amount: usize, bytes: &'a [u8]) -> Result<(T, usize), Self::Error> {
        if bytes.len() < amount {
            return Err(BasicOutOfSpaceError {
                bytes_got: bytes.len(),
                bytes_required: amount,
            });
        }
        Ok((T::from(&bytes[..amount]), amount))
    }
}

impl<'a, T> Serializer<'a, T> for Take
where
    for<'b> &'b T: AsRef<[u8]>,
{
    type Error = BasicOutOfSpaceError;
    type Meta = usize;

    fn serialize(this: &T, amount: usize, bytes: &'a mut [u8]) -> Result<usize, Self::Error> {
        if bytes.len() < amount {
            return Err(BasicOutOfSpaceError {
                bytes_got: bytes.len(),
                bytes_required: amount,
            });
        }
        bytes[..amount].copy_from_slice(this.as_ref());
        Ok(amount)
    }
}

pub trait Serializer<'a, T> {
    type Meta;
    type Error: OutOfSpaceError;

    fn serialize(data: &T, meta: Self::Meta, bytes: &'a mut [u8]) -> Result<usize, Self::Error>;
}

pub trait Serialize<'a>: Serializer<'a, Self>
where
    Self: Sized,
{
}

pub trait SerializerKnownLength<'a, T>: Serializer<'a, T> {
    const LEN: usize;
}
pub trait SerializeKnownLength<'a>: SerializerKnownLength<'a, Self> + Serialize<'a>
where
    Self: Sized,
{
}

pub struct Le;
pub struct Be;

macro_rules! parser_impl(
    ($target:ty, $parser:ty, $from:ident, $to:ident) => {
        impl<'a> Parser<'a, $target> for $parser {
            type Error = BasicOutOfSpaceError;
            type Meta = ();

            fn parse(_: Self::Meta, bytes: &'a [u8]) -> Result<($target, usize), Self::Error> {
                const SIZE: usize = ::std::mem::size_of::<$target>();

                if bytes.len() < SIZE {
                    return Err(BasicOutOfSpaceError { bytes_got: bytes.len(), bytes_required: SIZE });
                }

                let mut array = [0u8; SIZE];
                array.copy_from_slice(&bytes[..SIZE]);

                Ok((<$target>::$from(array), SIZE))
            }
        }
        impl<'a> Serializer<'a, $target> for $parser {
            type Error = BasicOutOfSpaceError;
            type Meta = ();

            fn serialize(data: &$target, _: Self::Meta, bytes: &'a mut [u8]) -> Result<usize, Self::Error> {
                const SIZE: usize = ::std::mem::size_of::<$target>();

                if bytes.len() < SIZE {
                    return Err(BasicOutOfSpaceError { bytes_got: bytes.len(), bytes_required: SIZE });
                }

                let array = data.$to();
                bytes[..SIZE].copy_from_slice(&array);

                Ok(SIZE)
            }
        }
    };
);

parser_impl!(u8, Le, from_le_bytes, to_le_bytes);
parser_impl!(u16, Le, from_le_bytes, to_le_bytes);
parser_impl!(u32, Le, from_le_bytes, to_le_bytes);
parser_impl!(u64, Le, from_le_bytes, to_le_bytes);
parser_impl!(u128, Le, from_le_bytes, to_le_bytes);

parser_impl!(i8, Le, from_le_bytes, to_le_bytes);
parser_impl!(i16, Le, from_le_bytes, to_le_bytes);
parser_impl!(i32, Le, from_le_bytes, to_le_bytes);
parser_impl!(i64, Le, from_le_bytes, to_le_bytes);
parser_impl!(i128, Le, from_le_bytes, to_le_bytes);

parser_impl!(u8, Be, from_be_bytes, to_be_bytes);
parser_impl!(u16, Be, from_be_bytes, to_be_bytes);
parser_impl!(u32, Be, from_be_bytes, to_be_bytes);
parser_impl!(u64, Be, from_be_bytes, to_be_bytes);
parser_impl!(u128, Be, from_be_bytes, to_be_bytes);

parser_impl!(i8, Be, from_be_bytes, to_be_bytes);
parser_impl!(i16, Be, from_be_bytes, to_be_bytes);
parser_impl!(i32, Be, from_be_bytes, to_be_bytes);
parser_impl!(i64, Be, from_be_bytes, to_be_bytes);
parser_impl!(i128, Be, from_be_bytes, to_be_bytes);

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

#[derive(Debug)]
pub enum ParseBitflagsError<T> {
    InsufficientSize(BasicOutOfSpaceError),
    InvalidBitmask(T, T),
}

impl<T> From<BasicOutOfSpaceError> for ParseBitflagsError<T> {
    fn from(basic: BasicOutOfSpaceError) -> Self {
        Self::InsufficientSize(basic)
    }
}

impl<T> OutOfSpaceError for ParseBitflagsError<T> {
    fn additional_required_bytes(&self) -> Option<NonZeroUsize> {
        match self {
            Self::InsufficientSize(err) => err.additional_required_bytes(),
            _ => None,
        }
    }
}

/// Declare a bitflags struct that can be parsed and serialized.
/// _Only available if the `bitflags` feature has been enabled._
#[cfg(feature = "bitflags")]
#[macro_export]
macro_rules! parsable_bitflags(
    {
        __impl impl, $name:ident, $target:ty, $endianness:ident
    } => {
        impl<'a> ::sbp::Parser<'a, $name> for $name {
            type Meta = ();
            type Error = ::sbp::ParseBitflagsError<$target>;

            fn parse(_: Self::Meta, bytes: &'a [u8]) -> Result<(Self, usize), Self::Error> {
                let (raw, size) = <::sbp::$endianness as ::sbp::Parser<'a, $target>>::parse((), bytes)?;
                Self::from_bits(raw).map(|this| (this, size)).ok_or(::sbp::ParseBitflagsError::<$target>::InvalidBitmask(raw, Self::all().bits()))
            }
        }
        impl<'a> ::sbp::Parse<'a> for $name {}

        impl<'a> ::sbp::ParserKnownSize<'a, $name> for $name {
            const LEN: usize = ::std::mem::size_of::<$target>();
        }
        impl<'a> ::sbp::ParseKnownSize<'a> for $name {}

        impl<'a> ::sbp::Serializer<'a, $name> for $name {
            type Meta = ();
            type Error = ::sbp::BasicOutOfSpaceError;

            fn serialize(data: &Self, _: Self::Meta, bytes: &'a mut [u8]) -> Result<usize, Self::Error> {
                let raw = data.bits();
                <::sbp::$endianness as ::sbp::Serializer<'a, $target>>::serialize(&raw, (), bytes)
            }
        }
        impl<'a> ::sbp::Serialize<'a> for $name {}

        impl<'a> ::sbp::SerializerKnownLength<'a, $name> for $name {
            const LEN: usize = ::std::mem::size_of::<$target>();
        }
        impl<'a> ::sbp::SerializeKnownLength<'a> for $name {}
    };
    {
        pub struct $name:ident: Le<$repr:ty> {
            $($body:tt)+
        }
    } => {
        ::bitflags::bitflags! {
            pub struct $name: $repr {
                $($body)+
            }
        }
        ::sbp::parsable_bitflags! { __impl impl, $name, $repr, Le }
    };
    {
        struct $name:ident: Be<$repr:ty> {
            $($body:tt)+
        }
    } => {
        ::bitflags::bitflags! {
            struct $name: $repr {
                $($body)+
            }
        }
        ::sbp::parsable_bitflags! { __impl impl, $name, $repr, Be }
    };

    {
        pub struct $name:ident: Le<$repr:ty> {
            $($body:tt)+
        }
    } => {
        ::bitflags::bitflags! {
            pub struct $name: $repr {
                $($body)+
            }
        }
        ::sbp::parsable_bitflags! { __impl impl, $name, $repr, Le }
    };
    {
        struct $name:ident: Be<$repr:ty> {
            $($body:tt)+
        }
    } => {
        ::bitflags::bitflags! {
            struct $name: $repr {
                $($body)+
            }
        }
        ::sbp::parsable_bitflags! { __impl impl, $name, $repr, Be }
    };
);

