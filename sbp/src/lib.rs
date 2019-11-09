use std::{
    num::NonZeroUsize,
    ops::{Add, Div, Mul, Rem},
};

#[cfg(feature = "derive")]
pub use sbp_derive::sbp;

#[cfg(feature = "derive")]
pub trait DeriveError: OutOfSpaceError + std::error::Error {}

impl<T> DeriveError for T where T: OutOfSpaceError + std::error::Error {}

#[cfg(feature = "derive")]
impl OutOfSpaceError for Box<dyn DeriveError> {
    fn additional_required_bytes(&self) -> Option<NonZeroUsize> {
        <Box<dyn DeriveError> as std::ops::Deref>::deref(self).additional_required_bytes()
    }
}

#[cfg(feature = "derive")]
impl std::error::Error for Box<dyn DeriveError> {}

#[cfg(feature = "derive")]
impl From<BasicOutOfSpaceError> for Box<dyn DeriveError> {
    fn from(basic: BasicOutOfSpaceError) -> Self {
        Box::new(basic)
    }
}

/// An error that may be caused by insufficient bytes.
pub trait OutOfSpaceError {
    fn additional_required_bytes(&self) -> Option<NonZeroUsize>;
}

/// An error indicating that an insufficient amount of bytes were available when parsing or
/// serializing.
#[derive(Debug)]
pub struct BasicOutOfSpaceError {
    /// The amount of bytes supplied.
    pub bytes_got: usize,

    /// The amount of bytes required for successful parsing.
    pub bytes_required: usize,
}

impl std::fmt::Display for BasicOutOfSpaceError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "expected {} bytes, got {}",
            self.bytes_required, self.bytes_got
        )
    }
}
impl std::error::Error for BasicOutOfSpaceError {}

impl OutOfSpaceError for BasicOutOfSpaceError {
    fn additional_required_bytes(&self) -> Option<NonZeroUsize> {
        NonZeroUsize::new(self.bytes_required - self.bytes_got)
    }
}

/// Represents a parser that can parse a sequency of bytes into a meaningful type.
pub trait Parser<'a, Target> {
    /// The error that the parsing can result in.
    type Error: OutOfSpaceError;

    // XXX: Const generics could possibly eliminate this associated type, its only real use is the
    // Take combinator, which byte count could be specified using const generics.
    /// An arbitrary additional value provided, this will usually be `()`.
    /// When const generics comes, this probably won't be required.
    type Meta;

    /// Parse bytes. The bytes may or may not have an adequately size for successful parsing, this
    /// should be taken into account when implementing a parser.
    fn parse(meta: Self::Meta, bytes: &'a [u8]) -> Result<(Target, usize), Self::Error>;
}

/// A parser which size is known at compile time.
pub trait ParserKnownSize<'a, Target>: Parser<'a, Target> {
    const LEN: usize;
}

/// A type that can parse itself.
pub trait Parse<'a>: Parser<'a, Self>
where
    Self: Sized,
{
}

/// A type that can parse itself, and with a static size.
pub trait ParseKnownSize<'a>: ParserKnownSize<'a, Self> + Parse<'a> {}

/// A parser that takes an arbitrary amount of bytes.
pub struct Take;

/// A parser that repeats the same parser multiple times.
pub struct Repeat<P> {
    _marker: std::marker::PhantomData<P>
}

impl<'a, P, T> Parser<'a, Vec<T>> for Repeat<P>
where
    P: Parser<'a, T>,
    P::Meta: Default,
{
    type Error = P::Error;
    type Meta = usize;

    fn parse(count: usize, bytes: &'a [u8]) -> Result<(Vec<T>, usize), Self::Error> {
        let mut offset = 0;

        let vector = (0..count).map(|_| {
            let (value, additional_bytes) = P::parse(Default::default(), &bytes[offset..])?;
            offset += additional_bytes;
            Ok(value)
        }).collect::<Result<Vec<_>, P::Error>>()?;
        Ok((vector, offset))
    }
}

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

/// A serializer that can serialize a type into raw bytes.
pub trait Serializer<'a, T> {
    /// An extra value provided, currently only meant and used for `Take`.
    type Meta;

    /// The error that may occur when serializing.
    type Error: OutOfSpaceError;

    /// Serialize a type into raw bytes.
    fn serialize(data: &T, meta: Self::Meta, bytes: &'a mut [u8]) -> Result<usize, Self::Error>;
}

/// A type that can serialize itself.
pub trait Serialize<'a>: Serializer<'a, Self>
where
    Self: Sized,
{
}

/// A type which requires a static (known at compile-time) amount of bytes for serializing to.
pub trait SerializerKnownLength<'a, T>: Serializer<'a, T> {
    const LEN: usize;
}

/// A type that can serialize itself and has a static raw size.
pub trait SerializeKnownLength<'a>: SerializerKnownLength<'a, Self> + Serialize<'a>
where
    Self: Sized,
{
}

/// A parser for parsing unsigned and signed integers, stored in little-endian.
///
/// Note that this type is frequently used in the `sbp` macro, in that case it's an intermediate
/// type, since there normally is no type argument here.

pub struct Le;
/// A parser for parsing unsigned and signed integers, stored in big-endian.
///
/// Note that this type is frequently used in the `sbp` macro, in that case it's an intermediate
/// type, since there normally is no type argument here.
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

/// Align a number to an alignment, by rounding upwards.
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

/// An error caused by either an insufficient byte count, or an invalid bitmask.
///
/// _Only available if the `bitflags` feature has been enabled._
#[cfg(feature = "bitflags")]
#[derive(Debug)]
pub enum ParseBitflagsError<T> {
    InsufficientSize(BasicOutOfSpaceError),
    InvalidBitmask(T, T),
}

impl<T: std::fmt::LowerHex> std::fmt::Display for ParseBitflagsError<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::InsufficientSize(error) => std::fmt::Display::fmt(&error, f),
            Self::InvalidBitmask(got, expected) => write!(
                f,
                "invalid bitmask, bitmask 0x{:x} wasn't contained in 0x{:x}",
                got, expected
            ),
        }
    }
}
#[cfg(feature = "bitflags")]
impl<T: std::fmt::Debug + std::fmt::LowerHex> std::error::Error for ParseBitflagsError<T> {}

#[cfg(feature = "bitflags")]
impl<T> From<BasicOutOfSpaceError> for ParseBitflagsError<T> {
    fn from(basic: BasicOutOfSpaceError) -> Self {
        Self::InsufficientSize(basic)
    }
}

#[cfg(feature = "bitflags")]
impl<T> OutOfSpaceError for ParseBitflagsError<T> {
    fn additional_required_bytes(&self) -> Option<NonZeroUsize> {
        match self {
            Self::InsufficientSize(err) => err.additional_required_bytes(),
            _ => None,
        }
    }
}

#[cfg(all(feature = "bitflags", feature = "derive"))]
impl<T: std::fmt::LowerHex + std::fmt::Debug + 'static> From<ParseBitflagsError<T>> for Box<dyn DeriveError> {
    fn from(err: ParseBitflagsError<T>) -> Self {
        Box::new(err)
    }
}

/// Declare a bitflags struct that can be parsed and serialized.
///
/// _Only available if the `bitflags` feature has been enabled._
///
/// ## Example
/// ```rust
/// # use sbp::{Parser, parsable_bitflags};
/// #
/// # fn main() -> Result<(), Box<dyn std::error::Error>> {
///
/// parsable_bitflags! {
///     pub struct MyStruct: Le<u16> {
///         const FLAG_A = 0x8;
///         const FLAG_B = 0x10;
///         const FLAG_C = 0x200;
///     }
/// }
///
/// let bytes = [0x18, 0x02];
/// let (my_struct, _length) = MyStruct::parse((), &bytes)?;
/// assert_eq!(my_struct, MyStruct::FLAG_A | MyStruct::FLAG_B | MyStruct::FLAG_C);
///
/// # Ok(())
/// # }
/// ```
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

#[cfg(test)]
mod tests {
    #[test]
    fn take_bytes() {
        use crate::{Parser, Take};

        let bytes = b"Hello, world! Hello, again!";

        let (subrange, len): (&[u8], usize) = Take::parse(13, bytes).unwrap();
        assert_eq!(len, 13);
        assert_eq!(subrange, b"Hello, world!");
    }

    #[test]
    fn repeat_struct() {
        use crate::{Le, Parser, Repeat};

        let bytes = [
            0x00, 0x0F, 0xF0, 0xFF,
            0x00, 0x0E, 0xE0, 0xEE,
            0x00, 0x0D, 0xD0, 0xDD,
            0x00, 0x0C, 0xC0, 0xCC,
        ];

        let (nums, len): (Vec<u32>, usize) = Repeat::<Le>::parse(4, &bytes).unwrap();
        assert_eq!(len, bytes.len());
        assert_eq!(&nums[..], &[
            0xFFF00F00,
            0xEEE00E00,
            0xDDD00D00,
            0xCCC00C00,
        ]);
    }
}
