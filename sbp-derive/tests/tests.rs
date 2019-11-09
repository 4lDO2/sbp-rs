use sbp::{Parser, Serializer, Take};
use sbp_derive::sbp;

#[test]
fn custom_parsing() {
    #[sbp(parsable, serializable)]
    struct Struct {
        a: Le<u64>,
        b: Be<u32>,
        c: Be<i32>,
        len: Le<u16>,

        #[custom(Take, *len as usize)]
        bytes: Vec<u8>,
    }
    let string = b"Hello, world!";

    let mut bytes = vec![
        0x10,
        0x20,
        0x30,
        0x40,
        0x50,
        0x60,
        0x70,
        0x80, // a
        0x10,
        0x11,
        0x12,
        0x13, // b
        0xFF,
        0xFF,
        0xFF,
        0xFF, // c
        string.len() as u8,
        0x00, // len
    ];
    bytes.extend(string);

    let (s, len) = Struct::parse((), &bytes).unwrap();
    assert_eq!(len, bytes.len());
    assert_eq!(s.a, 0x8070605040302010);
    assert_eq!(s.b, 0x10111213);
    assert_eq!(s.c, -1);
    assert_eq!(s.bytes, string);

    let mut bytes2 = vec![0u8; bytes.len()];
    Struct::serialize(&s, (), &mut bytes2).unwrap();
    assert_eq!(bytes, bytes2);
}

#[test]
fn alignment() {
    #[sbp(parsable)]
    struct Struct {
        eight: Le<u8>,

        #[align(2)]
        sixteen: Le<u16>,

        #[align(4)]
        be: Be<u32>,

        #[align(16)]
        some_bytes_after: Be<u128>,
    }

    let bytes = [
        0x08, // eight
        0xFF, // alignment
        0x10, 0x00, // sixteen
        0xDE, 0xAD, 0xBE, 0xEF, // be
        // alignment
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, // some_bytes_after
        0xDE, 0xAD, 0xBE, 0xEF, 0xFE, 0xD0, 0xFA, 0xCE, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xF0,
    ];

    let (s, len) = Struct::parse((), &bytes).unwrap();
    assert_eq!(len, bytes.len());
    assert_eq!(s.eight, 8);
    assert_eq!(s.sixteen, 16);
    assert_eq!(s.be, 0xDEADBEEF);
    assert_eq!(s.some_bytes_after, 0xDEADBEEF_FED0FACE_FFFFFFFF_FFFFFFF0);
}

#[test]
fn conditional() {
    #[sbp(parsable)]
    #[derive(Debug, PartialEq)]
    struct Struct {
        a: Le<u32>,
        b: Le<u16>,

        #[align(4)]
        version: Le<u32>,

        #[condition(*version >= 1)]
        extra_features: Be<u64>,

        #[condition(*version >= 2)]
        extra_extra_features: Be<u64>,
    }

    {
        let obsolete_bytes = [
            0x42, 0x42, 0x42, 0x42, // a
            0x37, 0x13, // b
            0xFF, 0xFF, // alignment
            0x00, 0x00, 0x00, 0x00, // version
        ];

        let (s, len) = Struct::parse((), &obsolete_bytes).unwrap();
        assert_eq!(len, 12);
        assert_eq!(
            s,
            Struct {
                a: 0x42424242,
                b: 0x1337,
                version: 0,
                extra_features: None,
                extra_extra_features: None,
            }
        );
    }
    {
        let stable_bytes = [
            0x42, 0x42, 0x42, 0x42, // a
            0x37, 0x13, // b
            0xFF, 0xFF, // alignment
            0x01, 0x00, 0x00, 0x00, // version
            0x00, 0x22, 0x44, 0x66, 0x88, 0xAA, 0xCC, 0xEE, // extra_features
        ];
        let (s, len) = Struct::parse((), &stable_bytes).unwrap();
        assert_eq!(len, 20);
        assert_eq!(
            s,
            Struct {
                a: 0x42424242,
                b: 0x1337,
                version: 1,
                extra_features: Some(0x0022446688AACCEE),
                extra_extra_features: None,
            }
        );
    }
    {
        let bleeding_edge_bytes = [
            0x42, 0x42, 0x42, 0x42, // a
            0x37, 0x13, // b
            0xFF, 0xFF, // alignment
            0x00, 0x00, 0x00, 0xF0, // version
            0x00, 0x22, 0x44, 0x66, 0x88, 0xAA, 0xCC, 0xEE, // extra_features
            0x11, 0x33, 0x55, 0x77, 0x99, 0xBB, 0xDD, 0xFF, // extra_extra_features
        ];
        let (s, len) = Struct::parse((), &bleeding_edge_bytes).unwrap();
        assert_eq!(len, 28);
        assert_eq!(
            s,
            Struct {
                a: 0x42424242,
                b: 0x1337,
                version: 0xF000_0000,
                extra_features: Some(0x0022446688AACCEE),
                extra_extra_features: Some(0x1133557799BBDDFF),
            }
        );
    }
}

#[test]
fn parse_and_serialize() {
    #[sbp(parsable, serializable)]
    #[derive(Debug, PartialEq)]
    struct Struct {
        a: Le<u64>,
        b: Be<u32>,
        c: Be<i32>,
    }

    let instance = Struct {
        a: 0xDEAD_BEEF_FED_FACE,
        b: 0x13371337,
        c: -0x13371337,
    };

    let mut buffer = [0u8; 16];
    Struct::serialize(&instance, (), &mut buffer).unwrap();

    let (instance2, len) = Struct::parse((), &buffer).unwrap();
    assert_eq!(len, buffer.len());
    assert_eq!(instance, instance2);
}

#[test]
fn conditional_serializing() {
    #[sbp(parsable, serializable)]
    #[derive(Debug, PartialEq)]
    struct Struct {
        a: Le<u32>,
        b: Le<u16>,

        #[align(4)]
        version: Le<u32>,

        #[condition(*version >= 1)]
        extra_features: Be<u64>,

        #[condition(*version >= 2)]
        extra_extra_features: Be<u64>,
    }

    {
        let obsolete_bytes = [
            0x42, 0x42, 0x42, 0x42, // a
            0x37, 0x13, // b
            0xFF, 0xFF, // alignment
            0x00, 0x00, 0x00, 0x00, // version
        ];

        let (s, len) = Struct::parse((), &obsolete_bytes).unwrap();
        assert_eq!(len, 12);
        assert_eq!(
            s,
            Struct {
                a: 0x42424242,
                b: 0x1337,
                version: 0,
                extra_features: None,
                extra_extra_features: None,
            }
        );
    }
    {
        let stable_bytes = [
            0x42, 0x42, 0x42, 0x42, // a
            0x37, 0x13, // b
            0xFF, 0xFF, // alignment
            0x01, 0x00, 0x00, 0x00, // version
            0x00, 0x22, 0x44, 0x66, 0x88, 0xAA, 0xCC, 0xEE, // extra_features
        ];
        let (s, len) = Struct::parse((), &stable_bytes).unwrap();
        assert_eq!(len, 20);
        assert_eq!(
            s,
            Struct {
                a: 0x42424242,
                b: 0x1337,
                version: 1,
                extra_features: Some(0x0022446688AACCEE),
                extra_extra_features: None,
            }
        );
    }
    {
        let bleeding_edge_bytes = [
            0x42, 0x42, 0x42, 0x42, // a
            0x37, 0x13, // b
            0xFF, 0xFF, // alignment
            0x00, 0x00, 0x00, 0xF0, // version
            0x00, 0x22, 0x44, 0x66, 0x88, 0xAA, 0xCC, 0xEE, // extra_features
            0x11, 0x33, 0x55, 0x77, 0x99, 0xBB, 0xDD, 0xFF, // extra_extra_features
        ];
        let (s, len) = Struct::parse((), &bleeding_edge_bytes).unwrap();
        assert_eq!(len, 28);
        assert_eq!(
            s,
            Struct {
                a: 0x42424242,
                b: 0x1337,
                version: 0xF000_0000,
                extra_features: Some(0x0022446688AACCEE),
                extra_extra_features: Some(0x1133557799BBDDFF),
            }
        );
    }
}

#[test]
fn bitflags_invocation() {
    sbp::parsable_bitflags! {
        pub struct Flags: Le<u16> {
            const A = 0x1;
            const B = 0x2;
        }
    }

    #[sbp(parsable)]
    #[derive(Debug, PartialEq)]
    struct Struct {
        len: Le<u16>,
        offset: Le<u32>,
        flags: Flags,
    }

    let bytes = [
        0xFA, 0xCE,
        0xDE, 0xAD, 0xBE, 0xEF,
        0x03, 0x00,
    ];

    let (s, len) = Struct::parse((), &bytes).unwrap();
    assert_eq!(len, bytes.len());
    assert_eq!(s, Struct {
        len: 0xCEFA,
        offset: 0xEFBEADDE,
        flags: Flags::A | Flags::B,
    });
}
