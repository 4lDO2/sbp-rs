#[test]
fn parsable_bitflags_invocation() {
    use sbp::{Parser, Serializer};

    let bytes = [0x8F, 0x53, 0x22, 0x42];

    sbp::parsable_bitflags! {
        struct Struct: Be<u32> {
            const A = 0x0000_0002;
            const B = 0x0000_0040;
            const C = 0x0000_0200;
            const D = 0x0000_2000;
            const E = 0x0001_0000;
            const F = 0x0002_0000;
            const G = 0x0040_0000;
            const H = 0x0010_0000;
            const I = 0x0100_0000;
            const J = 0x0200_0000;
            const K = 0x0400_0000;
            const L = 0x0800_0000;
            const M = 0x8000_0000;
            const N = 0x0000_0001;
        }
    }

    let (parsed, len) = Struct::parse((), &bytes).unwrap();
    assert_eq!(len, 4);
    assert_eq!(
        parsed,
        Struct::A
            | Struct::B
            | Struct::C
            | Struct::D
            | Struct::E
            | Struct::F
            | Struct::G
            | Struct::H
            | Struct::I
            | Struct::J
            | Struct::K
            | Struct::L
            | Struct::M
    );

    let mut bytes2 = [0u8; 4];
    Struct::serialize(&parsed, (), &mut bytes2).unwrap();

    assert_eq!(bytes, bytes2);

    assert_eq!(<Struct as sbp::ParserKnownSize<'_, Struct>>::LEN, 4);
    assert_eq!(<Struct as sbp::SerializerKnownLength<'_, Struct>>::LEN, 4);
}
