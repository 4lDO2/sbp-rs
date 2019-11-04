use sbp::{Le, Parser};
use sbp_derive::parsable;

#[parsable]
#[derive(Debug)]
struct Data {
    flags: Le<u64>,
    block_count: Le<u64>,
    len: Le<u64>,
    inode_count: Le<u64>,
    u32_count: Le<u32>,

    #[align(2)]
    u16_count: Le<u16>,

    #[pad(2)]
    padded: Le<u128>,
}

fn main() {
    let bytes = [0u8; 56];
    let data = Data::parse((), &bytes);
    dbg!(data);
}
