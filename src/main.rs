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

#[parsable]
#[derive(Debug)]
struct DataPhys {
    version: Le<u64>,

    #[condition(version >= 1)]
    data: Data,
}

fn main() {
    let mut bytes = [0u8; 64];
    //bytes[..8].copy_from_slice(&[1, 2, 3, 4, 5, 6, 7, 8]);
    let data = DataPhys::parse((), &bytes);
    dbg!(data);
}
