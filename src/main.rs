use sbp::Le;
use sbp_derive::parsable;

#[parsable]
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

fn main() {}
