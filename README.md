# sbp - a simple and compact binary parsing crate
[![Build Status](https://travis-ci.org/4lDO2/sbp-rs.svg?branch=master)](https://travis-ci.org/4lDO2/sbp-rs)

While libraries such as [`nom`](https://crates.io/crates/nom) and [`cookie-factory`](https://crates.io/crates/cookie-factory) can provide good parsing and serializing, they require a lot of boilerplate code. If your goal is to simply parse a structure like you would do it in C, you would need to first declare the struct, then use the parse macro which requires writing every field twice, and a serialize macro which requires writing every field once again. This is fine for a few simple structures, but in a filesystem implementation, for example, you might end up declaring over 100 structs. In that case a more compact parsing crate would be useful. On the other hand, transmuting raw bytes into the structs doesn't support configurable endianness, and might be undefined behavior. [`sbp`](https://crates.io/crates/sbp) provides a quick-and-dirty way to compactly define binary structs for parsing and serializing.

## License

Licensed under either of

 * Apache License, Version 2.0
   ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
 * MIT license
   ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

at your option.

## Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be
dual licensed as above, without any additional terms or conditions.
