bitfield-derive
===============

This crate provides derive to generate struct with bitfield access.

Example
-------

```rust
use bitfield_derive::BitFields;

#[derive(Default, BitFields)]
struct Foo {
    #[bitfield(bar : [3:0] as u8 "The bar flags")]
    #[bitfield(baz : [7:4] as u8 "The baz flags")]
    #[bitfield(ro, _ : [8] as bool)] // Read only
    #[bitfield(_ , set_wr : [9] as bool)] // Write only
    #[bitfield(stuff : [31:16] as u16)]
    #[bitfield(all_bits : [31:0])]
    _bi1: u32,
    other: usize,
}

// Instance a struct with bitfields.
let mut foo = Foo::default();

// Initial states.
assert_eq!(foo.bar(), 0);
assert_eq!(foo.baz(), 0);

foo.set_bar(7);
foo.set_baz(3);
assert_eq!(foo.bar(), 7);
assert_eq!(foo.baz(), 3);

// Overflowing tests.
foo.set_bar(0x13);
foo.set_baz(0x17);
assert_eq!(foo.bar(), 3);
assert_eq!(foo.baz(), 7);

foo.set_bar(0x0f);
foo.set_baz(0x0f);
assert_eq!(foo.bar(), 0x0f);
assert_eq!(foo.baz(), 0x0f);

assert_eq!(foo.ro(), false);
// Compile fail if uncomment the follow line.
// foo.set_ro(false);

// Compile fail if uncomment the follow line.
// assert_eq!(foo.wr(), 0);
foo.set_wr(true);

assert_eq!(foo.stuff(), 0);
foo.set_stuff(0xffff);
assert_eq!(foo.stuff(), 0xffff);

// All bits in the container field.
assert_eq!(foo.all_bits(), 0xffff_02ff);
foo.set_all_bits(0);
assert_eq!(foo.all_bits(), 0);
```
