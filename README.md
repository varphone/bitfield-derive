bitfield-derive
===============

This crate provides derive to generate struct with bitfield access.

Examples
--------

```rust
use bitfield_derive::BitFields;

#[derive(Default, BitFields)]
struct Foo {
    #[bitfield(bar @ "3:0" as u8 "The bar flags")]
    #[bitfield(baz @ "7:4" as u8 "The baz flags")]
    #[bitfield(ro, _ @ "8" as bool)] // Read only
    #[bitfield(_ , set_wr @ "9" as bool)] // Write only
    #[bitfield(stuff @ "31:16" as u16)]
    #[bitfield(all_bits @ "31:0")]
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

Visibility
----------

The visibility of the bitfield follows the container field.

Examples:

```rust
use bitfield_derive::BitFields;

#[derive(Default, BitFields)]
pub struct Foo {
    #[bitfield(bar @ "3:0" as u8 "The bar flags")]
    pub _bi1: u32;
    #[bitfield(baz @ "3:0" as u8 "The baz flags")]
    _bi2: u32;
}
```

The container field `Foo::_bi1` is `pub`, so the `Foo::bar() and Foo::set_bar()` are implements as:

```rust
impl Foo {
    pub fn bar(&self) -> u8 { ... }
    pub fn set_bar(&mut self, value: u8) { ... }
}
```

The container field `Foo::_bi2` is `private`, so the `Foo::baz() and Foo::set_baz()` are implements as:

```rust
impl Foo {
    fn baz(&self) -> u8 { ... }
    fn set_baz(&mut self, value: u8) { ... }
}
```

Documentation
-------------

The last parameter of the bitfield is the description of the `getter` and `setter`.

The document generated by the above example is as follows:

```rust
[-] pub(crate) fn bar(&self) -> u8
    The bar flags

[-] pub(crate) fn set_bar(&mut self, value: u8)
    See also: bar

[-] pub(crate) fn baz(&self) -> u8
    The baz flags

[-] pub(crate) fn set_baz(&mut self, value: u8)
    See also: baz

...
```
