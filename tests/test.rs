use bitfield_derive::BitFields;

#[test]
fn test_signed() {
    #[derive(BitFields)]
    struct Foo {
        #[bitfield(byte @ "7:0" as i8)]
        #[bitfield(word @ "23:8" as i16)]
        #[bitfield(flag @ "31" as bool)]
        #[bitfield(signed_all @ "31:0" as i32)]
        all: u32,
    }
    let mut f = Foo { all: 0 };
    assert_eq!(f.byte(), 0);
    assert_eq!(f.word(), 0);
    assert_eq!(f.flag(), false);

    f.set_byte(-1);
    f.set_word(-1);
    f.set_flag(true);

    assert_eq!(f.byte(), -1);
    assert_eq!(f.word(), -1);
    assert_eq!(f.flag(), true);
    assert_eq!(f.all, 0x80ff_ffff);

    for v in -128..=127 {
        f.set_byte(v);
        assert_eq!(f.byte(), v);
        assert_eq!(f.all, 0x80ff_ff00 | v as u8 as u32);
    }

    for v in i16::MIN..=i16::MAX {
        f.set_word(v);
        assert_eq!(f.word(), v);
        assert_eq!(f.all, 0x8000_007f | (v as u16 as u32) << 8);
    }

    f.set_signed_all(-1);
    assert_eq!(f.signed_all(), -1);
}

#[test]
fn test_overflowing() {
    #[derive(BitFields)]
    struct Foo {
        #[bitfield(byte @ "7:0" as u8)]
        #[bitfield(word @ "23:8" as u16)]
        #[bitfield(flag @ "31" as bool)]
        all: usize,
    }
    let mut f = Foo { all: 0 };
    assert_eq!(f.byte(), 0);
    assert_eq!(f.word(), 0);
    assert_eq!(f.flag(), false);
    f.set_byte(0xff);
    f.set_word(0xffff);
    f.set_flag(true);
    assert_eq!(f.byte(), 0xff);
    assert_eq!(f.word(), 0xffff);
    assert_eq!(f.flag(), true);
    assert_eq!(f.all, 0x80ff_ffff);
}
