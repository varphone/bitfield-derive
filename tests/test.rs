use bitfield_derive::BitFields;

#[test]
fn test_overflowing() {
    #[derive(BitFields)]
    struct Foo {
        #[bitfield(byte : [7:0] as u8)]
        #[bitfield(word : [23:8] as u16)]
        #[bitfield(flag : [31] as bool)]
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
