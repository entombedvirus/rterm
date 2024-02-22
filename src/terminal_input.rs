pub fn ctrl(key_name: &str, dst: &mut String) {
    match key_name.as_bytes() {
        [] => (),
        [first_byte @ 0x40..=0x7f] => append_ctrl_masked_key(*first_byte, dst),
        b"Backslash" | b"OpenBracket" | b"CloseBracket" => append_ctrl_masked_key(b'\\', dst),
        _unsupported => log::warn!("Unsupported ctrl key name: {}", key_name),
    }
}

// See: https://pbxbook.com/other/ctrlcods.html
// clear the top two bits to get the ctrl code
fn append_ctrl_masked_key(ascii_key: u8, dst: &mut String) {
    let ctrl_key_code = &[ascii_key & 0b0011_1111];
    let as_str = unsafe { std::str::from_utf8_unchecked(ctrl_key_code) };
    dst.push_str(as_str);
}

pub fn alt(txt: &str, dst: &mut String) {
    dst.push_str(match txt {
        "å" => "\u{1b}a",
        "∫" => "\u{1b}b",
        "ç" => "\u{1b}c",
        "∂" => "\u{1b}d",
        "e" => "\u{1b}e",
        "ƒ" => "\u{1b}f",
        "©" => "\u{1b}g",
        "˙" => "\u{1b}h",
        "i" => "\u{1b}i",
        "∆" => "\u{1b}j",
        "˚" => "\u{1b}k",
        "¬" => "\u{1b}l",
        "µ" => "\u{1b}m",
        "n" => "\u{1b}n",
        "ø" => "\u{1b}o",
        "π" => "\u{1b}p",
        "œ" => "\u{1b}q",
        "®" => "\u{1b}r",
        "ß" => "\u{1b}s",
        "†" => "\u{1b}t",
        "u" => "\u{1b}u",
        "√" => "\u{1b}v",
        "∑" => "\u{1b}w",
        "≈" => "\u{1b}x",
        "¥" => "\u{1b}y",
        "Ω" => "\u{1b}z",
        _ => {
            log::warn!("Unsupported alt key name: {}", txt);
            return;
        }
    });
}
