use byteorder::{ByteOrder, BigEndian, LittleEndian};
use std::str;

static SIZE_8: usize = 1;
static SIZE_16: usize = 2;
static SIZE_32: usize = 4;
static SIZE_64: usize = 8;
static SIZE_BOOL: usize = 1;

pub fn i8(_le: bool, input: &[u8], pos: usize) -> (i8, usize) {
	(input[pos] as i8, SIZE_8)
}

pub fn u8(_le: bool, input: &[u8], pos: usize) -> (u8, usize) {
	(input[pos], SIZE_8)
}

pub fn i16(le: bool, input: &[u8], pos: usize) -> (i16, usize) {
	let val = if le {
		LittleEndian::read_i16(&input[pos..pos+SIZE_16])
	} else {
		BigEndian::read_i16(&input[pos..pos+SIZE_16])
	};

	(val, SIZE_16)
}

pub fn u16(le: bool, input: &[u8], pos: usize) -> (u16, usize) {
	let val = if le {
		LittleEndian::read_u16(&input[pos..pos+SIZE_16])
	} else {
		BigEndian::read_u16(&input[pos..pos+SIZE_16])
	};

	(val, SIZE_16)
}

pub fn i32(le: bool, input: &[u8], pos: usize) -> (i32, usize) {
	let val = if le {
		LittleEndian::read_i32(&input[pos..pos+SIZE_32])
	} else {
		BigEndian::read_i32(&input[pos..pos+SIZE_32])
	};

	(val, SIZE_32)
}

pub fn u32(le: bool, input: &[u8], pos: usize) -> (u32, usize) {
	let val = if le {
		LittleEndian::read_u32(&input[pos..pos+SIZE_32])
	} else {
		BigEndian::read_u32(&input[pos..pos+SIZE_32])
	};

	(val, SIZE_32)
}

pub fn i64(le: bool, input: &[u8], pos: usize) -> (i64, usize) {
	let val = if le {
		LittleEndian::read_i64(&input[pos..pos+SIZE_64])
	} else {
		BigEndian::read_i64(&input[pos..pos+SIZE_64])
	};

	(val, SIZE_64)
}

pub fn u64(le: bool, input: &[u8], pos: usize) -> (u64, usize) {
	let val = if le {
		LittleEndian::read_u64(&input[pos..pos+SIZE_64])
	} else {
		BigEndian::read_u64(&input[pos..pos+SIZE_64])
	};

	(val, SIZE_64)
}

// TODO
/*pub fn isize(le: bool, input: &[u8], pos: usize) -> (isize, usize) {

}

pub fn usize(le: bool, input: &[u8], pos: usize) -> (usize, usize) {

}*/

pub fn f32(le: bool, input: &[u8], pos: usize) -> (f32, usize) {
	let val = if le {
		LittleEndian::read_f32(&input[pos..pos+SIZE_32])
	} else {
		BigEndian::read_f32(&input[pos..pos+SIZE_32])
	};

	(val, SIZE_32)
}

pub fn f64(le: bool, input: &[u8], pos: usize) -> (f64, usize) {
	let val = if le {
		LittleEndian::read_f64(&input[pos..pos+SIZE_64])
	} else {
		BigEndian::read_f64(&input[pos..pos+SIZE_64])
	};

	(val, SIZE_64)
}

pub fn bool(_le: bool, input: &[u8], pos: usize) -> (bool, usize) {
	(input[pos] == 1, SIZE_BOOL)
}

// TODO: DRY
pub fn char(_le: bool, input: &[u8], pos: usize) -> (char, usize) {
	let chr = {
        // Cannot actually cause undefined behavior.
        let slice = unsafe { str::from_utf8_unchecked(&input[pos..]) };

        if let Some(c) = slice.chars().next() {
            Some(c)
        } else {
            None
        }
    };

    match chr {
        Some(chr) => {
            (chr, chr.len_utf8())
        }
        None => panic!("Could not decode char")
    }
}
