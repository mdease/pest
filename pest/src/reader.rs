use byteorder::{ByteOrder, BigEndian, LittleEndian};
use std::str;

static SIZE_8: usize = 1;
static SIZE_16: usize = 2;
static SIZE_32: usize = 4;
static SIZE_64: usize = 8;
static SIZE_BOOL: usize = 1;

pub fn i8(_le: bool, input: &mut Vec<u8>) -> i8 {
	input.remove(0) as i8
}

pub fn u8(_le: bool, input: &mut Vec<u8>) -> u8 {
	input.remove(0)
}

pub fn i16(le: bool, input: &mut Vec<u8>) -> i16 {
	let val: Vec<u8> = input.drain(0..SIZE_16).collect();
	let slice = val.as_slice();

	if le {
		LittleEndian::read_i16(slice)
	} else {
		BigEndian::read_i16(slice)
	}
}

pub fn u16(le: bool, input: &mut Vec<u8>) -> u16 {
	let val: Vec<u8> = input.drain(0..SIZE_16).collect();
	let slice = val.as_slice();

	if le {
		LittleEndian::read_u16(slice)
	} else {
		BigEndian::read_u16(slice)
	}
}

pub fn i32(le: bool, input: &mut Vec<u8>) -> i32 {
	let val: Vec<u8> = input.drain(0..SIZE_32).collect();
	let slice = val.as_slice();

	if le {
		LittleEndian::read_i32(slice)
	} else {
		BigEndian::read_i32(slice)
	}
}

pub fn u32(le: bool, input: &mut Vec<u8>) -> u32 {
	let val: Vec<u8> = input.drain(0..SIZE_32).collect();
	let slice = val.as_slice();

	if le {
		LittleEndian::read_u32(slice)
	} else {
		BigEndian::read_u32(slice)
	}
}

pub fn i64(le: bool, input: &mut Vec<u8>) -> i64 {
	let val: Vec<u8> = input.drain(0..SIZE_64).collect();
	let slice = val.as_slice();

	if le {
		LittleEndian::read_i64(slice)
	} else {
		BigEndian::read_i64(slice)
	}
}

pub fn u64(le: bool, input: &mut Vec<u8>) -> u64 {
	let val: Vec<u8> = input.drain(0..SIZE_64).collect();
	let slice = val.as_slice();

	if le {
		LittleEndian::read_u64(slice)
	} else {
		BigEndian::read_u64(slice)
	}
}

// TODO
/*pub fn isize(le: bool, input: &mut Vec<u8>) -> isize {

}

pub fn usize(le: bool, input: &mut Vec<u8>) -> usize {

}*/

pub fn f32(le: bool, input: &mut Vec<u8>) -> f32 {
	let val: Vec<u8> = input.drain(0..SIZE_32).collect();
	let slice = val.as_slice();

	if le {
		LittleEndian::read_f32(slice)
	} else {
		BigEndian::read_f32(slice)
	}
}

pub fn f64(le: bool, input: &mut Vec<u8>) -> f64 {
	let val: Vec<u8> = input.drain(0..SIZE_64).collect();
	let slice = val.as_slice();

	if le {
		LittleEndian::read_f64(slice)
	} else {
		BigEndian::read_f64(slice)
	}
}

pub fn bool(_le: bool, input: &mut Vec<u8>) -> bool {
	input.remove(0) == 1
}

// TODO: DRY
pub fn char(_le: bool, input: &mut Vec<u8>) -> char {
	let chr = {
        // Cannot actually cause undefined behavior.
        let slice = unsafe { str::from_utf8_unchecked(&input[..]) };

        if let Some(c) = slice.chars().next() {
            Some(c)
        } else {
            None
        }
    };

    match chr {
        Some(chr) => {
			input.drain(0..chr.len_utf8());
            chr
        }
        None => panic!("Could not decode char")
    }
}
