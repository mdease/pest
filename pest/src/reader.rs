use byteorder::{ByteOrder, BigEndian, LittleEndian};
use core::str;

static SIZE_8: usize = 1;
static SIZE_16: usize = 2;
static SIZE_32: usize = 4;
static SIZE_64: usize = 8;
static SIZE_BOOL: usize = 1;

// TODO: char, usize, isize
pub fn size(ty: &str) -> usize {
	let size = env!("AVID_ARCH_SIZE");
	let mut arch_size = SIZE_32;

	if size == "64" {
		arch_size = SIZE_64;
	}

	match ty {
		"i8" => SIZE_8,
		"u8" => SIZE_8,
		"i16" => SIZE_16,
		"u16" => SIZE_16,
		"i32" => SIZE_32,
		"u32" => SIZE_32,
		"i64" => SIZE_64,
		"u64" => SIZE_64,
		"f32" => SIZE_32,
		"f64" => SIZE_64,
		"bool" => SIZE_BOOL,
		"usize" => arch_size,
		"isize" => arch_size,
		_ => 0
	}
}

pub fn is_little_endian() -> bool {
	let endianness = env!("AVID_ENDIANNESS");

	if endianness == "LE" {
		true
	} else {
		false
	}
}

pub fn i8(_le: bool, offset: &mut usize, input: &[u8]) -> i8 {
	let data = input[*offset];
	*offset += 1;
	data as i8
}

pub fn u8(_le: bool, offset: &mut usize, input: &[u8]) -> u8 {
	let data = input[*offset];
	*offset += 1;
	data
}

pub fn i16(_le: bool, offset: &mut usize, input: &[u8]) -> i16 {
	let slice = &input[*offset..*offset + SIZE_16];
	*offset += SIZE_16;

	if is_little_endian() {
		LittleEndian::read_i16(&slice)
	} else {
		BigEndian::read_i16(&slice)
	}
}

pub fn u16(_le: bool, offset: &mut usize, input: &[u8]) -> u16 {
	let slice = &input[*offset..*offset + SIZE_16];
	*offset += SIZE_16;

	if is_little_endian() {
		LittleEndian::read_u16(&slice)
	} else {
		BigEndian::read_u16(&slice)
	}
}

pub fn i32(_le: bool, offset: &mut usize, input: &[u8]) -> i32 {
	let slice = &input[*offset..*offset + SIZE_32];
	*offset += SIZE_32;

	if is_little_endian() {
		LittleEndian::read_i32(&slice)
	} else {
		BigEndian::read_i32(&slice)
	}
}

pub fn u32(_le: bool, offset: &mut usize, input: &[u8]) -> u32 {
	let slice = &input[*offset..*offset + SIZE_32];
	*offset += SIZE_32;

	if is_little_endian() {
		LittleEndian::read_u32(&slice)
	} else {
		BigEndian::read_u32(&slice)
	}
}

pub fn i64(_le: bool, offset: &mut usize, input: &[u8]) -> i64 {
	let slice = &input[*offset..*offset + SIZE_64];
	*offset += SIZE_64;

	if is_little_endian() {
		LittleEndian::read_i64(&slice)
	} else {
		BigEndian::read_i64(&slice)
	}
}

pub fn u64(_le: bool, offset: &mut usize, input: &[u8]) -> u64 {
	let slice = &input[*offset..*offset + SIZE_64];
	*offset += SIZE_64;

	if is_little_endian() {
		LittleEndian::read_u64(&slice)
	} else {
		BigEndian::read_u64(&slice)
	}
}

pub fn isize(_le: bool, offset: &mut usize, input: &[u8]) -> isize {
	let size = env!("AVID_ARCH_SIZE");

	// Assume either 32- or 64-bit
	if size == "64" {
		let slice = &input[*offset..*offset + SIZE_64];
		*offset += SIZE_64;

		if is_little_endian() {
			LittleEndian::read_i64(&slice) as isize
		} else {
			BigEndian::read_i64(&slice) as isize
		}
	} else {
		let slice = &input[*offset..*offset + SIZE_32];
		*offset += SIZE_32;

		if is_little_endian() {
			LittleEndian::read_i32(&slice) as isize
		} else {
			BigEndian::read_i32(&slice) as isize
		}
	}
}

pub fn usize(_le: bool, offset: &mut usize, input: &[u8]) -> usize {
	let size = env!("AVID_ARCH_SIZE");

	if size == "64" {
		let slice = &input[*offset..*offset + SIZE_64];
		*offset += SIZE_64;

		if is_little_endian() {
			LittleEndian::read_u64(&slice) as usize
		} else {
			BigEndian::read_u64(&slice) as usize
		}
	} else {
		let slice = &input[*offset..*offset + SIZE_32];
		*offset += SIZE_32;

		if is_little_endian() {
			LittleEndian::read_u32(&slice) as usize
		} else {
			BigEndian::read_u32(&slice) as usize
		}
	}
}

pub fn f32(_le: bool, offset: &mut usize, input: &[u8]) -> f32 {
	let slice = &input[*offset..*offset + SIZE_32];
	*offset += SIZE_32;

	if is_little_endian() {
		LittleEndian::read_f32(&slice)
	} else {
		BigEndian::read_f32(&slice)
	}
}

pub fn f64(_le: bool, offset: &mut usize, input: &[u8]) -> f64 {
	let slice = &input[*offset..*offset + SIZE_64];
	*offset += SIZE_64;

	if is_little_endian() {
		LittleEndian::read_f64(&slice)
	} else {
		BigEndian::read_f64(&slice)
	}
}

pub fn bool(_le: bool, offset: &mut usize, input: &[u8]) -> bool {
	let data = input[*offset];
	*offset += 1;
	data == 1
}

// TODO: DRY
pub fn char(_le: bool, offset: &mut usize, input: &[u8]) -> char {
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
			*offset += chr.len_utf8();
            chr
        }
        None => panic!("Could not decode char")
    }
}
