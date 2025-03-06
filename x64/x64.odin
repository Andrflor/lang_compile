package x64_assembler

import "core:fmt"
import "core:log"
import "core:testing"

// ===== REGISTER DEFINITIONS =====
Register64 :: enum u8 {
	RAX = 0,
	RCX = 1,
	RDX = 2,
	RBX = 3,
	RSP = 4,
	RBP = 5,
	RSI = 6,
	RDI = 7,
	R8  = 8,
	R9  = 9,
	R10 = 10,
	R11 = 11,
	R12 = 12,
	R13 = 13,
	R14 = 14,
	R15 = 15,
}

XMMRegister :: enum u8 {
	XMM0 = 0,
	XMM1,
	XMM2,
	XMM3,
	XMM4,
	XMM5,
	XMM6,
	XMM7,
	XMM8,
	XMM9,
	XMM10,
	XMM11,
	XMM12,
	XMM13,
	XMM14,
	XMM15,
	XMM16,
	XMM17,
	XMM18,
	XMM19,
	XMM20,
	XMM21,
	XMM22,
	XMM23,
	XMM24,
	XMM25,
	XMM26,
	XMM27,
	XMM28,
	XMM29,
	XMM30,
	XMM31,
}

x87Register :: enum u8 {
	ST0 = 0,
	ST1,
	ST2,
	ST3,
	ST4,
	ST5,
	ST6,
	ST7,
}

ByteBuffer :: struct {
	data: []u8,
	len:  int,
	cap:  int,
}

_buffer := ByteBuffer{}

_grow :: proc(min_size: int) {
	new_cap := _buffer.cap
	for new_cap < min_size {
		new_cap = new_cap * 2
		if new_cap == 0 {
			new_cap = 16
		}
	}
	new_data := make([]u8, new_cap)
	copy(new_data, _buffer.data)
	_buffer.data = new_data
	_buffer.cap = new_cap
}

_write :: proc(bytes: []u8) {
	required_size := _buffer.len + len(bytes)
	if required_size > _buffer.cap {
		_grow(required_size)
	}
	copy(_buffer.data[_buffer.len:_buffer.len + len(bytes)], bytes)
	_buffer.len += len(bytes)
}

resetBuffer :: proc() {
	_buffer.len = 0
}

// ===== DATA MOVEMENT INSTRUCTIONS =====
// --- General Purpose Register Movement ---
mov_r64_imm64 :: proc(reg: Register64, imm: u64) {
	// REX.W + B8+rd io (48 B8+ rd io)
	_write(
		[]u8 {
			0x48 + (u8(reg) >> 3), // REX.W + extension bit for register
			0xB8 + (u8(reg) & 0x7), // Opcode + register

			// Store immediate value (little-endian)
			u8(imm & 0xFF),
			u8((imm >> 8) & 0xFF),
			u8((imm >> 16) & 0xFF),
			u8((imm >> 24) & 0xFF),
			u8((imm >> 32) & 0xFF),
			u8((imm >> 40) & 0xFF),
			u8((imm >> 48) & 0xFF),
			u8((imm >> 56) & 0xFF),
		},
	)
}

mov_r64_r64 :: proc(dst: Register64, src: Register64) {
	// REX.W + 89 /r (48 89 /r)
	_write(
		[]u8 {
			0x48 + ((u8(src) & 0x8) >> 1) + ((u8(dst) & 0x8) >> 3), // REX.W + register extensions
			0x89, // Opcode
			0xC0 + ((u8(src) & 0x7) << 3) + (u8(dst) & 0x7), // ModR/M byte
		},
	)
}

mov_r64_m64 :: proc(dst: Register64, mem_addr: u64) {
	// REX.W + 8B /r (48 8B /r)
	_write(
		[]u8 {
			0x48 + ((u8(dst) & 0x8) >> 1), // REX.W + register extension
			0x8B, // Opcode
			0x04 + ((u8(dst) & 0x7) << 3), // ModR/M byte with SIB
			0x25, // SIB byte for absolute addressing

			// Store address (little-endian, 32-bit)
			u8(mem_addr & 0xFF),
			u8((mem_addr >> 8) & 0xFF),
			u8((mem_addr >> 16) & 0xFF),
			u8((mem_addr >> 24) & 0xFF),
		},
	)
}

mov_m64_r64 :: proc(mem_addr: u64, src: Register64) {
	// REX.W + 89 /r (48 89 /r)
	_write(
		[]u8 {
			0x48 + ((u8(src) & 0x8) >> 1), // REX.W + register extension
			0x89, // Opcode
			0x04 + ((u8(src) & 0x7) << 3), // ModR/M byte with SIB
			0x25, // SIB byte for absolute addressing

			// Store address (little-endian, 32-bit)
			u8(mem_addr & 0xFF),
			u8((mem_addr >> 8) & 0xFF),
			u8((mem_addr >> 16) & 0xFF),
			u8((mem_addr >> 24) & 0xFF),
		},
	)
}

movabs_r64_imm64 :: proc(reg: Register64, imm: u64) {
	// This is the same as mov_r64_imm64 for 64-bit mode
	mov_r64_imm64(reg, imm)
}

xchg_r64_r64 :: proc(dst: Register64, src: Register64) {
	// REX.W + 87 /r (48 87 /r)
	_write(
		[]u8 {
			0x48 + ((u8(src) & 0x8) >> 1) + ((u8(dst) & 0x8) >> 3), // REX.W + register extensions
			0x87, // Opcode
			0xC0 + ((u8(src) & 0x7) << 3) + (u8(dst) & 0x7), // ModR/M byte
		},
	)
}

// --- Sign and Zero Extensions ---
movsx_r64_r32 :: proc(dst: Register64, src: Register64) {
	// REX.W + 63 /r (48 63 /r)
	_write(
		[]u8 {
			0x48 + ((u8(dst) & 0x8) >> 1) + ((u8(src) & 0x8) >> 3), // REX.W + register extensions
			0x63, // Opcode
			0xC0 + ((u8(dst) & 0x7) << 3) + (u8(src) & 0x7), // ModR/M byte
		},
	)
}


// --- Byte Order Control ---
movbe_r64_m64 :: proc(dst: Register64, mem_addr: u64) {
	// REX.W + 0F 38 F0 /r (48 0F 38 F0 /r)
	_write(
		[]u8 {
			0x48 + ((u8(dst) & 0x8) >> 1), // REX.W + register extension
			0x0F, // Two-byte opcode prefix
			0x38, // Three-byte opcode prefix
			0xF0, // Opcode
			0x04 + ((u8(dst) & 0x7) << 3), // ModR/M byte with SIB
			0x25, // SIB byte for absolute addressing

			// Store address (little-endian, 32-bit)
			u8(mem_addr & 0xFF),
			u8((mem_addr >> 8) & 0xFF),
			u8((mem_addr >> 16) & 0xFF),
			u8((mem_addr >> 24) & 0xFF),
		},
	)
}

movbe_m64_r64 :: proc(mem_addr: u64, src: Register64) {
	// REX.W + 0F 38 F1 /r (48 0F 38 F1 /r)
	_write(
		[]u8 {
			0x48 + ((u8(src) & 0x8) >> 1), // REX.W + register extension
			0x0F, // Two-byte opcode prefix
			0x38, // Three-byte opcode prefix
			0xF1, // Opcode
			0x04 + ((u8(src) & 0x7) << 3), // ModR/M byte with SIB
			0x25, // SIB byte for absolute addressing

			// Store address (little-endian, 32-bit)
			u8(mem_addr & 0xFF),
			u8((mem_addr >> 8) & 0xFF),
			u8((mem_addr >> 16) & 0xFF),
			u8((mem_addr >> 24) & 0xFF),
		},
	)
}

bswap_r64 :: proc(reg: Register64) { 	// Byte swap
	// REX.W + 0F C8+rd (48 0F C8+rd)
	_write(
		[]u8 {
			0x48 + ((u8(reg) & 0x8) >> 3), // REX.W + register extension
			0x0F, // Two-byte opcode prefix
			0xC8 + (u8(reg) & 0x7), // Opcode + register
		},
	)
}


mov_r64_cr :: proc(reg: Register64, cr: u8) {
	// 0F 20 /r - MOV r64, CR0-CR7
	_write(
		[]u8 {
			0x0F, // Two-byte opcode prefix
			0x20, // Opcode
			0xC0 | (u8(reg) << 3) | (cr & 0x7), // ModR/M byte: mod=11, reg=r64, rm=cr
		},
	)
}

mov_cr_r64 :: proc(cr: u8, reg: Register64) {
	// 0F 22 /r - MOV CR0-CR7, r64
	_write(
		[]u8 {
			0x0F, // Two-byte opcode prefix
			0x22, // Opcode
			0xC0 | (cr << 3) | (u8(reg) & 0x7), // ModR/M byte: mod=11, reg=cr, rm=r64
		},
	)
}

mov_cr0_r64 :: proc(reg: Register64) {
	mov_cr_r64(0, reg)
}

mov_cr2_r64 :: proc(reg: Register64) {
	mov_cr_r64(2, reg)
}

mov_cr3_r64 :: proc(reg: Register64) {
	mov_cr_r64(3, reg)
}

mov_cr4_r64 :: proc(reg: Register64) {
	mov_cr_r64(4, reg)
}

// --- Debug Registers ---
// MOV r64, DR# (Read from debug register)
mov_r64_dr :: proc(reg: Register64, dr: u8) {
	// 0F 21 /r
	_write(
		[]u8 {
			0x0F, // Two-byte opcode prefix
			0x21, // Opcode
			0xC0 | (u8(reg) & 0x7) | (dr << 3), // ModR/M byte: mod=11, reg=dr, rm=r64
		},
	)
}

mov_dr_r64 :: proc(dr: u8, reg: Register64) {
	rex: u8 = 0

	if (u8(reg) & 0x8) != 0 { 	// If using R8-R15, set REX.B
		rex |= 0x41 // REX.B
	}

	if (dr & 0x8) != 0 { 	// If using DR8-DR15, set REX.R
		rex |= 0x44 // REX.R
	}

	if rex != 0 {
		_write([]u8{0x40 | rex, 0x0F, 0x23, 0xC0 | ((dr & 0x7) << 3) | (u8(reg) & 0x7)})
	}
	_write([]u8{0x0F, 0x23, 0xC0 | ((dr & 0x7) << 3) | (u8(reg) & 0x7)})
}


// Helper functions for debug registers
mov_dr0_r64 :: proc(reg: Register64) {
	mov_dr_r64(0, reg)
}

mov_dr1_r64 :: proc(reg: Register64) {
	mov_dr_r64(1, reg)
}

mov_dr2_r64 :: proc(reg: Register64) {
	mov_dr_r64(2, reg)
}

mov_dr3_r64 :: proc(reg: Register64) {
	mov_dr_r64(3, reg)
}

mov_dr6_r64 :: proc(reg: Register64) {
	mov_dr_r64(6, reg)
}

mov_dr7_r64 :: proc(reg: Register64) {
	mov_dr_r64(7, reg)
}
// --- Address Manipulation ---
lea_r64_m64 :: proc(dst: Register64, mem_addr: u64) {
	// REX.W + 8D /r (48 8D /r)
	_write(
		[]u8 {
			0x48 + ((u8(dst) & 0x8) >> 1), // REX.W + register extension
			0x8D, // Opcode
			0x04 + ((u8(dst) & 0x7) << 3), // ModR/M byte with SIB
			0x25, // SIB byte for absolute addressing

			// Store address (little-endian, 32-bit)
			u8(mem_addr & 0xFF),
			u8((mem_addr >> 8) & 0xFF),
			u8((mem_addr >> 16) & 0xFF),
			u8((mem_addr >> 24) & 0xFF),
		},
	)
}

// ===== ARITHMETIC INSTRUCTIONS =====
// --- Basic Arithmetic ---
add_r64_imm32 :: proc(reg: Register64, imm: u32) {
	// REX.W + 81 /0 id (48 81 /0 id)
	_write(
		[]u8 {
			0x48 + ((u8(reg) & 0x8) >> 3), // REX.W + register extension
			0x81, // Opcode
			0xC0 + (u8(reg) & 0x7), // ModR/M byte (/0 = opcode extension 0 for ADD)
			// Immediate (little-endian)
			u8(imm & 0xFF),
			u8((imm >> 8) & 0xFF),
			u8((imm >> 16) & 0xFF),
			u8((imm >> 24) & 0xFF),
		},
	)
}

add_r64_r64 :: proc(dst: Register64, src: Register64) {
	// REX.W + 01 /r (48 01 /r)
	_write(
		[]u8 {
			0x48 + ((u8(src) & 0x8) >> 1) + ((u8(dst) & 0x8) >> 3), // REX.W + register extensions
			0x01, // Opcode
			0xC0 + ((u8(src) & 0x7) << 3) + (u8(dst) & 0x7), // ModR/M byte
		},
	)
}

sub_r64_imm32 :: proc(reg: Register64, imm: u32) {
	// REX.W + 81 /5 id (48 81 /5 id)
	_write(
		[]u8 {
			0x48 + ((u8(reg) & 0x8) >> 3), // REX.W + register extension
			0x81, // Opcode
			0xE8 + (u8(reg) & 0x7), // ModR/M byte (/5 = opcode extension 5 for SUB)
			// Immediate (little-endian)
			u8(imm & 0xFF),
			u8((imm >> 8) & 0xFF),
			u8((imm >> 16) & 0xFF),
			u8((imm >> 24) & 0xFF),
		},
	)
}

sub_r64_r64 :: proc(dst: Register64, src: Register64) {
	// REX.W + 29 /r (48 29 /r)
	_write(
		[]u8 {
			0x48 + ((u8(src) & 0x8) >> 1) + ((u8(dst) & 0x8) >> 3), // REX.W + register extensions
			0x29, // Opcode
			0xC0 + ((u8(src) & 0x7) << 3) + (u8(dst) & 0x7), // ModR/M byte
		},
	)
}

inc_r64 :: proc(reg: Register64) {
	// REX.W + FF /0 (48 FF /0)
	_write(
		[]u8 {
			0x48 + ((u8(reg) & 0x8) >> 3), // REX.W + register extension
			0xFF, // Opcode
			0xC0 + (u8(reg) & 0x7), // ModR/M byte (/0 = opcode extension 0 for INC)
		},
	)
}

dec_r64 :: proc(reg: Register64) {
	// REX.W + FF /1 (48 FF /1)
	_write([]u8{0x48 + ((u8(reg) & 0x8) >> 3), 0xFF, 0xC8 + (u8(reg) & 0x7)})
}

neg_r64 :: proc(reg: Register64) {
	// REX.W + F7 /3 (48 F7 /3)
	_write([]u8{0x48 + ((u8(reg) & 0x8) >> 3), 0xF7, 0xD8 + (u8(reg) & 0x7)})
}

// --- Advanced Arithmetic ---
adc_r64_r64 :: proc(dst: Register64, src: Register64) { 	// Add with carry
	// REX.W + 11 /r (48 11 /r)
	_write(
		[]u8 {
			0x48 + ((u8(src) & 0x8) >> 1) + ((u8(dst) & 0x8) >> 3),
			0x11, // Opcode
			0xC0 + ((u8(src) & 0x7) << 3) + (u8(dst) & 0x7), // ModR/M byte
		},
	)
}

sbb_r64_r64 :: proc(dst: Register64, src: Register64) { 	// Subtract with borrow
	// REX.W + 19 /r (48 19 /r)
	_write(
		[]u8 {
			0x48 + ((u8(src) & 0x8) >> 1) + ((u8(dst) & 0x8) >> 3),
			0x19, // Opcode
			0xC0 + ((u8(src) & 0x7) << 3) + (u8(dst) & 0x7),
		},
	)
}

xadd_r64_r64 :: proc(dst: Register64, src: Register64) { 	// Exchange and add
	// REX.W + 0F C1 /r (48 0F C1 /r)
	_write(
		[]u8 {
			0x48 + ((u8(src) & 0x8) >> 1) + ((u8(dst) & 0x8) >> 3),
			0x0F, // Two-byte opcode prefix
			0xC1, // Opcode
			0xC0 + ((u8(src) & 0x7) << 3) + (u8(dst) & 0x7), // ModR/M byte
		},
	)
}

// --- Multiplication and Division ---
mul_r64 :: proc(reg: Register64) { 	// Unsigned multiply
	// REX.W + F7 /4 (48 F7 /4)
	_write([]u8{0x48 + ((u8(reg) & 0x8) >> 3), 0xF7, 0xE0 + (u8(reg) & 0x7)})
}

imul_r64_r64 :: proc(dst: Register64, src: Register64) { 	// Signed multiply
	// REX.W + 0F AF /r (48 0F AF /r)
	// Corrected: destination is in the reg field and source in r/m field.
	_write(
		[]u8 {
			0x48 + ((u8(src) & 0x8) >> 1) + ((u8(dst) & 0x8) >> 3),
			0x0F, // Two-byte opcode prefix
			0xAF, // Opcode
			0xC0 + ((u8(dst) & 0x7) << 3) + (u8(src) & 0x7), // ModR/M byte
		},
	)
}

imul_r64_r64_imm32 :: proc(dst: Register64, src: Register64, imm: u32) {
	// REX.W + 69 /r id (48 69 /r id)
	// Corrected REX prefix order (same as mov_r64_r64) and immediate encoding.
	_write(
		[]u8 {
			0x48 + ((u8(src) & 0x8) >> 1) + ((u8(dst) & 0x8) >> 3),
			0x69, // Opcode
			0xC0 + ((u8(dst) & 0x7) << 3) + (u8(src) & 0x7),
			u8(imm & 0xFF),
			u8((imm >> 8) & 0xFF),
			u8((imm >> 16) & 0xFF),
			u8((imm >> 24) & 0xFF),
		},
	)
}

imul_r64_imm32 :: proc(reg: Register64, imm: u32) {
	// This is the same as imul_r64_r64_imm32 where dst and src are the same
	imul_r64_r64_imm32(reg, reg, imm)
}

div_r64 :: proc(reg: Register64) { 	// Unsigned divide
	// REX.W + F7 /6 (48 F7 /6)
	_write([]u8{0x48 + ((u8(reg) & 0x8) >> 3), 0xF7, 0xF0 + (u8(reg) & 0x7)})
}

idiv_r64 :: proc(reg: Register64) { 	// Signed divide
	// REX.W + F7 /7 (48 F7 /7)
	_write(
		[]u8 {
			0x48 + ((u8(reg) & 0x8) >> 3),
			0xF7,
			0xF8 + (u8(reg) & 0x7), // ModR/M byte for signed divide
		},
	)
}

// ===== BITWISE INSTRUCTIONS =====

and_r64_r64 :: proc(dst: Register64, src: Register64) {
	_write(
		[]u8 {
			0x48 + ((u8(src) & 0x8) >> 1) + ((u8(dst) & 0x8) >> 3),
			0x21, // Opcode for AND
			0xC0 + ((u8(src) & 0x7) << 3) + (u8(dst) & 0x7),
		},
	)
}

or_r64_r64 :: proc(dst: Register64, src: Register64) {
	_write(
		[]u8 {
			0x48 + ((u8(src) & 0x8) >> 1) + ((u8(dst) & 0x8) >> 3),
			0x09, // Opcode for OR
			0xC0 + ((u8(src) & 0x7) << 3) + (u8(dst) & 0x7),
		},
	)
}

xor_r64_r64 :: proc(dst: Register64, src: Register64) {
	_write(
		[]u8 {
			0x48 + ((u8(src) & 0x8) >> 1) + ((u8(dst) & 0x8) >> 3),
			0x31, // Opcode for XOR
			0xC0 + ((u8(src) & 0x7) << 3) + (u8(dst) & 0x7),
		},
	)
}

not_r64 :: proc(reg: Register64) {
	_write([]u8{0x48 + ((u8(reg) & 0x8) >> 3), 0xF7, 0xD0 + (u8(reg) & 0x7)})
}
// --- Shifts and Rotates ---
// ===== SHIFT AND ROTATE INSTRUCTIONS IMPLEMENTATION =====
shl_r64_imm8 :: proc(reg: Register64, imm: u8) {
	_write([]u8{0x48 + ((u8(reg) & 0x8) >> 3), 0xC1, 0xE0 + (u8(reg) & 0x7), imm})
}

shr_r64_imm8 :: proc(reg: Register64, imm: u8) {
	_write([]u8{0x48 + ((u8(reg) & 0x8) >> 3), 0xC1, 0xE8 + (u8(reg) & 0x7), imm})
}

rol_r64_imm8 :: proc(reg: Register64, imm: u8) {
	_write([]u8{0x48 + ((u8(reg) & 0x8) >> 3), 0xC1, 0xC0 + (u8(reg) & 0x7), imm})
}

ror_r64_imm8 :: proc(reg: Register64, imm: u8) {
	_write([]u8{0x48 + ((u8(reg) & 0x8) >> 3), 0xC1, 0xC8 + (u8(reg) & 0x7), imm})
}

// --- Bit Manipulation ---
shld_r64_r64_imm8 :: proc(dst: Register64, src: Register64, imm: u8) {
	_write(
		[]u8 {
			0x48 + ((u8(src) & 0x8) >> 1) + ((u8(dst) & 0x8) >> 3),
			0x0F,
			0xA4,
			0xC0 + ((u8(src) & 0x7) << 3) + (u8(dst) & 0x7),
			imm,
		},
	)
}

shrd_r64_r64_imm8 :: proc(dst: Register64, src: Register64, imm: u8) {
	_write(
		[]u8 {
			0x48 + ((u8(src) & 0x8) >> 1) + ((u8(dst) & 0x8) >> 3),
			0x0F,
			0xAC,
			0xC0 + ((u8(src) & 0x7) << 3) + (u8(dst) & 0x7),
			imm,
		},
	)
}

bt_r64_r64 :: proc(reg: Register64, bit_index: Register64) {
	_write(
		[]u8 {
			0x48 + ((u8(bit_index) & 0x8) >> 1) + ((u8(reg) & 0x8) >> 3),
			0x0F,
			0xA3,
			0xC0 + ((u8(bit_index) & 0x7) << 3) + (u8(reg) & 0x7),
		},
	)
}

bts_r64_r64 :: proc(reg: Register64, bit_index: Register64) {
	_write(
		[]u8 {
			0x48 + ((u8(bit_index) & 0x8) >> 1) + ((u8(reg) & 0x8) >> 3),
			0x0F,
			0xAB,
			0xC0 + ((u8(bit_index) & 0x7) << 3) + (u8(reg) & 0x7),
		},
	)
}

btr_r64_r64 :: proc(reg: Register64, bit_index: Register64) {
	_write(
		[]u8 {
			0x48 + ((u8(bit_index) & 0x8) >> 1) + ((u8(reg) & 0x8) >> 3),
			0x0F,
			0xB3,
			0xC0 + ((u8(bit_index) & 0x7) << 3) + (u8(reg) & 0x7),
		},
	)
}

btc_r64_r64 :: proc(reg: Register64, bit_index: Register64) {
	_write(
		[]u8 {
			0x48 + ((u8(bit_index) & 0x8) >> 1) + ((u8(reg) & 0x8) >> 3),
			0x0F,
			0xBB,
			0xC0 + ((u8(bit_index) & 0x7) << 3) + (u8(reg) & 0x7),
		},
	)
}

bsf_r64_r64 :: proc(dst: Register64, src: Register64) {
	_write(
		[]u8 {
			0x48 + ((u8(dst) & 0x8) >> 1) + ((u8(src) & 0x8) >> 3),
			0x0F,
			0xBC,
			0xC0 + ((u8(dst) & 0x7) << 3) + (u8(src) & 0x7),
		},
	)
}

bsr_r64_r64 :: proc(dst: Register64, src: Register64) {
	_write(
		[]u8 {
			0x48 + ((u8(dst) & 0x8) >> 1) + ((u8(src) & 0x8) >> 3),
			0x0F,
			0xBD,
			0xC0 + ((u8(dst) & 0x7) << 3) + (u8(src) & 0x7),
		},
	)
}

popcnt_r64_r64 :: proc(dst: Register64, src: Register64) {
	_write(
		[]u8 {
			0x48 + ((u8(dst) & 0x8) >> 1) + ((u8(src) & 0x8) >> 3),
			0xF3, // POPCNT requires F3 prefix
			0x0F,
			0xB8,
			0xC0 + ((u8(dst) & 0x7) << 3) + (u8(src) & 0x7),
		},
	)
}

lzcnt_r64_r64 :: proc(dst: Register64, src: Register64) {
	_write(
		[]u8 {
			0x48 + ((u8(dst) & 0x8) >> 1) + ((u8(src) & 0x8) >> 3),
			0xF3, // LZCNT requires F3 prefix
			0x0F,
			0xBD,
			0xC0 + ((u8(dst) & 0x7) << 3) + (u8(src) & 0x7),
		},
	)
}

tzcnt_r64_r64 :: proc(dst: Register64, src: Register64) {
	_write(
		[]u8 {
			0x48 + ((u8(dst) & 0x8) >> 1) + ((u8(src) & 0x8) >> 3),
			0xF3, // TZCNT requires F3 prefix
			0x0F,
			0xBC,
			0xC0 + ((u8(dst) & 0x7) << 3) + (u8(src) & 0x7),
		},
	)
}

pext_r64_r64_r64 :: proc(dst: Register64, src1: Register64, src2: Register64) {
	_write(
		[]u8 {
			0xC4, // VEX prefix
			0xE2 | ((~u8(dst) & 0x8) << 4) | ((~u8(src2) & 0x8) << 3),
			0xF8 | ((~u8(src1) & 0x8) >> 3),
			0xF5,
			0xC0 + ((u8(dst) & 0x7) << 3) + (u8(src2) & 0x7),
			u8(src1) & 0x7,
		},
	)
}

pdep_r64_r64_r64 :: proc(dst: Register64, src1: Register64, src2: Register64) {
	_write(
		[]u8 {
			0xC4, // VEX prefix
			0xE2 | ((~u8(dst) & 0x8) << 4) | ((~u8(src2) & 0x8) << 3),
			0xF8 | ((~u8(src1) & 0x8) >> 3),
			0xF5,
			0xC0 + ((u8(dst) & 0x7) << 3) + (u8(src2) & 0x7),
			u8(src1) & 0x7,
		},
	)
}

// ===== COMPARISON INSTRUCTIONS =====
cmp_r64_r64 :: proc(reg1: Register64, reg2: Register64) {
	_write(
		[]u8 {
			0x48 + ((u8(reg2) & 0x8) >> 1) + ((u8(reg1) & 0x8) >> 3),
			0x39,
			0xC0 + ((u8(reg2) & 0x7) << 3) + (u8(reg1) & 0x7),
		},
	)
}

cmp_r64_imm32 :: proc(reg: Register64, imm: u32) {
	_write(
		[]u8 {
			0x48 + ((u8(reg) & 0x8) >> 3),
			0x81,
			0xF8 + (u8(reg) & 0x7),
			u8(imm & 0xFF),
			u8((imm >> 8) & 0xFF),
			u8((imm >> 16) & 0xFF),
			u8((imm >> 24) & 0xFF),
		},
	)
}

test_r64_r64 :: proc(reg1: Register64, reg2: Register64) {
	_write(
		[]u8 {
			0x48 + ((u8(reg2) & 0x8) >> 1) + ((u8(reg1) & 0x8) >> 3),
			0x85,
			0xC0 + ((u8(reg2) & 0x7) << 3) + (u8(reg1) & 0x7),
		},
	)
}

test_r64_imm32 :: proc(reg: Register64, imm: u32) {
	_write(
		[]u8 {
			0x48 + ((u8(reg) & 0x8) >> 3),
			0xF7,
			0xC0 + (u8(reg) & 0x7),
			u8(imm & 0xFF),
			u8((imm >> 8) & 0xFF),
			u8((imm >> 16) & 0xFF),
			u8((imm >> 24) & 0xFF),
		},
	)
}

// --- Conditional Moves ---
cmove_r64_r64 :: proc(dst: Register64, src: Register64) {
	_write(
		[]u8 {
			0x48 + ((u8(dst) & 0x8) >> 1) + ((u8(src) & 0x8) >> 3),
			0x0F,
			0x44,
			0xC0 + ((u8(dst) & 0x7) << 3) + (u8(src) & 0x7),
		},
	)
}

cmovne_r64_r64 :: proc(dst: Register64, src: Register64) {
	_write(
		[]u8 {
			0x48 + ((u8(dst) & 0x8) >> 1) + ((u8(src) & 0x8) >> 3),
			0x0F,
			0x45,
			0xC0 + ((u8(dst) & 0x7) << 3) + (u8(src) & 0x7),
		},
	)
}

cmova_r64_r64 :: proc(dst: Register64, src: Register64) {
	_write(
		[]u8 {
			0x48 + ((u8(dst) & 0x8) >> 1) + ((u8(src) & 0x8) >> 3),
			0x0F,
			0x47,
			0xC0 + ((u8(dst) & 0x7) << 3) + (u8(src) & 0x7),
		},
	)
}

cmovae_r64_r64 :: proc(dst: Register64, src: Register64) {
	_write(
		[]u8 {
			0x48 + ((u8(dst) & 0x8) >> 1) + ((u8(src) & 0x8) >> 3),
			0x0F,
			0x43,
			0xC0 + ((u8(dst) & 0x7) << 3) + (u8(src) & 0x7),
		},
	)
}

cmovb_r64_r64 :: proc(dst: Register64, src: Register64) {
	_write(
		[]u8 {
			0x48 + ((u8(dst) & 0x8) >> 1) + ((u8(src) & 0x8) >> 3),
			0x0F,
			0x42,
			0xC0 + ((u8(dst) & 0x7) << 3) + (u8(src) & 0x7),
		},
	)
}

cmovbe_r64_r64 :: proc(dst: Register64, src: Register64) {
	_write(
		[]u8 {
			0x48 + ((u8(dst) & 0x8) >> 1) + ((u8(src) & 0x8) >> 3),
			0x0F,
			0x46,
			0xC0 + ((u8(dst) & 0x7) << 3) + (u8(src) & 0x7),
		},
	)
}

// ===== CONTROL FLOW INSTRUCTIONS =====
// --- Unconditional Jumps & Calls ---
jmp_rel32 :: proc(offset: i32) {
	_write(
		[]u8 {
			0xE9,
			u8(offset & 0xFF),
			u8((offset >> 8) & 0xFF),
			u8((offset >> 16) & 0xFF),
			u8((offset >> 24) & 0xFF),
		},
	)
}

jmp_r64 :: proc(reg: Register64) {
	_write([]u8{0xFF, 0xE0 + (u8(reg) & 0x7)})
}

jmp_m64 :: proc(mem_addr: u64) {
	_write(
		[]u8 {
			0xFF,
			0x25,
			u8(mem_addr & 0xFF),
			u8((mem_addr >> 8) & 0xFF),
			u8((mem_addr >> 16) & 0xFF),
			u8((mem_addr >> 24) & 0xFF),
		},
	)
}

call_rel32 :: proc(offset: i32) {
	_write(
		[]u8 {
			0xE8,
			u8(offset & 0xFF),
			u8((offset >> 8) & 0xFF),
			u8((offset >> 16) & 0xFF),
			u8((offset >> 24) & 0xFF),
		},
	)
}

call_r64 :: proc(reg: Register64) {
	_write([]u8{0xFF, 0xD0 + (u8(reg) & 0x7)})
}

call_m64 :: proc(mem_addr: u64) {
	_write(
		[]u8 {
			0xFF,
			0x15,
			u8(mem_addr & 0xFF),
			u8((mem_addr >> 8) & 0xFF),
			u8((mem_addr >> 16) & 0xFF),
			u8((mem_addr >> 24) & 0xFF),
		},
	)
}

ret :: proc() {
	_write([]u8{0xC3})
}

// --- Conditional Jumps ---
je_rel32 :: proc(offset: i32) {
	_write(
		[]u8 {
			0x0F,
			0x84,
			u8(offset & 0xFF),
			u8((offset >> 8) & 0xFF),
			u8((offset >> 16) & 0xFF),
			u8((offset >> 24) & 0xFF),
		},
	)
}

jne_rel32 :: proc(offset: i32) {
	_write(
		[]u8 {
			0x0F,
			0x85,
			u8(offset & 0xFF),
			u8((offset >> 8) & 0xFF),
			u8((offset >> 16) & 0xFF),
			u8((offset >> 24) & 0xFF),
		},
	)
}

jg_rel32 :: proc(offset: i32) {
	_write(
		[]u8 {
			0x0F,
			0x8F,
			u8(offset & 0xFF),
			u8((offset >> 8) & 0xFF),
			u8((offset >> 16) & 0xFF),
			u8((offset >> 24) & 0xFF),
		},
	)
}

jl_rel32 :: proc(offset: i32) {
	_write(
		[]u8 {
			0x0F,
			0x8C,
			u8(offset & 0xFF),
			u8((offset >> 8) & 0xFF),
			u8((offset >> 16) & 0xFF),
			u8((offset >> 24) & 0xFF),
		},
	)
}

jge_rel32 :: proc(offset: i32) {
	_write(
		[]u8 {
			0x0F,
			0x8D,
			u8(offset & 0xFF),
			u8((offset >> 8) & 0xFF),
			u8((offset >> 16) & 0xFF),
			u8((offset >> 24) & 0xFF),
		},
	)
}

jle_rel32 :: proc(offset: i32) {
	_write(
		[]u8 {
			0x0F,
			0x8E,
			u8(offset & 0xFF),
			u8((offset >> 8) & 0xFF),
			u8((offset >> 16) & 0xFF),
			u8((offset >> 24) & 0xFF),
		},
	)
}

ja_rel32 :: proc(offset: i32) {
	_write(
		[]u8 {
			0x0F,
			0x87,
			u8(offset & 0xFF),
			u8((offset >> 8) & 0xFF),
			u8((offset >> 16) & 0xFF),
			u8((offset >> 24) & 0xFF),
		},
	)
}

jae_rel32 :: proc(offset: i32) {
	_write(
		[]u8 {
			0x0F,
			0x83,
			u8(offset & 0xFF),
			u8((offset >> 8) & 0xFF),
			u8((offset >> 16) & 0xFF),
			u8((offset >> 24) & 0xFF),
		},
	)
}

jb_rel32 :: proc(offset: i32) {
	_write(
		[]u8 {
			0x0F,
			0x82,
			u8(offset & 0xFF),
			u8((offset >> 8) & 0xFF),
			u8((offset >> 16) & 0xFF),
			u8((offset >> 24) & 0xFF),
		},
	)
}

jbe_rel32 :: proc(offset: i32) {
	_write(
		[]u8 {
			0x0F,
			0x86,
			u8(offset & 0xFF),
			u8((offset >> 8) & 0xFF),
			u8((offset >> 16) & 0xFF),
			u8((offset >> 24) & 0xFF),
		},
	)
}

// --- Loop Control ---
loop_rel8 :: proc(offset: i8) {
	_write([]u8{0xE2, u8(offset)})
}

loope_rel8 :: proc(offset: i8) {
	_write([]u8{0xE1, u8(offset)})
}

loopne_rel8 :: proc(offset: i8) {
	_write([]u8{0xE0, u8(offset)})
}

jecxz_rel8 :: proc(offset: i8) {
	_write([]u8{0xE3, u8(offset)})
}

// --- Miscellaneous Control Flow ---
setcc_r8 :: proc(dst: u8, condition_code: u8) {
	_write(
		[]u8 {
			0x0F,
			0x90 + condition_code, // 0x90 is SETO, add condition code offset (0-F)
			0xC0 + dst,
		},
	)
}

endbr64 :: proc() {
	_write([]u8{0xF3, 0x0F, 0x1E, 0xFA})
}

//===== STACK MANIPULATION =====
push_r64 :: proc(reg: Register64) {
	if u8(reg) & 0x8 == 0 {
		_write([]u8{0x50 + (u8(reg) & 0x7)})
	} else {
		_write([]u8{0x41, 0x50 + (u8(reg) & 0x7)})
	}
}

pop_r64 :: proc(reg: Register64) {
	if u8(reg) & 0x8 == 0 {
		_write([]u8{0x58 + (u8(reg) & 0x7)})
	} else {
		_write([]u8{0x41, 0x58 + (u8(reg) & 0x7)})
	}
}

pushfq :: proc() {
	_write([]u8{0x9C})
}

popfq :: proc() {
	_write([]u8{0x9D})
}

// ===== SIMD INSTRUCTIONS =====
// --- SSE/AVX Data Movement ---
movd_xmm_r64 :: proc(xmm: XMMRegister, reg: Register64) {
	// 66 0F 6E /r
	rex := 0x48
	if u8(reg) & 0x8 != 0 {
		rex |= 0x41
	}
	if u8(xmm) & 0x8 != 0 {
		rex |= 0x44
	}
	_write([]u8{u8(rex), 0x66, 0x0F, 0x6E, 0xC0 | ((u8(xmm) & 0x7) << 3) | (u8(reg) & 0x7)})
}

movd_r64_xmm :: proc(reg: Register64, xmm: XMMRegister) {
	// 66 0F 7E /r
	rex := 0x48
	if u8(reg) & 0x8 != 0 {
		rex |= 0x41
	}
	if u8(xmm) & 0x8 != 0 {
		rex |= 0x44
	}
	_write([]u8{u8(rex), 0x66, 0x0F, 0x7E, 0xC0 | ((u8(xmm) & 0x7) << 3) | (u8(reg) & 0x7)})
}

movups_xmm_m128 :: proc(dst: XMMRegister, mem: u64) {
	// 0F 10 /r
	rex := 0x48
	if u8(dst) & 0x8 != 0 {
		rex |= 0x44
	}
	_write([]u8{u8(rex), 0x0F, 0x10})
	_write_modrm_sib_disp(u8(dst), mem)
}

movdqu_xmm_m128 :: proc(dst: XMMRegister, mem: u64) {
	// F3 0F 6F /r
	rex := 0x48
	if u8(dst) & 0x8 != 0 {
		rex |= 0x44
	}
	_write([]u8{u8(rex), 0xF3, 0x0F, 0x6F})
	_write_modrm_sib_disp(u8(dst), mem)
}

// --- SSE/AVX Arithmetic ---
addps_xmm_xmm :: proc(dst: XMMRegister, src: XMMRegister) {
	// 0F 58 /r
	rex := 0x40
	if u8(dst) & 0x8 != 0 {
		rex |= 0x44
	}
	if u8(src) & 0x8 != 0 {
		rex |= 0x41
	}
	if rex != 0x40 {
		_write([]u8{u8(rex), 0x0F, 0x58, 0xC0 | ((u8(dst) & 0x7) << 3) | (u8(src) & 0x7)})
	} else {
		_write([]u8{0x0F, 0x58, 0xC0 | ((u8(dst) & 0x7) << 3) | (u8(src) & 0x7)})
	}
}

mulps_xmm_xmm :: proc(dst: XMMRegister, src: XMMRegister) {
	// 0F 59 /r
	rex := 0x40
	if u8(dst) & 0x8 != 0 {
		rex |= 0x44
	}
	if u8(src) & 0x8 != 0 {
		rex |= 0x41
	}
	if rex != 0x40 {
		_write([]u8{u8(rex), 0x0F, 0x59, 0xC0 | ((u8(dst) & 0x7) << 3) | (u8(src) & 0x7)})
	} else {
		_write([]u8{0x0F, 0x59, 0xC0 | ((u8(dst) & 0x7) << 3) | (u8(src) & 0x7)})
	}
}

divps_xmm_xmm :: proc(dst: XMMRegister, src: XMMRegister) {
	// 0F 5E /r
	rex := 0x40
	if u8(dst) & 0x8 != 0 {
		rex |= 0x44
	}
	if u8(src) & 0x8 != 0 {
		rex |= 0x41
	}
	if rex != 0x40 {
		_write([]u8{u8(rex), 0x0F, 0x5E, 0xC0 | ((u8(dst) & 0x7) << 3) | (u8(src) & 0x7)})
	} else {
		_write([]u8{0x0F, 0x5E, 0xC0 | ((u8(dst) & 0x7) << 3) | (u8(src) & 0x7)})
	}
}

sqrtps_xmm :: proc(xmm: XMMRegister) {
	// 0F 51 /r
	rex := 0x40
	if u8(xmm) & 0x8 != 0 {
		rex |= 0x44
	}
	if rex != 0x40 {
		_write([]u8{u8(rex), 0x0F, 0x51, 0xC0 | ((u8(xmm) & 0x7) << 3) | (u8(xmm) & 0x7)})
	} else {
		_write([]u8{0x0F, 0x51, 0xC0 | ((u8(xmm) & 0x7) << 3) | (u8(xmm) & 0x7)})
	}
}

// --- SSE/AVX Comparison ---
cmpps_xmm_xmm_imm8 :: proc(dst: XMMRegister, src: XMMRegister, imm: u8) {
	// 0F C2 /r ib
	rex := 0x40
	if u8(dst) & 0x8 != 0 {
		rex |= 0x44
	}
	if u8(src) & 0x8 != 0 {
		rex |= 0x41
	}
	if rex != 0x40 {
		_write([]u8{u8(rex), 0x0F, 0xC2, 0xC0 | ((u8(dst) & 0x7) << 3) | (u8(src) & 0x7), imm})
	} else {
		_write([]u8{0x0F, 0xC2, 0xC0 | ((u8(dst) & 0x7) << 3) | (u8(src) & 0x7), imm})
	}
}

cmpeqps_xmm_xmm :: proc(dst: XMMRegister, src: XMMRegister) {
	// 0F C2 /r 00
	cmpps_xmm_xmm_imm8(dst, src, 0)
}

cmpneqps_xmm_xmm :: proc(dst: XMMRegister, src: XMMRegister) {
	// 0F C2 /r 04
	cmpps_xmm_xmm_imm8(dst, src, 4)
}

// --- AVX FMA (Fused Multiply-Add) ---
vfmadd132ps_xmm_xmm_xmm :: proc(dst: XMMRegister, src1: XMMRegister, src2: XMMRegister) {
	// VEX.DDS.128.66.0F38.W0 98 /r
	_write_vex_prefix(0x66, 0x0F, 0x38, u8(src1), false, false)
	_write([]u8{0x98, 0xC0 | ((u8(dst) & 0x7) << 3) | (u8(src2) & 0x7)})
}

vfmadd213ps_xmm_xmm_xmm :: proc(dst: XMMRegister, src1: XMMRegister, src2: XMMRegister) {
	// VEX.DDS.128.66.0F38.W0 A8 /r
	_write_vex_prefix(0x66, 0x0F, 0x38, u8(src1), false, false)
	_write([]u8{0xA8, 0xC0 | ((u8(dst) & 0x7) << 3) | (u8(src2) & 0x7)})
}

vfmadd231ps_xmm_xmm_xmm :: proc(dst: XMMRegister, src1: XMMRegister, src2: XMMRegister) {
	// VEX.DDS.128.66.0F38.W0 B8 /r
	_write_vex_prefix(0x66, 0x0F, 0x38, u8(src1), false, false)
	_write([]u8{0xB8, 0xC0 | ((u8(dst) & 0x7) << 3) | (u8(src2) & 0x7)})
}

// --- AVX Advanced Operations ---
vaddps_ymm :: proc(dst: XMMRegister, src1: XMMRegister, src2: XMMRegister) {
	// VEX.256.0F.WIG 58 /r
	_write_vex_prefix(0x00, 0x0F, 0x00, u8(src1), true, false)
	_write([]u8{0x58, 0xC0 | ((u8(dst) & 0x7) << 3) | (u8(src2) & 0x7)})
}

vmulps_ymm :: proc(dst: XMMRegister, src1: XMMRegister, src2: XMMRegister) {
	// VEX.256.0F.WIG 59 /r
	_write_vex_prefix(0x00, 0x0F, 0x00, u8(src1), true, false)
	_write([]u8{0x59, 0xC0 | ((u8(dst) & 0x7) << 3) | (u8(src2) & 0x7)})
}

vdivps_ymm :: proc(dst: XMMRegister, src1: XMMRegister, src2: XMMRegister) {
	// VEX.256.0F.WIG 5E /r
	_write_vex_prefix(0x00, 0x0F, 0x00, u8(src1), true, false)
	_write([]u8{0x5E, 0xC0 | ((u8(dst) & 0x7) << 3) | (u8(src2) & 0x7)})
}

vblendps_ymm :: proc(dst: XMMRegister, src1: XMMRegister, src2: XMMRegister, imm: u8) {
	// VEX.256.66.0F3A.WIG 0C /r ib
	_write_vex_prefix(0x66, 0x0F, 0x3A, u8(src1), true, false)
	_write([]u8{0x0C, 0xC0 | ((u8(dst) & 0x7) << 3) | (u8(src2) & 0x7), imm})
}

vpand_ymm :: proc(dst: XMMRegister, src1: XMMRegister, src2: XMMRegister) {
	// VEX.256.66.0F.WIG DB /r
	_write_vex_prefix(0x66, 0x0F, 0x00, u8(src1), true, false)
	_write([]u8{0xDB, 0xC0 | ((u8(dst) & 0x7) << 3) | (u8(src2) & 0x7)})
}

vpor_ymm :: proc(dst: XMMRegister, src1: XMMRegister, src2: XMMRegister) {
	// VEX.256.66.0F.WIG EB /r
	_write_vex_prefix(0x66, 0x0F, 0x00, u8(src1), true, false)
	_write([]u8{0xEB, 0xC0 | ((u8(dst) & 0x7) << 3) | (u8(src2) & 0x7)})
}

vpxor_ymm :: proc(dst: XMMRegister, src1: XMMRegister, src2: XMMRegister) {
	// VEX.256.66.0F.WIG EF /r
	_write_vex_prefix(0x66, 0x0F, 0x00, u8(src1), true, false)
	_write([]u8{0xEF, 0xC0 | ((u8(dst) & 0x7) << 3) | (u8(src2) & 0x7)})
}

vpternlogd_ymm_ymm_ymm_imm8 :: proc(
	dst: XMMRegister,
	src1: XMMRegister,
	src2: XMMRegister,
	imm: u8,
) {
	// EVEX.256.66.0F3A.W0 25 /r ib
	_write_evex_prefix(0x66, 0x0F, 0x3A, u8(src1), u8(src2), true, false)
	_write([]u8{0x25, 0xC0 | ((u8(dst) & 0x7) << 3) | (u8(src2) & 0x7), imm})
}

vextracti128_ymm_ymm_imm8 :: proc(dst: XMMRegister, src: XMMRegister, imm: u8) {
	// VEX.256.66.0F3A.W0 39 /r ib
	_write_vex_prefix(0x66, 0x0F, 0x3A, 0, true, false)
	_write([]u8{0x39, 0xC0 | ((u8(dst) & 0x7) << 3) | (u8(src) & 0x7), imm})
}

// --- SIMD Integer Operations ---
pavgb_xmm_xmm :: proc(dst: XMMRegister, src: XMMRegister) {
	// 66 0F E0 /r
	rex := 0x40
	if u8(dst) & 0x8 != 0 {
		rex |= 0x44
	}
	if u8(src) & 0x8 != 0 {
		rex |= 0x41
	}
	if rex != 0x40 {
		_write([]u8{u8(rex), 0x66, 0x0F, 0xE0, 0xC0 | ((u8(dst) & 0x7) << 3) | (u8(src) & 0x7)})
	} else {
		_write([]u8{0x66, 0x0F, 0xE0, 0xC0 | ((u8(dst) & 0x7) << 3) | (u8(src) & 0x7)})
	}
}

pavgb_ymm_ymm :: proc(dst: XMMRegister, src: XMMRegister) {
	// VEX.256.66.0F.WIG E0 /r
	_write_vex_prefix(0x66, 0x0F, 0x00, u8(src), true, false)
	_write([]u8{0xE0, 0xC0 | ((u8(dst) & 0x7) << 3) | (u8(src) & 0x7)})
}

pmaddwd_xmm_xmm :: proc(dst: XMMRegister, src: XMMRegister) {
	// 66 0F F5 /r
	rex := 0x40
	if u8(dst) & 0x8 != 0 {
		rex |= 0x44
	}
	if u8(src) & 0x8 != 0 {
		rex |= 0x41
	}
	if rex != 0x40 {
		_write([]u8{u8(rex), 0x66, 0x0F, 0xF5, 0xC0 | ((u8(dst) & 0x7) << 3) | (u8(src) & 0x7)})
	} else {
		_write([]u8{0x66, 0x0F, 0xF5, 0xC0 | ((u8(dst) & 0x7) << 3) | (u8(src) & 0x7)})
	}
}

pmulhuw_xmm_xmm :: proc(dst: XMMRegister, src: XMMRegister) {
	// 66 0F E4 /r
	rex := 0x40
	if u8(dst) & 0x8 != 0 {
		rex |= 0x44
	}
	if u8(src) & 0x8 != 0 {
		rex |= 0x41
	}
	if rex != 0x40 {
		_write([]u8{u8(rex), 0x66, 0x0F, 0xE4, 0xC0 | ((u8(dst) & 0x7) << 3) | (u8(src) & 0x7)})
	} else {
		_write([]u8{0x66, 0x0F, 0xE4, 0xC0 | ((u8(dst) & 0x7) << 3) | (u8(src) & 0x7)})
	}
}

// --- AVX-512 Operations ---
vaddpd_zmm_zmm_zmm :: proc(dst: XMMRegister, src1: XMMRegister, src2: XMMRegister) {
	// EVEX.512.66.0F.W1 58 /r
	_write_evex_prefix(0x66, 0x0F, 0x00, u8(src1), u8(src2), false, true)
	_write([]u8{0x58, 0xC0 | ((u8(dst) & 0x7) << 3) | (u8(src2) & 0x7)})
}

vsubpd_zmm_zmm_zmm :: proc(dst: XMMRegister, src1: XMMRegister, src2: XMMRegister) {
	// EVEX.512.66.0F.W1 5C /r
	_write_evex_prefix(0x66, 0x0F, 0x00, u8(src1), u8(src2), false, true)
	_write([]u8{0x5C, 0xC0 | ((u8(dst) & 0x7) << 3) | (u8(src2) & 0x7)})
}

vmulpd_zmm_zmm_zmm :: proc(dst: XMMRegister, src1: XMMRegister, src2: XMMRegister) {
	// EVEX.512.66.0F.W1 59 /r
	_write_evex_prefix(0x66, 0x0F, 0x00, u8(src1), u8(src2), false, true)
	_write([]u8{0x59, 0xC0 | ((u8(dst) & 0x7) << 3) | (u8(src2) & 0x7)})
}

vdivpd_zmm_zmm_zmm :: proc(dst: XMMRegister, src1: XMMRegister, src2: XMMRegister) {
	// EVEX.512.66.0F.W1 5E /r
	_write_evex_prefix(0x66, 0x0F, 0x00, u8(src1), u8(src2), false, true)
	_write([]u8{0x5E, 0xC0 | ((u8(dst) & 0x7) << 3) | (u8(src2) & 0x7)})
}

vgatherdps_xmm :: proc(dst: XMMRegister) {
	// EVEX.128.66.0F38.W0 92 /vsib
	_write_evex_prefix(0x66, 0x0F, 0x38, 0, 0, false, false)
	_write([]u8{0x92, 0x00 | ((u8(dst) & 0x7) << 3)})
	// Note: Full gather instructions need more parameters for vm32x/vm64x etc.
}

vscatterdps_xmm :: proc(dst: XMMRegister) {
	// EVEX.128.66.0F38.W0 A2 /vsib
	_write_evex_prefix(0x66, 0x0F, 0x38, 0, 0, false, false)
	_write([]u8{0xA2, 0x00 | ((u8(dst) & 0x7) << 3)})
	// Note: Full scatter instructions need more parameters for vm32x/vm64x etc.
}

kmovq_k_k :: proc(dst: u8, src: u8) {
	// VEX.L0.66.0F.W1 90 /r
	_write_vex_prefix(0x66, 0x0F, 0x00, 0, false, true)
	_write([]u8{0x90, 0xC0 | ((dst & 0x7) << 3) | (src & 0x7)})
}

vpxordq_zmm_zmm_zmm :: proc(dst: XMMRegister, src1: XMMRegister, src2: XMMRegister) {
	// EVEX.512.66.0F.W1 EF /r
	_write_evex_prefix(0x66, 0x0F, 0x00, u8(src1), u8(src2), false, true)
	_write([]u8{0xEF, 0xC0 | ((u8(dst) & 0x7) << 3) | (u8(src2) & 0x7)})
}

vpscatterdd_ymm_m :: proc(src: XMMRegister, mem: u64) {
	// EVEX.256.66.0F38.W0 A0 /vsib
	_write_evex_prefix(0x66, 0x0F, 0x38, 0, 0, true, false)
	_write([]u8{0xA0})
	_write_modrm_sib_disp(u8(src), mem)
	// Note: Full scatter instructions need more parameters for vm32x etc.
}

vpscatterdq_ymm_m :: proc(src: XMMRegister, mem: u64) {
	// EVEX.256.66.0F38.W1 A0 /vsib
	_write_evex_prefix(0x66, 0x0F, 0x38, 0, 0, true, true)
	_write([]u8{0xA0})
	_write_modrm_sib_disp(u8(src), mem)
	// Note: Full scatter instructions need more parameters for vm32x etc.
}

vpscatterqd_ymm_m :: proc(src: XMMRegister, mem: u64) {
	// EVEX.256.66.0F38.W0 A1 /vsib
	_write_evex_prefix(0x66, 0x0F, 0x38, 0, 0, true, false)
	_write([]u8{0xA1})
	_write_modrm_sib_disp(u8(src), mem)
	// Note: Full scatter instructions need more parameters for vm64x etc.
}

vpcompressd_ymm_ymm :: proc(dst: XMMRegister, src: XMMRegister) {
	// EVEX.256.66.0F38.W0 8B /r
	_write_evex_prefix(0x66, 0x0F, 0x38, 0, u8(src), true, false)
	_write([]u8{0x8B, 0xC0 | ((u8(dst) & 0x7) << 3) | (u8(src) & 0x7)})
}

vpcompressq_ymm_ymm :: proc(dst: XMMRegister, src: XMMRegister) {
	// EVEX.256.66.0F38.W1 8B /r
	_write_evex_prefix(0x66, 0x0F, 0x38, 0, u8(src), true, true)
	_write([]u8{0x8B, 0xC0 | ((u8(dst) & 0x7) << 3) | (u8(src) & 0x7)})
}

// Helper function for writing VEX prefix
_write_vex_prefix :: proc(pp: u8, map_select: u8, vvvv_complement: u8, b: u8, L: bool, W: bool) {
	// map_select: 0=0F, 1=0F38, 2=0F3A
	if map_select == 0 && vvvv_complement == 0x00 && !W {
		// 2-byte VEX
		vex := 0xC5
		vvvvv := (~b) & 0xF
		if L {
			vvvvv |= 0x04
		}
		_write([]u8{u8(vex), u8(vvvvv << 3 | pp)})
	} else {
		// 3-byte VEX
		vex := 0xC4
		byte1 := u8(0xE0) // R=0, X=0, B=0, 1110_0000
		byte1 |= map_select & 0x3 // 0=0F, 1=0F38, 2=0F3A

		vvvvv := (~b) & 0xF
		byte2 := vvvvv << 3
		if W {
			byte2 |= 0x80
		}
		if L {
			byte2 |= 0x04
		}
		byte2 |= pp
		_write([]u8{u8(vex), byte1, byte2})
	}
}

// Helper function for writing EVEX prefix
_write_evex_prefix :: proc(
	pp: u8,
	map_select: u8,
	vvvv_complement: u8,
	b: u8,
	v2: u8,
	L: bool,
	W: bool,
) {
	// 62 R X B R' mmmmm W vvvv pp
	// map_select: 0=0F, 1=0F38, 2=0F3A
	evex := 0x62
	byte1 := u8(0xF0) // Inverting R, X, B bits (all set to 0)

	byte2 := u8(0)
	byte2 |= map_select & 0x3 // 0=0F, 1=0F38, 2=0F3A
	if W {
		byte2 |= 0x80
	}
	byte2 |= (~v2) & 0x78 // Inverted V' field

	byte3 := (~b) & 0xF // Inverted vvvv field
	byte3 <<= 3
	if L {
		byte3 |= 0x20 // L bit
	}
	byte3 |= pp

	_write([]u8{u8(evex), byte1, byte2, byte3})
}

// Helper function to write ModRM, SIB, and displacement
_write_modrm_sib_disp :: proc(reg: u8, mem: u64) {
	// Simplified implementation - would need expansion for full addressing modes
	_write([]u8{0x04, (reg & 0x7) << 3})
}

// ===== CRYPTOGRAPHY INSTRUCTIONS =====
aesenc :: proc(dst: XMMRegister, src: XMMRegister) {
	// 66 0F 38 DC /r
	rex := 0x40
	if u8(dst) & 0x8 != 0 {
		rex |= 0x44
	}
	if u8(src) & 0x8 != 0 {
		rex |= 0x41
	}
	if rex != 0x40 {
		_write(
			[]u8{u8(rex), 0x66, 0x0F, 0x38, 0xDC, 0xC0 | ((u8(dst) & 0x7) << 3) | (u8(src) & 0x7)},
		)
	} else {
		_write([]u8{0x66, 0x0F, 0x38, 0xDC, 0xC0 | ((u8(dst) & 0x7) << 3) | (u8(src) & 0x7)})
	}
}

aesdec :: proc(dst: XMMRegister, src: XMMRegister) {
	// 66 0F 38 DE /r
	rex := 0x40
	if u8(dst) & 0x8 != 0 {
		rex |= 0x44
	}
	if u8(src) & 0x8 != 0 {
		rex |= 0x41
	}
	if rex != 0x40 {
		_write(
			[]u8{u8(rex), 0x66, 0x0F, 0x38, 0xDE, 0xC0 | ((u8(dst) & 0x7) << 3) | (u8(src) & 0x7)},
		)
	} else {
		_write([]u8{0x66, 0x0F, 0x38, 0xDE, 0xC0 | ((u8(dst) & 0x7) << 3) | (u8(src) & 0x7)})
	}
}

pclmulqdq :: proc(dst: XMMRegister, src: XMMRegister, imm: u8) {
	// 66 0F 3A 44 /r ib
	rex := 0x40
	if u8(dst) & 0x8 != 0 {
		rex |= 0x44
	}
	if u8(src) & 0x8 != 0 {
		rex |= 0x41
	}
	if rex != 0x40 {
		_write(
			[]u8 {
				u8(rex),
				0x66,
				0x0F,
				0x3A,
				0x44,
				0xC0 | ((u8(dst) & 0x7) << 3) | (u8(src) & 0x7),
				imm,
			},
		)
	} else {
		_write([]u8{0x66, 0x0F, 0x3A, 0x44, 0xC0 | ((u8(dst) & 0x7) << 3) | (u8(src) & 0x7), imm})
	}
}

crc32_r64_r64 :: proc(dst: Register64, src: Register64) {
	// F2 REX.W 0F 38 F1 /r
	rex := 0x48
	if u8(dst) & 0x8 != 0 {
		rex |= 0x44
	}
	if u8(src) & 0x8 != 0 {
		rex |= 0x41
	}
	_write([]u8{0xF2, u8(rex), 0x0F, 0x38, 0xF1, 0xC0 | ((u8(dst) & 0x7) << 3) | (u8(src) & 0x7)})
}

// ===== X87 FLOATING POINT INSTRUCTIONS =====
fadd_st0_st :: proc(src: x87Register) {
	// D8 C0+i
	_write([]u8{0xD8, 0xC0 | u8(src)})
}

fsub_st0_st :: proc(src: x87Register) {
	// D8 E0+i
	_write([]u8{0xD8, 0xE0 | u8(src)})
}

fmul_st0_st :: proc(src: x87Register) {
	// D8 C8+i
	_write([]u8{0xD8, 0xC8 | u8(src)})
}

fdiv_st0_st :: proc(src: x87Register) {
	// D8 F0+i
	_write([]u8{0xD8, 0xF0 | u8(src)})
}

fcom_st :: proc(src: x87Register) {
	// D8 D0+i
	_write([]u8{0xD8, 0xD0 | u8(src)})
}

fcomp_st :: proc(src: x87Register) {
	// D8 D8+i
	_write([]u8{0xD8, 0xD8 | u8(src)})
}

fucom_st :: proc(src: x87Register) {
	// DD E0+i
	_write([]u8{0xDD, 0xE0 | u8(src)})
}

fucomp_st :: proc(src: x87Register) {
	// DD E8+i
	_write([]u8{0xDD, 0xE8 | u8(src)})
}

fldcw :: proc(mem: u64) {
	// D9 /5
	_write([]u8{0xD9})
	_write_modrm_sib_disp(5, mem)
}

fstcw :: proc(mem: u64) {
	// 9B D9 /7
	_write([]u8{0x9B, 0xD9})
	_write_modrm_sib_disp(7, mem)
}

fnstcw :: proc(mem: u64) {
	// D9 /7
	_write([]u8{0xD9})
	_write_modrm_sib_disp(7, mem)
}

fninit :: proc() {
	// DB E3
	_write([]u8{0xDB, 0xE3})
}

fwait :: proc() {
	// 9B
	_write([]u8{0x9B})
}

// ===== MEMORY AND STRING OPERATIONS =====
movs_m64_m64 :: proc() {
	// REX.W A5
	_write([]u8{0x48, 0xA5})
}

stos_m64 :: proc() {
	// REX.W AA
	_write([]u8{0x48, 0xAA})
}

cmps_m64_m64 :: proc() {
	// REX.W A7
	_write([]u8{0x48, 0xA7})
}

rep_movs :: proc() {
	// F3 REX.W A5
	_write([]u8{0xF3, 0x48, 0xA5})
}

rep_stos :: proc() {
	// F3 REX.W AA
	_write([]u8{0xF3, 0x48, 0xAA})
}

rep_cmps :: proc() {
	// F3 REX.W A7
	_write([]u8{0xF3, 0x48, 0xA7})
}

// ===== SYSTEM INSTRUCTIONS =====
// --- Interrupts and System Calls ---
syscall :: proc() {
	// 0F 05
	_write([]u8{0x0F, 0x05})
}

sysret :: proc() {
	// 0F 07
	_write([]u8{0x0F, 0x07})
}

int_imm8 :: proc(imm: u8) {
	// CD ib
	_write([]u8{0xCD, imm})
}

int3 :: proc() {
	// CC
	_write([]u8{0xCC})
}

iret :: proc() {
	// 48 CF
	_write([]u8{0x48, 0xCF})
}

// --- Processor Control ---
cpuid :: proc() {
	// 0F A2
	_write([]u8{0x0F, 0xA2})
}

rdtsc :: proc() {
	// 0F 31
	_write([]u8{0x0F, 0x31})
}

rdtscp :: proc() {
	// 0F 01 F9
	_write([]u8{0x0F, 0x01, 0xF9})
}

rdmsr :: proc() {
	// 0F 32
	_write([]u8{0x0F, 0x32})
}

wrmsr :: proc() {
	// 0F 30
	_write([]u8{0x0F, 0x30})
}

rdpmc :: proc() {
	// 0F 33
	_write([]u8{0x0F, 0x33})
}

hlt :: proc() {
	// F4
	_write([]u8{0xF4})
}

// --- Process & Memory Management ---
swapgs :: proc() {
	// 0F 01 F8
	_write([]u8{0x0F, 0x01, 0xF8})
}

wrpkru :: proc() {
	// 0F 01 EF
	_write([]u8{0x0F, 0x01, 0xEF})
}

rdpkru :: proc() {
	// 0F 01 EE
	_write([]u8{0x0F, 0x01, 0xEE})
}

clac :: proc() {
	// 0F 01 CA
	_write([]u8{0x0F, 0x01, 0xCA})
}

stac :: proc() {
	// 0F 01 CB
	_write([]u8{0x0F, 0x01, 0xCB})
}

ud2 :: proc() {
	// 0F 0B
	_write([]u8{0x0F, 0x0B})
}

// --- Virtualization Instructions ---
vmcall :: proc() {
	// 0F 01 C1
	_write([]u8{0x0F, 0x01, 0xC1})
}

vmlaunch :: proc() {
	// 0F 01 C2
	_write([]u8{0x0F, 0x01, 0xC2})
}

vmresume :: proc() {
	// 0F 01 C3
	_write([]u8{0x0F, 0x01, 0xC3})
}

vmxoff :: proc() {
	// 0F 01 C4
	_write([]u8{0x0F, 0x01, 0xC4})
}

// ===== ATOMIC OPERATIONS =====
lock_xadd_r64_r64 :: proc(dst: Register64, src: Register64) {
	// F0 REX.W 0F C1 /r
	rex := 0x48
	if u8(dst) & 0x8 != 0 {
		rex |= 0x44
	}
	if u8(src) & 0x8 != 0 {
		rex |= 0x41
	}
	_write([]u8{0xF0, u8(rex), 0x0F, 0xC1, 0xC0 | ((u8(src) & 0x7) << 3) | (u8(dst) & 0x7)})
}

lock_cmpxchg_r64_r64 :: proc(dst: Register64, src: Register64) {
	// F0 REX.W 0F B1 /r
	rex := 0x48
	if u8(dst) & 0x8 != 0 {
		rex |= 0x41
	}
	if u8(src) & 0x8 != 0 {
		rex |= 0x44
	}
	_write([]u8{0xF0, u8(rex), 0x0F, 0xB1, 0xC0 | ((u8(src) & 0x7) << 3) | (u8(dst) & 0x7)})
}

lock_inc_r64 :: proc(reg: Register64) {
	// F0 REX.W FF /0
	rex := 0x48
	if u8(reg) & 0x8 != 0 {
		rex |= 0x41
	}
	_write([]u8{0xF0, u8(rex), 0xFF, 0xC0 | (u8(reg) & 0x7)})
}

lock_dec_r64 :: proc(reg: Register64) {
	// F0 REX.W FF /1
	rex := 0x48
	if u8(reg) & 0x8 != 0 {
		rex |= 0x41
	}
	_write([]u8{0xF0, u8(rex), 0xFF, 0xC8 | (u8(reg) & 0x7)})
}

lock_xchg_r64_r64 :: proc(dst: Register64, src: Register64) {
	// F0 REX.W 87 /r
	rex := 0x48
	if u8(dst) & 0x8 != 0 {
		rex |= 0x41
	}
	if u8(src) & 0x8 != 0 {
		rex |= 0x44
	}
	_write([]u8{0xF0, u8(rex), 0x87, 0xC0 | ((u8(src) & 0x7) << 3) | (u8(dst) & 0x7)})
}

atomic_load_r64 :: proc(dst: Register64, src: u64) {
	// REX.W 8B /r
	rex := 0x48
	if u8(dst) & 0x8 != 0 {
		rex |= 0x44
	}
	_write([]u8{u8(rex), 0x8B})
	_write_modrm_sib_disp(u8(dst), src)
}

atomic_store_r64 :: proc(dst: u64, src: Register64) {
	// REX.W 89 /r
	rex := 0x48
	if u8(src) & 0x8 != 0 {
		rex |= 0x44
	}
	_write([]u8{u8(rex), 0x89})
	_write_modrm_sib_disp(u8(src), dst)
}

// ===== TRANSACTIONAL MEMORY INSTRUCTIONS =====
xbegin :: proc(offset: i32) {
	// C7 F8 <rel32>
	_write([]u8{0xC7, 0xF8})
	_write_i32(offset)
}

xend :: proc() {
	// 0F 01 D5
	_write([]u8{0x0F, 0x01, 0xD5})
}

xabort :: proc(imm: u8) {
	// C6 F8 ib
	_write([]u8{0xC6, 0xF8, imm})
}

xtest :: proc() {
	// 0F 01 D6
	_write([]u8{0x0F, 0x01, 0xD6})
}

// ===== RANDOM NUMBER GENERATION =====
rdrand_r64 :: proc(reg: Register64) {
	// REX.W 0F C7 /6
	rex := 0x48
	if u8(reg) & 0x8 != 0 {
		rex |= 0x41
	}
	_write([]u8{u8(rex), 0x0F, 0xC7, 0xF0 | (u8(reg) & 0x7)})
}

rdseed_r64 :: proc(reg: Register64) {
	// REX.W 0F C7 /7
	rex := 0x48
	if u8(reg) & 0x8 != 0 {
		rex |= 0x41
	}
	_write([]u8{u8(rex), 0x0F, 0xC7, 0xF8 | (u8(reg) & 0x7)})
}

// ===== PREFETCH INSTRUCTIONS =====
prefetcht0 :: proc(mem: u64) {
	// 0F 18 /1
	_write([]u8{0x0F, 0x18})
	_write_modrm_sib_disp(1, mem)
}

prefetcht1 :: proc(mem: u64) {
	// 0F 18 /2
	_write([]u8{0x0F, 0x18})
	_write_modrm_sib_disp(2, mem)
}

prefetcht2 :: proc(mem: u64) {
	// 0F 18 /3
	_write([]u8{0x0F, 0x18})
	_write_modrm_sib_disp(3, mem)
}

prefetchnta :: proc(mem: u64) {
	// 0F 18 /0
	_write([]u8{0x0F, 0x18})
	_write_modrm_sib_disp(0, mem)
}

// ===== MEMORY MANAGEMENT AND OPTIMIZATION =====
clflush_m64 :: proc(mem: u64) {
	// 0F AE /7
	_write([]u8{0x0F, 0xAE})
	_write_modrm_sib_disp(7, mem)
}

clflushopt_m64 :: proc(mem: u64) {
	// 66 0F AE /7
	_write([]u8{0x66, 0x0F, 0xAE})
	_write_modrm_sib_disp(7, mem)
}

clwb_m64 :: proc(mem: u64) {
	// 66 0F AE /6
	_write([]u8{0x66, 0x0F, 0xAE})
	_write_modrm_sib_disp(6, mem)
}


// ===== POWER MANAGEMENT =====
monitor_r64_r64_r64 :: proc(reg1: Register64, reg2: Register64, reg3: Register64) {
	// 0F 01 C8
	_write([]u8{0x0F, 0x01, 0xC8})
	// Note: This instruction uses EAX, ECX, and EDX implicitly, regardless of the register parameters
}

mwait_r64_r64 :: proc(reg1: Register64, reg2: Register64) {
	// 0F 01 C9
	_write([]u8{0x0F, 0x01, 0xC9})
	// Note: This instruction uses EAX and ECX implicitly, regardless of the register parameters
}

// ===== MEMORY FENCES =====
mfence :: proc() {
	// 0F AE F0
	_write([]u8{0x0F, 0xAE, 0xF0})
}

lfence :: proc() {
	// 0F AE E8
	_write([]u8{0x0F, 0xAE, 0xE8})
}

sfence :: proc() {
	// 0F AE F8
	_write([]u8{0x0F, 0xAE, 0xF8})
}

// ===== PERFORMANCE MONITORING =====
perfmon_instructions :: proc() {
	// This is a placeholder function that serves as a reminder for various performance monitoring instructions
	// Actual implementations should call specific perf monitoring instructions as needed
}

// Helper function to write 32-bit signed immediate
_write_i32 :: proc(imm: i32) {
	_write(
		[]u8 {
			u8(imm & 0xFF),
			u8((imm >> 8) & 0xFF),
			u8((imm >> 16) & 0xFF),
			u8((imm >> 24) & 0xFF),
		},
	)
}

VoidCallback :: proc()
// Helper function to test if two byte arrays are equal
test_equal :: proc(t: ^testing.T, desc: string, expected: []u8, writer: VoidCallback) {
	writer()
	if len(expected) != _buffer.len {
		log.info(
			desc,
			": Length mismatch. Expected",
			expected,
			"bytes, got",
			_buffer.data[:_buffer.len],
			"bytes.",
		)
		resetBuffer()
		testing.fail(t)
		return
	}


	for i in 0 ..< len(expected) {
		if expected[i] != _buffer.data[i] {
			log.infof(
				"%s: Byte mismatch at position %d. Expected 0x%02X, got 0x%02X.\n",
				desc,
				i,
				expected[i],
				_buffer.data[i],
			)
			resetBuffer()
			testing.fail(t)
			return
		}
	}
	resetBuffer()
}

@(test)
test_mov_r64_imm64 :: proc(t: ^testing.T) {
	test_equal(
		t,
		"mov rax, 0x123456789ABCDEF0",
		asm_to_bytes("mov rax, 0x123456789ABCDEF0"),
		proc() {mov_r64_imm64(Register64.RAX, 0x123456789ABCDEF0)},
	)
	test_equal(
		t,
		"mov rcx, 0x0",
		asm_to_bytes("mov rcx, 0x0"),
		proc() {mov_r64_imm64(Register64.RCX, 0x0)},
	)
	test_equal(
		t,
		"mov r8, 0xFFFFFFFFFFFFFFFF",
		asm_to_bytes("mov r8, 0xFFFFFFFFFFFFFFFF"),
		proc() {mov_r64_imm64(Register64.R8, 0xFFFFFFFFFFFFFFFF)},
	)
	test_equal(
		t,
		"mov r15, 0xAAAAAAAAAAAAAAAA",
		asm_to_bytes("mov r15, 0xAAAAAAAAAAAAAAAA"),
		proc() {mov_r64_imm64(Register64.R15, 0xAAAAAAAAAAAAAAAA)},
	)
}

@(test)
test_mov_r64_r64 :: proc(t: ^testing.T) {
	// Regular cases
	test_equal(
		t,
		"mov rax, rbx",
		asm_to_bytes("mov rax, rbx"),
		proc() {mov_r64_r64(Register64.RAX, Register64.RBX)},
	)
	test_equal(
		t,
		"mov r8, r9",
		asm_to_bytes("mov r8, r9"),
		proc() {mov_r64_r64(Register64.R8, Register64.R9)},
	)
	test_equal(
		t,
		"mov r15, rax",
		asm_to_bytes("mov r15, rax"),
		proc() {mov_r64_r64(Register64.R15, Register64.RAX)},
	)
	test_equal(
		t,
		"mov rsp, rbp",
		asm_to_bytes("mov rsp, rbp"),
		proc() {mov_r64_r64(Register64.RSP, Register64.RBP)},
	)

	// Self-move case
	test_equal(
		t,
		"mov rdx, rdx",
		asm_to_bytes("mov rdx, rdx"),
		proc() {mov_r64_r64(Register64.RDX, Register64.RDX)},
	)
}

@(test)
test_mov_m64_r64 :: proc(t: ^testing.T) {
	// Regular cases
	test_equal(
		t,
		"mov [0x1000], rax",
		asm_to_bytes("mov [0x1000], rax"),
		proc() {mov_m64_r64(0x1000, Register64.RAX)},
	)
	test_equal(
		t,
		"mov [0x0], r15",
		asm_to_bytes("mov [0x0], r15"),
		proc() {mov_m64_r64(0x0, Register64.R15)},
	)
	test_equal(
		t,
		"mov [0x0FFFFFFF], rdx",
		asm_to_bytes("mov [0x0FFFFFFF], rdx"),
		proc() {mov_m64_r64(0x0FFFFFFF, Register64.RDX)},
	)
	test_equal(
		t,
		"mov [0x7FFFFFFF], r8",
		asm_to_bytes("mov [0x7FFFFFFF], r8"),
		proc() {mov_m64_r64(0x7FFFFFFF, Register64.R8)},
	)
}

@(test)
test_movabs_r64_imm64 :: proc(t: ^testing.T) {
	// Regular cases
	test_equal(
		t,
		"movabs rax, 0x123456789ABCDEF0",
		asm_to_bytes("movabs rax, 0x123456789ABCDEF0"),
		proc() {movabs_r64_imm64(Register64.RAX, 0x123456789ABCDEF0)},
	)
	test_equal(
		t,
		"movabs r10, 0x0",
		asm_to_bytes("movabs r10, 0x0"),
		proc() {movabs_r64_imm64(Register64.R10, 0x0)},
	)
	test_equal(
		t,
		"movabs r15, 0xFFFFFFFFFFFFFFFF",
		asm_to_bytes("movabs r15, 0xFFFFFFFFFFFFFFFF"),
		proc() {movabs_r64_imm64(Register64.R15, 0xFFFFFFFFFFFFFFFF)},
	)
	test_equal(
		t,
		"movabs rbx, 0x8000000000000000",
		asm_to_bytes("movabs rbx, 0x8000000000000000"),
		proc() {movabs_r64_imm64(Register64.RBX, 0x8000000000000000)},
	)
}

@(test)
test_xchg_r64_r64 :: proc(t: ^testing.T) {
	// Regular cases
	test_equal(
		t,
		"xchg rax, rbx",
		asm_to_bytes("xchg rax, rbx"),
		proc() {xchg_r64_r64(Register64.RAX, Register64.RBX)},
	)
	test_equal(
		t,
		"xchg r8, r9",
		asm_to_bytes("xchg r8, r9"),
		proc() {xchg_r64_r64(Register64.R8, Register64.R9)},
	)
	test_equal(
		t,
		"xchg r15, rcx",
		asm_to_bytes("xchg r15, rcx"),
		proc() {xchg_r64_r64(Register64.R15, Register64.RCX)},
	)

	// Self-exchange case
	test_equal(
		t,
		"xchg rdx, rdx",
		asm_to_bytes("xchg rdx, rdx"),
		proc() {xchg_r64_r64(Register64.RDX, Register64.RDX)},
	)
}

@(test)
test_movsx_r64_r32 :: proc(t: ^testing.T) {
	// Regular cases
	test_equal(
		t,
		"movsx rax, ebx",
		asm_to_bytes("movsx rax, ebx"),
		proc() {movsx_r64_r32(Register64.RAX, Register64.RBX)},
	)
	test_equal(
		t,
		"movsx r8, ecx",
		asm_to_bytes("movsx r8, ecx"),
		proc() {movsx_r64_r32(Register64.R8, Register64.RCX)},
	)
	test_equal(
		t,
		"movsx r15, eax",
		asm_to_bytes("movsx r15, eax"),
		proc() {movsx_r64_r32(Register64.R15, Register64.RAX)},
	)
	test_equal(
		t,
		"movsx rbx, r9d",
		asm_to_bytes("movsx rbx, r9d"),
		proc() {movsx_r64_r32(Register64.RBX, Register64.R9)},
	)
}

@(test)
test_movbe_r64_m64 :: proc(t: ^testing.T) {
	// Regular cases
	test_equal(
		t,
		"movbe rax, [0x1000]",
		asm_to_bytes("movbe rax, [0x1000]"),
		proc() {movbe_r64_m64(Register64.RAX, 0x1000)},
	)
	test_equal(
		t,
		"movbe r15, [0x0]",
		asm_to_bytes("movbe r15, [0x0]"),
		proc() {movbe_r64_m64(Register64.R15, 0x0)},
	)
	test_equal(
		t,
		"movbe rdx, [0x0FFFFFFF]",
		asm_to_bytes("movbe rdx, [0x0FFFFFFF]"),
		proc() {movbe_r64_m64(Register64.RDX, 0x0FFFFFFF)},
	)
	test_equal(
		t,
		"movbe r8, [0x7FFFFFFF]",
		asm_to_bytes("movbe r8, [0x7FFFFFFF]"),
		proc() {movbe_r64_m64(Register64.R8, 0x7FFFFFFF)},
	)
}

@(test)
test_movbe_m64_r64 :: proc(t: ^testing.T) {
	// Regular cases
	test_equal(
		t,
		"movbe [0x1000], rax",
		asm_to_bytes("movbe [0x1000], rax"),
		proc() {movbe_m64_r64(0x1000, Register64.RAX)},
	)
	test_equal(
		t,
		"movbe [0x0], r15",
		asm_to_bytes("movbe [0x0], r15"),
		proc() {movbe_m64_r64(0x0, Register64.R15)},
	)
	test_equal(
		t,
		"movbe [0x0FFFFFFF], rdx",
		asm_to_bytes("movbe [0x0FFFFFFF], rdx"),
		proc() {movbe_m64_r64(0x0FFFFFFF, Register64.RDX)},
	)
	test_equal(
		t,
		"movbe [0x7FFFFFFF], r8",
		asm_to_bytes("movbe [0x7FFFFFFF], r8"),
		proc() {movbe_m64_r64(0x7FFFFFFF, Register64.R8)},
	)
}

@(test)
test_bswap_r64 :: proc(t: ^testing.T) {
	// Test all registers
	test_equal(t, "bswap rax", asm_to_bytes("bswap rax"), proc() {bswap_r64(Register64.RAX)})
	test_equal(t, "bswap rcx", asm_to_bytes("bswap rcx"), proc() {bswap_r64(Register64.RCX)})
	test_equal(t, "bswap r8", asm_to_bytes("bswap r8"), proc() {bswap_r64(Register64.R8)})
	test_equal(t, "bswap r15", asm_to_bytes("bswap r15"), proc() {bswap_r64(Register64.R15)})
}

@(test)
test_mov_cr_r64 :: proc(t: ^testing.T) {
	// Test various CR registers
	test_equal(
		t,
		"mov cr0, rax",
		asm_to_bytes("mov cr0, rax"),
		proc() {mov_cr_r64(0, Register64.RAX)},
	)
	test_equal(
		t,
		"mov cr3, rbx",
		asm_to_bytes("mov cr3, rbx"),
		proc() {mov_cr_r64(3, Register64.RBX)},
	)
	test_equal(
		t,
		"mov cr4, r8",
		asm_to_bytes("mov cr4, r8"),
		proc() {mov_cr_r64(4, Register64.R8)},
	)
	test_equal(
		t,
		"mov cr8, r15",
		asm_to_bytes("mov cr8, r15"),
		proc() {mov_cr_r64(8, Register64.R15)},
	)
}

@(test)
test_mov_r64_cr :: proc(t: ^testing.T) {
	// Test various CR registers
	test_equal(
		t,
		"mov rax, cr0",
		asm_to_bytes("mov rax, cr0"),
		proc() {mov_r64_cr(Register64.RAX, 0)},
	)
	test_equal(
		t,
		"mov rbx, cr3",
		asm_to_bytes("mov rbx, cr3"),
		proc() {mov_r64_cr(Register64.RBX, 3)},
	)
	test_equal(
		t,
		"mov r8, cr4",
		asm_to_bytes("mov r8, cr4"),
		proc() {mov_r64_cr(Register64.R8, 4)},
	)
	test_equal(
		t,
		"mov r15, cr8",
		asm_to_bytes("mov r15, cr8"),
		proc() {mov_r64_cr(Register64.R15, 8)},
	)
}

@(test)
test_mov_cr0_r64 :: proc(t: ^testing.T) {
	// Test all registers with CR0
	test_equal(
		t,
		"mov cr0, rax",
		asm_to_bytes("mov cr0, rax"),
		proc() {mov_cr0_r64(Register64.RAX)},
	)
	test_equal(
		t,
		"mov cr0, rcx",
		asm_to_bytes("mov cr0, rcx"),
		proc() {mov_cr0_r64(Register64.RCX)},
	)
	test_equal(t, "mov cr0, r8", asm_to_bytes("mov cr0, r8"), proc() {mov_cr0_r64(Register64.R8)})
	test_equal(
		t,
		"mov cr0, r15",
		asm_to_bytes("mov cr0, r15"),
		proc() {mov_cr0_r64(Register64.R15)},
	)
}

@(test)
test_mov_cr2_r64 :: proc(t: ^testing.T) {
	// Test all registers with CR2
	test_equal(
		t,
		"mov cr2, rax",
		asm_to_bytes("mov cr2, rax"),
		proc() {mov_cr2_r64(Register64.RAX)},
	)
	test_equal(
		t,
		"mov cr2, rcx",
		asm_to_bytes("mov cr2, rcx"),
		proc() {mov_cr2_r64(Register64.RCX)},
	)
	test_equal(t, "mov cr2, r8", asm_to_bytes("mov cr2, r8"), proc() {mov_cr2_r64(Register64.R8)})
	test_equal(
		t,
		"mov cr2, r15",
		asm_to_bytes("mov cr2, r15"),
		proc() {mov_cr2_r64(Register64.R15)},
	)
}

@(test)
test_mov_cr3_r64 :: proc(t: ^testing.T) {
	// Test all registers with CR3
	test_equal(
		t,
		"mov cr3, rax",
		asm_to_bytes("mov cr3, rax"),
		proc() {mov_cr3_r64(Register64.RAX)},
	)
	test_equal(
		t,
		"mov cr3, rcx",
		asm_to_bytes("mov cr3, rcx"),
		proc() {mov_cr3_r64(Register64.RCX)},
	)
	test_equal(t, "mov cr3, r8", asm_to_bytes("mov cr3, r8"), proc() {mov_cr3_r64(Register64.R8)})
	test_equal(
		t,
		"mov cr3, r15",
		asm_to_bytes("mov cr3, r15"),
		proc() {mov_cr3_r64(Register64.R15)},
	)
}

@(test)
test_mov_cr4_r64 :: proc(t: ^testing.T) {
	// Test all registers with CR4
	test_equal(
		t,
		"mov cr4, rax",
		asm_to_bytes("mov cr4, rax"),
		proc() {mov_cr4_r64(Register64.RAX)},
	)
	test_equal(
		t,
		"mov cr4, rcx",
		asm_to_bytes("mov cr4, rcx"),
		proc() {mov_cr4_r64(Register64.RCX)},
	)
	test_equal(t, "mov cr4, r8", asm_to_bytes("mov cr4, r8"), proc() {mov_cr4_r64(Register64.R8)})
	test_equal(
		t,
		"mov cr4, r15",
		asm_to_bytes("mov cr4, r15"),
		proc() {mov_cr4_r64(Register64.R15)},
	)
}

@(test)
test_mov_dr0_r64 :: proc(t: ^testing.T) {
	// Test all registers with DR0
	test_equal(
		t,
		"mov dr0, rax",
		asm_to_bytes("mov dr0, rax"),
		proc() {mov_dr0_r64(Register64.RAX)},
	)
	test_equal(
		t,
		"mov dr0, rcx",
		asm_to_bytes("mov dr0, rcx"),
		proc() {mov_dr0_r64(Register64.RCX)},
	)
	test_equal(t, "mov dr0, r8", asm_to_bytes("mov dr0, r8"), proc() {mov_dr0_r64(Register64.R8)})
	test_equal(
		t,
		"mov dr0, r15",
		asm_to_bytes("mov dr0, r15"),
		proc() {mov_dr0_r64(Register64.R15)},
	)
}

@(test)
test_mov_dr1_r64 :: proc(t: ^testing.T) {
	// Test all registers with DR1
	test_equal(
		t,
		"mov dr1, rax",
		asm_to_bytes("mov dr1, rax"),
		proc() {mov_dr1_r64(Register64.RAX)},
	)
	test_equal(
		t,
		"mov dr1, rcx",
		asm_to_bytes("mov dr1, rcx"),
		proc() {mov_dr1_r64(Register64.RCX)},
	)
	test_equal(t, "mov dr1, r8", asm_to_bytes("mov dr1, r8"), proc() {mov_dr1_r64(Register64.R8)})
	test_equal(
		t,
		"mov dr1, r15",
		asm_to_bytes("mov dr1, r15"),
		proc() {mov_dr1_r64(Register64.R15)},
	)
}

@(test)
test_mov_dr2_r64 :: proc(t: ^testing.T) {
	// Test all registers with DR2
	test_equal(
		t,
		"mov dr2, rax",
		asm_to_bytes("mov dr2, rax"),
		proc() {mov_dr2_r64(Register64.RAX)},
	)
	test_equal(
		t,
		"mov dr2, rcx",
		asm_to_bytes("mov dr2, rcx"),
		proc() {mov_dr2_r64(Register64.RCX)},
	)
	test_equal(t, "mov dr2, r8", asm_to_bytes("mov dr2, r8"), proc() {mov_dr2_r64(Register64.R8)})
	test_equal(
		t,
		"mov dr2, r15",
		asm_to_bytes("mov dr2, r15"),
		proc() {mov_dr2_r64(Register64.R15)},
	)
}

@(test)
test_mov_dr3_r64 :: proc(t: ^testing.T) {
	test_equal(
		t,
		"mov dr3, rax",
		asm_to_bytes("mov dr3, rax"),
		proc() {mov_dr3_r64(Register64.RAX)},
	)
	test_equal(
		t,
		"mov dr3, rcx",
		asm_to_bytes("mov dr3, rcx"),
		proc() {mov_dr3_r64(Register64.RCX)},
	)
	test_equal(t, "mov dr3, r8", asm_to_bytes("mov dr3, r8"), proc() {mov_dr3_r64(Register64.R8)})
	test_equal(
		t,
		"mov dr3, r15",
		asm_to_bytes("mov dr3, r15"),
		proc() {mov_dr3_r64(Register64.R15)},
	)
}

@(test)
test_mov_dr6_r64 :: proc(t: ^testing.T) {
	// Test all registers with DR6
	test_equal(
		t,
		"mov dr6, rax",
		asm_to_bytes("mov dr6, rax"),
		proc() {mov_dr6_r64(Register64.RAX)},
	)
	test_equal(
		t,
		"mov dr6, rcx",
		asm_to_bytes("mov dr6, rcx"),
		proc() {mov_dr6_r64(Register64.RCX)},
	)
	test_equal(t, "mov dr6, r8", asm_to_bytes("mov dr6, r8"), proc() {mov_dr6_r64(Register64.R8)})
	test_equal(
		t,
		"mov dr6, r15",
		asm_to_bytes("mov dr6, r15"),
		proc() {mov_dr6_r64(Register64.R15)},
	)
}

@(test)
test_mov_dr7_r64 :: proc(t: ^testing.T) {
	// Test all registers with DR7
	test_equal(
		t,
		"mov dr7, rax",
		asm_to_bytes("mov dr7, rax"),
		proc() {mov_dr7_r64(Register64.RAX)},
	)
	test_equal(
		t,
		"mov dr7, rcx",
		asm_to_bytes("mov dr7, rcx"),
		proc() {mov_dr7_r64(Register64.RCX)},
	)
	test_equal(t, "mov dr7, r8", asm_to_bytes("mov dr7, r8"), proc() {mov_dr7_r64(Register64.R8)})
	test_equal(
		t,
		"mov dr7, r15",
		asm_to_bytes("mov dr7, r15"),
		proc() {mov_dr7_r64(Register64.R15)},
	)
}

@(test)
test_lea_r64_m64 :: proc(t: ^testing.T) {
	// Regular cases
	test_equal(
		t,
		"lea rax, [0x1000]",
		asm_to_bytes("lea rax, [0x1000]"),
		proc() {lea_r64_m64(Register64.RAX, 0x1000)},
	)
	test_equal(
		t,
		"lea r15, [0x0]",
		asm_to_bytes("lea r15, [0x0]"),
		proc() {lea_r64_m64(Register64.R15, 0x0)},
	)
	test_equal(
		t,
		"lea rdx, [0x0FFFFFFF]",
		asm_to_bytes("lea rdx, [0x0FFFFFFF]"),
		proc() {lea_r64_m64(Register64.RDX, 0x0FFFFFFF)},
	)
	test_equal(
		t,
		"lea r8, [0x7FFFFFFF]",
		asm_to_bytes("lea r8, [0x7FFFFFFF]"),
		proc() {lea_r64_m64(Register64.R8, 0x7FFFFFFF)},
	)
}


// @(test)
// test_add_r64_imm32 :: proc(t: ^testing.T) {
// 	// Regular cases
// 	test_equal(
// 		t,
// 		"add rax, 0x1000",
// 		asm_to_bytes("add rax, 0x1000"),
// 		proc() {add_r64_imm32(Register64.RAX, 0x1000)},
// 	)
// 	test_equal(t, "add r15, 0x0", asm_to_bytes("add r15, 0x0"), proc() {add_r64_imm32(Register64.R15, 0x0)})
// 	test_equal(
// 		t,
// 		"add rdx, 0xFFFFFFFF",
// 		asm_to_bytes("add rdx, 0xFFFFFFFF"),
// 		proc() {add_r64_imm32(Register64.RDX, 0xFFFFFFFF)},
// 	)
// 	test_equal(
// 		t,
// 		"add r8, 0x7FFFFFFF",
// 		asm_to_bytes("add r8, 0x7FFFFFFF"),
// 		proc() {add_r64_imm32(Register64.R8, 0x7FFFFFFF)},
// 	)
// }

@(test)
test_add_r64_r64 :: proc(t: ^testing.T) {
	// Regular cases
	test_equal(
		t,
		"add rax, rbx",
		asm_to_bytes("add rax, rbx"),
		proc() {add_r64_r64(Register64.RAX, Register64.RBX)},
	)
	test_equal(
		t,
		"add r8, r9",
		asm_to_bytes("add r8, r9"),
		proc() {add_r64_r64(Register64.R8, Register64.R9)},
	)
	test_equal(
		t,
		"add r15, rax",
		asm_to_bytes("add r15, rax"),
		proc() {add_r64_r64(Register64.R15, Register64.RAX)},
	)
	test_equal(
		t,
		"add rsp, rbp",
		asm_to_bytes("add rsp, rbp"),
		proc() {add_r64_r64(Register64.RSP, Register64.RBP)},
	)

	// Self-add case
	test_equal(
		t,
		"add rdx, rdx",
		asm_to_bytes("add rdx, rdx"),
		proc() {add_r64_r64(Register64.RDX, Register64.RDX)},
	)
}

// @(test)
// test_sub_r64_imm32 :: proc(t: ^testing.T) {
// 	// Regular cases
// 	test_equal(
// 		t,
// 		"sub rax, 0x1000",
// 		asm_to_bytes("sub rax, 0x1000"),
// 		proc() {sub_r64_imm32(Register64.RAX, 0x1000)},
// 	)
// 	test_equal(t, "sub r15, 0x0", asm_to_bytes("sub r15, 0x0"), proc() {sub_r64_imm32(Register64.R15, 0x0)})
// 	test_equal(
// 		t,
// 		"sub rdx, 0xFFFFFFFF",
// 		asm_to_bytes("sub rdx, dword ptr 0xFFFFFFFF"),
// 		proc() {sub_r64_imm32(Register64.RDX, 0xFFFFFFFF)},
// 	)
// 	test_equal(
// 		t,
// 		"sub r8, 0x7FFFFFFF",
// 		asm_to_bytes("sub r8, 0x7FFFFFFF"),
// 		proc() {sub_r64_imm32(Register64.R8, 0x7FFFFFFF)},
// 	)
// }

@(test)
test_sub_r64_r64 :: proc(t: ^testing.T) {
	// Regular cases
	test_equal(
		t,
		"sub rax, rbx",
		asm_to_bytes("sub rax, rbx"),
		proc() {sub_r64_r64(Register64.RAX, Register64.RBX)},
	)
	test_equal(
		t,
		"sub r8, r9",
		asm_to_bytes("sub r8, r9"),
		proc() {sub_r64_r64(Register64.R8, Register64.R9)},
	)
	test_equal(
		t,
		"sub r15, rax",
		asm_to_bytes("sub r15, rax"),
		proc() {sub_r64_r64(Register64.R15, Register64.RAX)},
	)
	test_equal(
		t,
		"sub rsp, rbp",
		asm_to_bytes("sub rsp, rbp"),
		proc() {sub_r64_r64(Register64.RSP, Register64.RBP)},
	)

	// Self-subtract case
	test_equal(
		t,
		"sub rdx, rdx",
		asm_to_bytes("sub rdx, rdx"),
		proc() {sub_r64_r64(Register64.RDX, Register64.RDX)},
	)
}

@(test)
test_inc_r64 :: proc(t: ^testing.T) {
	// Test all registers
	test_equal(t, "inc rax", asm_to_bytes("inc rax"), proc() {inc_r64(Register64.RAX)})
	test_equal(t, "inc rcx", asm_to_bytes("inc rcx"), proc() {inc_r64(Register64.RCX)})
	test_equal(t, "inc r8", asm_to_bytes("inc r8"), proc() {inc_r64(Register64.R8)})
	test_equal(t, "inc r15", asm_to_bytes("inc r15"), proc() {inc_r64(Register64.R15)})
}

@(test)
test_dec_r64 :: proc(t: ^testing.T) {
	// Test all registers
	test_equal(t, "dec rax", asm_to_bytes("dec rax"), proc() {dec_r64(Register64.RAX)})
	test_equal(t, "dec rcx", asm_to_bytes("dec rcx"), proc() {dec_r64(Register64.RCX)})
	test_equal(t, "dec r8", asm_to_bytes("dec r8"), proc() {dec_r64(Register64.R8)})
	test_equal(t, "dec r15", asm_to_bytes("dec r15"), proc() {dec_r64(Register64.R15)})
}

@(test)
test_neg_r64 :: proc(t: ^testing.T) {
	// Test all registers
	test_equal(t, "neg rax", asm_to_bytes("neg rax"), proc() {neg_r64(Register64.RAX)})
	test_equal(t, "neg rcx", asm_to_bytes("neg rcx"), proc() {neg_r64(Register64.RCX)})
	test_equal(t, "neg r8", asm_to_bytes("neg r8"), proc() {neg_r64(Register64.R8)})
	test_equal(t, "neg r15", asm_to_bytes("neg r15"), proc() {neg_r64(Register64.R15)})
}


@(test)
test_and_r64_r64 :: proc(t: ^testing.T) {
	test_equal(
		t,
		"and rax, rbx",
		asm_to_bytes("and rax, rbx"),
		proc() {and_r64_r64(Register64.RAX, Register64.RBX)},
	)
	test_equal(
		t,
		"and r8, r9",
		asm_to_bytes("and r8, r9"),
		proc() {and_r64_r64(Register64.R8, Register64.R9)},
	)
}

@(test)
test_or_r64_r64 :: proc(t: ^testing.T) {
	test_equal(
		t,
		"or rax, rbx",
		asm_to_bytes("or rax, rbx"),
		proc() {or_r64_r64(Register64.RAX, Register64.RBX)},
	)
	test_equal(
		t,
		"or r8, r9",
		asm_to_bytes("or r8, r9"),
		proc() {or_r64_r64(Register64.R8, Register64.R9)},
	)
}

@(test)
test_xor_r64_r64 :: proc(t: ^testing.T) {
	test_equal(
		t,
		"xor rax, rbx",
		asm_to_bytes("xor rax, rbx"),
		proc() {xor_r64_r64(Register64.RAX, Register64.RBX)},
	)
	test_equal(
		t,
		"xor r8, r9",
		asm_to_bytes("xor r8, r9"),
		proc() {xor_r64_r64(Register64.R8, Register64.R9)},
	)
}

@(test)
test_not_r64 :: proc(t: ^testing.T) {
	test_equal(t, "not rax", asm_to_bytes("not rax"), proc() {not_r64(Register64.RAX)})
	test_equal(t, "not r8", asm_to_bytes("not r8"), proc() {not_r64(Register64.R8)})
}

@(test)
test_shl_r64_imm8 :: proc(t: ^testing.T) {
	test_equal(
		t,
		"shl rax, 5",
		asm_to_bytes("shl rax, 5"),
		proc() {shl_r64_imm8(Register64.RAX, 5)},
	)
}

@(test)
test_shr_r64_imm8 :: proc(t: ^testing.T) {
	test_equal(
		t,
		"shr rax, 3",
		asm_to_bytes("shr rax, 3"),
		proc() {shr_r64_imm8(Register64.RAX, 3)},
	)
}

@(test)
test_rol_r64_imm8 :: proc(t: ^testing.T) {
	test_equal(
		t,
		"rol rax, 7",
		asm_to_bytes("rol rax, 7"),
		proc() {rol_r64_imm8(Register64.RAX, 7)},
	)
}

@(test)
test_ror_r64_imm8 :: proc(t: ^testing.T) {
	test_equal(
		t,
		"ror rax, 2",
		asm_to_bytes("ror rax, 2"),
		proc() {ror_r64_imm8(Register64.RAX, 2)},
	)
}

// ===== TESTS FOR BIT MANIPULATION INSTRUCTIONS =====
@(test)
test_shld_r64_r64_imm8 :: proc(t: ^testing.T) {
	test_equal(
		t,
		"shld rax, rbx, 8",
		asm_to_bytes("shld rax, rbx, 8"),
		proc() {shld_r64_r64_imm8(Register64.RAX, Register64.RBX, 8)},
	)
	test_equal(
		t,
		"shld rcx, rdx, 16",
		asm_to_bytes("shld rcx, rdx, 16"),
		proc() {shld_r64_r64_imm8(Register64.RCX, Register64.RDX, 16)},
	)
	test_equal(
		t,
		"shld r8, r9, 1",
		asm_to_bytes("shld r8, r9, 1"),
		proc() {shld_r64_r64_imm8(Register64.R8, Register64.R9, 1)},
	)
	test_equal(
		t,
		"shld r15, r14, 31",
		asm_to_bytes("shld r15, r14, 31"),
		proc() {shld_r64_r64_imm8(Register64.R15, Register64.R14, 31)},
	)
}

@(test)
test_shrd_r64_r64_imm8 :: proc(t: ^testing.T) {
	test_equal(
		t,
		"shrd rax, rbx, 8",
		asm_to_bytes("shrd rax, rbx, 8"),
		proc() {shrd_r64_r64_imm8(Register64.RAX, Register64.RBX, 8)},
	)
	test_equal(
		t,
		"shrd rcx, rdx, 16",
		asm_to_bytes("shrd rcx, rdx, 16"),
		proc() {shrd_r64_r64_imm8(Register64.RCX, Register64.RDX, 16)},
	)
	test_equal(
		t,
		"shrd r8, r9, 1",
		asm_to_bytes("shrd r8, r9, 1"),
		proc() {shrd_r64_r64_imm8(Register64.R8, Register64.R9, 1)},
	)
	test_equal(
		t,
		"shrd r15, r14, 31",
		asm_to_bytes("shrd r15, r14, 31"),
		proc() {shrd_r64_r64_imm8(Register64.R15, Register64.R14, 31)},
	)
}

@(test)
test_bt_r64_r64 :: proc(t: ^testing.T) {
	test_equal(
		t,
		"bt rax, rbx",
		asm_to_bytes("bt rax, rbx"),
		proc() {bt_r64_r64(Register64.RAX, Register64.RBX)},
	)
	test_equal(
		t,
		"bt rcx, rdx",
		asm_to_bytes("bt rcx, rdx"),
		proc() {bt_r64_r64(Register64.RCX, Register64.RDX)},
	)
	test_equal(
		t,
		"bt r8, r9",
		asm_to_bytes("bt r8, r9"),
		proc() {bt_r64_r64(Register64.R8, Register64.R9)},
	)
	test_equal(
		t,
		"bt r15, r14",
		asm_to_bytes("bt r15, r14"),
		proc() {bt_r64_r64(Register64.R15, Register64.R14)},
	)
}

@(test)
test_bts_r64_r64 :: proc(t: ^testing.T) {
	test_equal(
		t,
		"bts rax, rbx",
		asm_to_bytes("bts rax, rbx"),
		proc() {bts_r64_r64(Register64.RAX, Register64.RBX)},
	)
	test_equal(
		t,
		"bts rcx, rdx",
		asm_to_bytes("bts rcx, rdx"),
		proc() {bts_r64_r64(Register64.RCX, Register64.RDX)},
	)
	test_equal(
		t,
		"bts r8, r9",
		asm_to_bytes("bts r8, r9"),
		proc() {bts_r64_r64(Register64.R8, Register64.R9)},
	)
	test_equal(
		t,
		"bts r15, r14",
		asm_to_bytes("bts r15, r14"),
		proc() {bts_r64_r64(Register64.R15, Register64.R14)},
	)
}

@(test)
test_btr_r64_r64 :: proc(t: ^testing.T) {
	test_equal(
		t,
		"btr rax, rbx",
		asm_to_bytes("btr rax, rbx"),
		proc() {btr_r64_r64(Register64.RAX, Register64.RBX)},
	)
	test_equal(
		t,
		"btr rcx, rdx",
		asm_to_bytes("btr rcx, rdx"),
		proc() {btr_r64_r64(Register64.RCX, Register64.RDX)},
	)
	test_equal(
		t,
		"btr r8, r9",
		asm_to_bytes("btr r8, r9"),
		proc() {btr_r64_r64(Register64.R8, Register64.R9)},
	)
	test_equal(
		t,
		"btr r15, r14",
		asm_to_bytes("btr r15, r14"),
		proc() {btr_r64_r64(Register64.R15, Register64.R14)},
	)
}

@(test)
test_btc_r64_r64 :: proc(t: ^testing.T) {
	test_equal(
		t,
		"btc rax, rbx",
		asm_to_bytes("btc rax, rbx"),
		proc() {btc_r64_r64(Register64.RAX, Register64.RBX)},
	)
	test_equal(
		t,
		"btc rcx, rdx",
		asm_to_bytes("btc rcx, rdx"),
		proc() {btc_r64_r64(Register64.RCX, Register64.RDX)},
	)
	test_equal(
		t,
		"btc r8, r9",
		asm_to_bytes("btc r8, r9"),
		proc() {btc_r64_r64(Register64.R8, Register64.R9)},
	)
	test_equal(
		t,
		"btc r15, r14",
		asm_to_bytes("btc r15, r14"),
		proc() {btc_r64_r64(Register64.R15, Register64.R14)},
	)
}

@(test)
test_bsf_r64_r64 :: proc(t: ^testing.T) {
	test_equal(
		t,
		"bsf rax, rbx",
		asm_to_bytes("bsf rax, rbx"),
		proc() {bsf_r64_r64(Register64.RAX, Register64.RBX)},
	)
	test_equal(
		t,
		"bsf rcx, rdx",
		asm_to_bytes("bsf rcx, rdx"),
		proc() {bsf_r64_r64(Register64.RCX, Register64.RDX)},
	)
	test_equal(
		t,
		"bsf r8, r9",
		asm_to_bytes("bsf r8, r9"),
		proc() {bsf_r64_r64(Register64.R8, Register64.R9)},
	)
	test_equal(
		t,
		"bsf r15, r14",
		asm_to_bytes("bsf r15, r14"),
		proc() {bsf_r64_r64(Register64.R15, Register64.R14)},
	)
}

@(test)
test_bsr_r64_r64 :: proc(t: ^testing.T) {
	test_equal(
		t,
		"bsr rax, rbx",
		asm_to_bytes("bsr rax, rbx"),
		proc() {bsr_r64_r64(Register64.RAX, Register64.RBX)},
	)
	test_equal(
		t,
		"bsr rcx, rdx",
		asm_to_bytes("bsr rcx, rdx"),
		proc() {bsr_r64_r64(Register64.RCX, Register64.RDX)},
	)
	test_equal(
		t,
		"bsr r8, r9",
		asm_to_bytes("bsr r8, r9"),
		proc() {bsr_r64_r64(Register64.R8, Register64.R9)},
	)
	test_equal(
		t,
		"bsr r15, r14",
		asm_to_bytes("bsr r15, r14"),
		proc() {bsr_r64_r64(Register64.R15, Register64.R14)},
	)
}

@(test)
test_popcnt_r64_r64 :: proc(t: ^testing.T) {
	test_equal(
		t,
		"popcnt rax, rbx",
		asm_to_bytes("popcnt rax, rbx"),
		proc() {popcnt_r64_r64(Register64.RAX, Register64.RBX)},
	)
	test_equal(
		t,
		"popcnt rcx, rdx",
		asm_to_bytes("popcnt rcx, rdx"),
		proc() {popcnt_r64_r64(Register64.RCX, Register64.RDX)},
	)
	test_equal(
		t,
		"popcnt r8, r9",
		asm_to_bytes("popcnt r8, r9"),
		proc() {popcnt_r64_r64(Register64.R8, Register64.R9)},
	)
	test_equal(
		t,
		"popcnt r15, r14",
		asm_to_bytes("popcnt r15, r14"),
		proc() {popcnt_r64_r64(Register64.R15, Register64.R14)},
	)
}


@(test)
test_lzcnt_r64_r64 :: proc(t: ^testing.T) {
	test_equal(
		t,
		"lzcnt rax, rbx",
		asm_to_bytes("lzcnt rax, rbx"),
		proc() {lzcnt_r64_r64(Register64.RAX, Register64.RBX)},
	)
	test_equal(
		t,
		"lzcnt rcx, rdx",
		asm_to_bytes("lzcnt rcx, rdx"),
		proc() {lzcnt_r64_r64(Register64.RCX, Register64.RDX)},
	)
	test_equal(
		t,
		"lzcnt r8, r9",
		asm_to_bytes("lzcnt r8, r9"),
		proc() {lzcnt_r64_r64(Register64.R8, Register64.R9)},
	)
	test_equal(
		t,
		"lzcnt r15, r14",
		asm_to_bytes("lzcnt r15, r14"),
		proc() {lzcnt_r64_r64(Register64.R15, Register64.R14)},
	)
}

@(test)
test_tzcnt_r64_r64 :: proc(t: ^testing.T) {
	test_equal(
		t,
		"tzcnt rax, rbx",
		asm_to_bytes("tzcnt rax, rbx"),
		proc() {tzcnt_r64_r64(Register64.RAX, Register64.RBX)},
	)
	test_equal(
		t,
		"tzcnt rcx, rdx",
		asm_to_bytes("tzcnt rcx, rdx"),
		proc() {tzcnt_r64_r64(Register64.RCX, Register64.RDX)},
	)
	test_equal(
		t,
		"tzcnt r8, r9",
		asm_to_bytes("tzcnt r8, r9"),
		proc() {tzcnt_r64_r64(Register64.R8, Register64.R9)},
	)
	test_equal(
		t,
		"tzcnt r15, r14",
		asm_to_bytes("tzcnt r15, r14"),
		proc() {tzcnt_r64_r64(Register64.R15, Register64.R14)},
	)
}

@(test)
test_pext_r64_r64_r64 :: proc(t: ^testing.T) {
	test_equal(
		t,
		"pext rax, rbx, rcx",
		asm_to_bytes("pext rax, rbx, rcx"),
		proc() {pext_r64_r64_r64(Register64.RAX, Register64.RBX, Register64.RCX)},
	)
	test_equal(
		t,
		"pext rdx, rsi, rdi",
		asm_to_bytes("pext rdx, rsi, rdi"),
		proc() {pext_r64_r64_r64(Register64.RDX, Register64.RSI, Register64.RDI)},
	)
	test_equal(
		t,
		"pext r8, r9, r10",
		asm_to_bytes("pext r8, r9, r10"),
		proc() {pext_r64_r64_r64(Register64.R8, Register64.R9, Register64.R10)},
	)
	test_equal(
		t,
		"pext r15, r14, r13",
		asm_to_bytes("pext r15, r14, r13"),
		proc() {pext_r64_r64_r64(Register64.R15, Register64.R14, Register64.R13)},
	)
}

@(test)
test_pdep_r64_r64_r64 :: proc(t: ^testing.T) {
	test_equal(
		t,
		"pdep rax, rbx, rcx",
		asm_to_bytes("pdep rax, rbx, rcx"),
		proc() {pdep_r64_r64_r64(Register64.RAX, Register64.RBX, Register64.RCX)},
	)
	test_equal(
		t,
		"pdep rdx, rsi, rdi",
		asm_to_bytes("pdep rdx, rsi, rdi"),
		proc() {pdep_r64_r64_r64(Register64.RDX, Register64.RSI, Register64.RDI)},
	)
	test_equal(
		t,
		"pdep r8, r9, r10",
		asm_to_bytes("pdep r8, r9, r10"),
		proc() {pdep_r64_r64_r64(Register64.R8, Register64.R9, Register64.R10)},
	)
	test_equal(
		t,
		"pdep r15, r14, r13",
		asm_to_bytes("pdep r15, r14, r13"),
		proc() {pdep_r64_r64_r64(Register64.R15, Register64.R14, Register64.R13)},
	)
}

// ===== TESTS FOR COMPARISON INSTRUCTIONS =====
@(test)
test_cmp_r64_r64 :: proc(t: ^testing.T) {
	test_equal(
		t,
		"cmp rax, rbx",
		asm_to_bytes("cmp rax, rbx"),
		proc() {cmp_r64_r64(Register64.RAX, Register64.RBX)},
	)
	test_equal(
		t,
		"cmp rcx, rdx",
		asm_to_bytes("cmp rcx, rdx"),
		proc() {cmp_r64_r64(Register64.RCX, Register64.RDX)},
	)
	test_equal(
		t,
		"cmp r8, r9",
		asm_to_bytes("cmp r8, r9"),
		proc() {cmp_r64_r64(Register64.R8, Register64.R9)},
	)
	test_equal(
		t,
		"cmp r15, r14",
		asm_to_bytes("cmp r15, r14"),
		proc() {cmp_r64_r64(Register64.R15, Register64.R14)},
	)
}

@(test)
test_cmp_r64_imm32 :: proc(t: ^testing.T) {
	test_equal(
		t,
		"cmp rax, 0x12345678",
		asm_to_bytes("cmp rax, 0x12345678"),
		proc() {cmp_r64_imm32(Register64.RAX, 0x12345678)},
	)
	test_equal(
		t,
		"cmp rbx, 0x42",
		asm_to_bytes("cmp rbx, 0x42"),
		proc() {cmp_r64_imm32(Register64.RBX, 0x42)},
	)
	test_equal(
		t,
		"cmp r8, 0xFFFFFFFF",
		asm_to_bytes("cmp r8, -1"),
		proc() {cmp_r64_imm32(Register64.R8, 0xFFFFFFFF)},
	)
	test_equal(
		t,
		"cmp r15, 0",
		asm_to_bytes("cmp r15, 0"),
		proc() {cmp_r64_imm32(Register64.R15, 0)},
	)
}

@(test)
test_test_r64_r64 :: proc(t: ^testing.T) {
	test_equal(
		t,
		"test rax, rbx",
		asm_to_bytes("test rax, rbx"),
		proc() {test_r64_r64(Register64.RAX, Register64.RBX)},
	)
	test_equal(
		t,
		"test rcx, rdx",
		asm_to_bytes("test rcx, rdx"),
		proc() {test_r64_r64(Register64.RCX, Register64.RDX)},
	)
	test_equal(
		t,
		"test r8, r9",
		asm_to_bytes("test r8, r9"),
		proc() {test_r64_r64(Register64.R8, Register64.R9)},
	)
	test_equal(
		t,
		"test r15, r14",
		asm_to_bytes("test r15, r14"),
		proc() {test_r64_r64(Register64.R15, Register64.R14)},
	)
}

@(test)
test_test_r64_imm32 :: proc(t: ^testing.T) {
	test_equal(
		t,
		"test rax, 0x12345678",
		asm_to_bytes("test rax, 0x12345678"),
		proc() {test_r64_imm32(Register64.RAX, 0x12345678)},
	)
	test_equal(
		t,
		"test rbx, 0x42",
		asm_to_bytes("test rbx, 0x42"),
		proc() {test_r64_imm32(Register64.RBX, 0x42)},
	)
	test_equal(
		t,
		"test r8, 0xFFFFFFFF",
		asm_to_bytes("test r8, -1"),
		proc() {test_r64_imm32(Register64.R8, 0xFFFFFFFF)},
	)
	test_equal(
		t,
		"test r15, 0",
		asm_to_bytes("test r15, 0"),
		proc() {test_r64_imm32(Register64.R15, 0)},
	)
}

// ===== TESTS FOR CONDITIONAL MOVE INSTRUCTIONS =====
@(test)
test_cmove_r64_r64 :: proc(t: ^testing.T) {
	test_equal(
		t,
		"cmove rax, rbx",
		asm_to_bytes("cmove rax, rbx"),
		proc() {cmove_r64_r64(Register64.RAX, Register64.RBX)},
	)
	test_equal(
		t,
		"cmove rcx, rdx",
		asm_to_bytes("cmove rcx, rdx"),
		proc() {cmove_r64_r64(Register64.RCX, Register64.RDX)},
	)
	test_equal(
		t,
		"cmove r8, r9",
		asm_to_bytes("cmove r8, r9"),
		proc() {cmove_r64_r64(Register64.R8, Register64.R9)},
	)
	test_equal(
		t,
		"cmove r15, r14",
		asm_to_bytes("cmove r15, r14"),
		proc() {cmove_r64_r64(Register64.R15, Register64.R14)},
	)
}

@(test)
test_cmovne_r64_r64 :: proc(t: ^testing.T) {
	test_equal(
		t,
		"cmovne rax, rbx",
		asm_to_bytes("cmovne rax, rbx"),
		proc() {cmovne_r64_r64(Register64.RAX, Register64.RBX)},
	)
	test_equal(
		t,
		"cmovne rcx, rdx",
		asm_to_bytes("cmovne rcx, rdx"),
		proc() {cmovne_r64_r64(Register64.RCX, Register64.RDX)},
	)
	test_equal(
		t,
		"cmovne r8, r9",
		asm_to_bytes("cmovne r8, r9"),
		proc() {cmovne_r64_r64(Register64.R8, Register64.R9)},
	)
	test_equal(
		t,
		"cmovne r15, r14",
		asm_to_bytes("cmovne r15, r14"),
		proc() {cmovne_r64_r64(Register64.R15, Register64.R14)},
	)
}

@(test)
test_cmova_r64_r64 :: proc(t: ^testing.T) {
	test_equal(
		t,
		"cmova rax, rbx",
		asm_to_bytes("cmova rax, rbx"),
		proc() {cmova_r64_r64(Register64.RAX, Register64.RBX)},
	)
	test_equal(
		t,
		"cmova rcx, rdx",
		asm_to_bytes("cmova rcx, rdx"),
		proc() {cmova_r64_r64(Register64.RCX, Register64.RDX)},
	)
	test_equal(
		t,
		"cmova r8, r9",
		asm_to_bytes("cmova r8, r9"),
		proc() {cmova_r64_r64(Register64.R8, Register64.R9)},
	)
	test_equal(
		t,
		"cmova r15, r14",
		asm_to_bytes("cmova r15, r14"),
		proc() {cmova_r64_r64(Register64.R15, Register64.R14)},
	)
}

@(test)
test_cmovae_r64_r64 :: proc(t: ^testing.T) {
	test_equal(
		t,
		"cmovae rax, rbx",
		asm_to_bytes("cmovae rax, rbx"),
		proc() {cmovae_r64_r64(Register64.RAX, Register64.RBX)},
	)
	test_equal(
		t,
		"cmovae rcx, rdx",
		asm_to_bytes("cmovae rcx, rdx"),
		proc() {cmovae_r64_r64(Register64.RCX, Register64.RDX)},
	)
	test_equal(
		t,
		"cmovae r8, r9",
		asm_to_bytes("cmovae r8, r9"),
		proc() {cmovae_r64_r64(Register64.R8, Register64.R9)},
	)
	test_equal(
		t,
		"cmovae r15, r14",
		asm_to_bytes("cmovae r15, r14"),
		proc() {cmovae_r64_r64(Register64.R15, Register64.R14)},
	)
}

@(test)
test_cmovb_r64_r64 :: proc(t: ^testing.T) {
	test_equal(
		t,
		"cmovb rax, rbx",
		asm_to_bytes("cmovb rax, rbx"),
		proc() {cmovb_r64_r64(Register64.RAX, Register64.RBX)},
	)
	test_equal(
		t,
		"cmovb rcx, rdx",
		asm_to_bytes("cmovb rcx, rdx"),
		proc() {cmovb_r64_r64(Register64.RCX, Register64.RDX)},
	)
	test_equal(
		t,
		"cmovb r8, r9",
		asm_to_bytes("cmovb r8, r9"),
		proc() {cmovb_r64_r64(Register64.R8, Register64.R9)},
	)
	test_equal(
		t,
		"cmovb r15, r14",
		asm_to_bytes("cmovb r15, r14"),
		proc() {cmovb_r64_r64(Register64.R15, Register64.R14)},
	)
}

@(test)
test_cmovbe_r64_r64 :: proc(t: ^testing.T) {
	test_equal(
		t,
		"cmovbe rax, rbx",
		asm_to_bytes("cmovbe rax, rbx"),
		proc() {cmovbe_r64_r64(Register64.RAX, Register64.RBX)},
	)
	test_equal(
		t,
		"cmovbe rcx, rdx",
		asm_to_bytes("cmovbe rcx, rdx"),
		proc() {cmovbe_r64_r64(Register64.RCX, Register64.RDX)},
	)
	test_equal(
		t,
		"cmovbe r8, r9",
		asm_to_bytes("cmovbe r8, r9"),
		proc() {cmovbe_r64_r64(Register64.R8, Register64.R9)},
	)
	test_equal(
		t,
		"cmovbe r15, r14",
		asm_to_bytes("cmovbe r15, r14"),
		proc() {cmovbe_r64_r64(Register64.R15, Register64.R14)},
	)
}

// ===== TESTS FOR CONTROL FLOW INSTRUCTIONS =====
@(test)
test_jmp_rel32 :: proc(t: ^testing.T) {
	test_equal(t, "jmp 0x12345678", asm_to_bytes("jmp 0x12345678"), proc() {jmp_rel32(0x12345678)})
	test_equal(t, "jmp 0", asm_to_bytes("jmp 0"), proc() {jmp_rel32(0)})
	test_equal(t, "jmp -0x42", asm_to_bytes("jmp -0x42"), proc() {jmp_rel32(-0x42)})
	test_equal(t, "jmp 0x7FFFFFFF", asm_to_bytes("jmp 0x7FFFFFFF"), proc() {jmp_rel32(0x7FFFFFFF)})
}

@(test)
test_jmp_r64 :: proc(t: ^testing.T) {
	test_equal(t, "jmp rax", asm_to_bytes("jmp rax"), proc() {jmp_r64(Register64.RAX)})
	test_equal(t, "jmp rbx", asm_to_bytes("jmp rbx"), proc() {jmp_r64(Register64.RBX)})
	test_equal(t, "jmp r8", asm_to_bytes("jmp r8"), proc() {jmp_r64(Register64.R8)})
	test_equal(t, "jmp r15", asm_to_bytes("jmp r15"), proc() {jmp_r64(Register64.R15)})
}

@(test)
test_jmp_m64 :: proc(t: ^testing.T) {
	test_equal(
		t,
		"jmp QWORD PTR [rip+0x12345678]",
		asm_to_bytes("jmp QWORD PTR [rip+0x12345678]"),
		proc() {jmp_m64(0x12345678)},
	)
	test_equal(
		t,
		"jmp QWORD PTR [rip+0]",
		asm_to_bytes("jmp QWORD PTR [rip+0]"),
		proc() {jmp_m64(0)},
	)
	test_equal(
		t,
		"jmp QWORD PTR [rip+0x1000]",
		asm_to_bytes("jmp QWORD PTR [rip+0x1000]"),
		proc() {jmp_m64(0x1000)},
	)
}

@(test)
test_call_rel32 :: proc(t: ^testing.T) {
	test_equal(
		t,
		"call 0x12345678",
		asm_to_bytes("call 0x12345678"),
		proc() {call_rel32(0x12345678)},
	)
	test_equal(t, "call 0", asm_to_bytes("call 0"), proc() {call_rel32(0)})
	test_equal(t, "call -0x42", asm_to_bytes("call -0x42"), proc() {call_rel32(-0x42)})
	test_equal(
		t,
		"call 0x7FFFFFFF",
		asm_to_bytes("call 0x7FFFFFFF"),
		proc() {call_rel32(0x7FFFFFFF)},
	)
}

@(test)
test_call_r64 :: proc(t: ^testing.T) {
	test_equal(t, "call rax", asm_to_bytes("call rax"), proc() {call_r64(Register64.RAX)})
	test_equal(t, "call rbx", asm_to_bytes("call rbx"), proc() {call_r64(Register64.RBX)})
	test_equal(t, "call r8", asm_to_bytes("call r8"), proc() {call_r64(Register64.R8)})
	test_equal(t, "call r15", asm_to_bytes("call r15"), proc() {call_r64(Register64.R15)})

}

@(test)
test_call_m64 :: proc(t: ^testing.T) {
	test_equal(
		t,
		"call QWORD PTR [rip+0x12345678]",
		asm_to_bytes("call QWORD PTR [rip+0x12345678]"),
		proc() {call_m64(0x12345678)},
	)
	test_equal(
		t,
		"call QWORD PTR [rip+0]",
		asm_to_bytes("call QWORD PTR [rip+0]"),
		proc() {call_m64(0)},
	)
	test_equal(
		t,
		"call QWORD PTR [rip+0x1000]",
		asm_to_bytes("call QWORD PTR [rip+0x1000]"),
		proc() {call_m64(0x1000)},
	)
}

@(test)
test_ret :: proc(t: ^testing.T) {
	test_equal(t, "ret", asm_to_bytes("ret"), proc() {ret()})
}

@(test)
test_conditional_jumps :: proc(t: ^testing.T) {
	test_equal(t, "je 0x1234", asm_to_bytes("je 0x1234"), proc() {je_rel32(0x1234)})
	test_equal(t, "jne 0x4321", asm_to_bytes("jne 0x4321"), proc() {jne_rel32(0x4321)})
	test_equal(t, "jg 0", asm_to_bytes("jg 0"), proc() {jg_rel32(0)})
	test_equal(t, "jl -0x42", asm_to_bytes("jl -0x42"), proc() {jl_rel32(-0x42)})
	test_equal(t, "jge 0x7FFFFFFF", asm_to_bytes("jge 0x7FFFFFFF"), proc() {jge_rel32(0x7FFFFFFF)})
	test_equal(t, "jle 0x12345678", asm_to_bytes("jle 0x12345678"), proc() {jle_rel32(0x12345678)})
	test_equal(t, "ja 0x42", asm_to_bytes("ja 0x42"), proc() {ja_rel32(0x42)})
	test_equal(t, "jae 0x1000", asm_to_bytes("jae 0x1000"), proc() {jae_rel32(0x1000)})
	test_equal(t, "jb 0x2000", asm_to_bytes("jb 0x2000"), proc() {jb_rel32(0x2000)})
	test_equal(t, "jbe 0x3000", asm_to_bytes("jbe 0x3000"), proc() {jbe_rel32(0x3000)})
}

@(test)
test_loop_control :: proc(t: ^testing.T) {
	test_equal(t, "loop 0x10", asm_to_bytes("loop 0x10"), proc() {loop_rel8(0x10)})
	test_equal(t, "loope 0x20", asm_to_bytes("loope 0x20"), proc() {loope_rel8(0x20)})
	test_equal(t, "loopne 0x30", asm_to_bytes("loopne 0x30"), proc() {loopne_rel8(0x30)})
	test_equal(t, "jecxz 0x40", asm_to_bytes("jecxz 0x40"), proc() {jecxz_rel8(0x40)})
	test_equal(t, "loop -0x10", asm_to_bytes("loop -0x10"), proc() {loop_rel8(-0x10)})
}

@(test)
test_setcc_r8 :: proc(t: ^testing.T) {
	// Test with various condition codes
	test_equal(t, "sete al", asm_to_bytes("sete al"), proc() {setcc_r8(0, 0x4)}) 	// sete = 0x94 (0x90 + 0x4)
	test_equal(t, "setne cl", asm_to_bytes("setne cl"), proc() {setcc_r8(1, 0x5)}) 	// setne = 0x95 (0x90 + 0x5)
	test_equal(t, "seta dl", asm_to_bytes("seta dl"), proc() {setcc_r8(2, 0x7)}) 	// seta = 0x97 (0x90 + 0x7)
	test_equal(t, "setb bl", asm_to_bytes("setb bl"), proc() {setcc_r8(3, 0x2)}) 	// setb = 0x92 (0x90 + 0x2)
}

@(test)
test_endbr64 :: proc(t: ^testing.T) {
	test_equal(t, "endbr64", asm_to_bytes("endbr64"), proc() {endbr64()})
}


// ===== TESTS FOR STACK MANIPULATION =====
@(test)
test_push_r64 :: proc(t: ^testing.T) {
	test_equal(t, "push rax", asm_to_bytes("push rax"), proc() {push_r64(Register64.RAX)})
	test_equal(t, "push rbx", asm_to_bytes("push rbx"), proc() {push_r64(Register64.RBX)})
	test_equal(t, "push r8", asm_to_bytes("push r8"), proc() {push_r64(Register64.R8)})
	test_equal(t, "push r15", asm_to_bytes("push r15"), proc() {push_r64(Register64.R15)})
}

@(test)
test_pop_r64 :: proc(t: ^testing.T) {
	test_equal(t, "pop rax", asm_to_bytes("pop rax"), proc() {pop_r64(Register64.RAX)})
	test_equal(t, "pop rbx", asm_to_bytes("pop rbx"), proc() {pop_r64(Register64.RBX)})
	test_equal(t, "pop r8", asm_to_bytes("pop r8"), proc() {pop_r64(Register64.R8)})
	test_equal(t, "pop r15", asm_to_bytes("pop r15"), proc() {pop_r64(Register64.R15)})
}

@(test)
test_pushfq_popfq :: proc(t: ^testing.T) {
	test_equal(t, "pushfq", asm_to_bytes("pushfq"), proc() {pushfq()})
	test_equal(t, "popfq", asm_to_bytes("popfq"), proc() {popfq()})
}

// --- SSE/AVX Data Movement Tests ---
@(test)
test_movd_xmm_r64 :: proc(t: ^testing.T) {
	test_equal(
		t,
		"movd xmm0, rax",
		asm_to_bytes("movd xmm0, rax"),
		proc() {movd_xmm_r64(XMMRegister.XMM0, Register64.RAX)},
	)
	test_equal(
		t,
		"movd xmm7, rdx",
		asm_to_bytes("movd xmm7, rdx"),
		proc() {movd_xmm_r64(XMMRegister.XMM7, Register64.RDX)},
	)
	test_equal(
		t,
		"movd xmm15, r15",
		asm_to_bytes("movd xmm15, r15"),
		proc() {movd_xmm_r64(XMMRegister.XMM15, Register64.R15)},
	)
}

@(test)
test_movd_r64_xmm :: proc(t: ^testing.T) {
	test_equal(
		t,
		"movd rax, xmm0",
		asm_to_bytes("movd rax, xmm0"),
		proc() {movd_r64_xmm(Register64.RAX, XMMRegister.XMM0)},
	)
	test_equal(
		t,
		"movd rdx, xmm7",
		asm_to_bytes("movd rdx, xmm7"),
		proc() {movd_r64_xmm(Register64.RDX, XMMRegister.XMM7)},
	)
	test_equal(
		t,
		"movd r15, xmm15",
		asm_to_bytes("movd r15, xmm15"),
		proc() {movd_r64_xmm(Register64.R15, XMMRegister.XMM15)},
	)
}

@(test)
test_movups_xmm_m128 :: proc(t: ^testing.T) {
	test_equal(
		t,
		"movups xmm0, [0x1000]",
		asm_to_bytes("movups xmm0, [0x1000]"),
		proc() {movups_xmm_m128(XMMRegister.XMM0, 0x1000)},
	)
	test_equal(
		t,
		"movups xmm7, [0x0]",
		asm_to_bytes("movups xmm7, [0x0]"),
		proc() {movups_xmm_m128(XMMRegister.XMM7, 0x0)},
	)
	test_equal(
		t,
		"movups xmm15, [0x8000]",
		asm_to_bytes("movups xmm15, [0x8000]"),
		proc() {movups_xmm_m128(XMMRegister.XMM15, 0x8000)},
	)
}

@(test)
test_movdqu_xmm_m128 :: proc(t: ^testing.T) {
	test_equal(
		t,
		"movdqu xmm0, [0x1000]",
		asm_to_bytes("movdqu xmm0, [0x1000]"),
		proc() {movdqu_xmm_m128(XMMRegister.XMM0, 0x1000)},
	)
	test_equal(
		t,
		"movdqu xmm7, [0x0]",
		asm_to_bytes("movdqu xmm7, [0x0]"),
		proc() {movdqu_xmm_m128(XMMRegister.XMM7, 0x0)},
	)
	test_equal(
		t,
		"movdqu xmm15, [0x8000]",
		asm_to_bytes("movdqu xmm15, [0x8000]"),
		proc() {movdqu_xmm_m128(XMMRegister.XMM15, 0x8000)},
	)
}

// --- SSE/AVX Arithmetic Tests ---
@(test)
test_addps_xmm_xmm :: proc(t: ^testing.T) {
	test_equal(
		t,
		"addps xmm0, xmm1",
		asm_to_bytes("addps xmm0, xmm1"),
		proc() {addps_xmm_xmm(XMMRegister.XMM0, XMMRegister.XMM1)},
	)
	test_equal(
		t,
		"addps xmm7, xmm7",
		asm_to_bytes("addps xmm7, xmm7"),
		proc() {addps_xmm_xmm(XMMRegister.XMM7, XMMRegister.XMM7)},
	)
	test_equal(
		t,
		"addps xmm15, xmm8",
		asm_to_bytes("addps xmm15, xmm8"),
		proc() {addps_xmm_xmm(XMMRegister.XMM15, XMMRegister.XMM8)},
	)
}

@(test)
test_mulps_xmm_xmm :: proc(t: ^testing.T) {
	test_equal(
		t,
		"mulps xmm0, xmm1",
		asm_to_bytes("mulps xmm0, xmm1"),
		proc() {mulps_xmm_xmm(XMMRegister.XMM0, XMMRegister.XMM1)},
	)
	test_equal(
		t,
		"mulps xmm7, xmm7",
		asm_to_bytes("mulps xmm7, xmm7"),
		proc() {mulps_xmm_xmm(XMMRegister.XMM7, XMMRegister.XMM7)},
	)
	test_equal(
		t,
		"mulps xmm15, xmm8",
		asm_to_bytes("mulps xmm15, xmm8"),
		proc() {mulps_xmm_xmm(XMMRegister.XMM15, XMMRegister.XMM8)},
	)
}

@(test)
test_divps_xmm_xmm :: proc(t: ^testing.T) {
	test_equal(
		t,
		"divps xmm0, xmm1",
		asm_to_bytes("divps xmm0, xmm1"),
		proc() {divps_xmm_xmm(XMMRegister.XMM0, XMMRegister.XMM1)},
	)
	test_equal(
		t,
		"divps xmm7, xmm7",
		asm_to_bytes("divps xmm7, xmm7"),
		proc() {divps_xmm_xmm(XMMRegister.XMM7, XMMRegister.XMM7)},
	)
	test_equal(
		t,
		"divps xmm15, xmm8",
		asm_to_bytes("divps xmm15, xmm8"),
		proc() {divps_xmm_xmm(XMMRegister.XMM15, XMMRegister.XMM8)},
	)
}

@(test)
test_sqrtps_xmm :: proc(t: ^testing.T) {
	test_equal(
		t,
		"sqrtps xmm0, xmm0",
		asm_to_bytes("sqrtps xmm0, xmm0"),
		proc() {sqrtps_xmm(XMMRegister.XMM0)},
	)
	test_equal(
		t,
		"sqrtps xmm7, xmm7",
		asm_to_bytes("sqrtps xmm7, xmm7"),
		proc() {sqrtps_xmm(XMMRegister.XMM7)},
	)
	test_equal(
		t,
		"sqrtps xmm15, xmm15",
		asm_to_bytes("sqrtps xmm15, xmm15"),
		proc() {sqrtps_xmm(XMMRegister.XMM15)},
	)
}

// --- SSE/AVX Comparison Tests ---
@(test)
test_cmpps_xmm_xmm_imm8 :: proc(t: ^testing.T) {
	test_equal(
		t,
		"cmpps xmm0, xmm1, 0",
		asm_to_bytes("cmpps xmm0, xmm1, 0"),
		proc() {cmpps_xmm_xmm_imm8(XMMRegister.XMM0, XMMRegister.XMM1, 0)},
	)
	test_equal(
		t,
		"cmpps xmm7, xmm7, 4",
		asm_to_bytes("cmpps xmm7, xmm7, 4"),
		proc() {cmpps_xmm_xmm_imm8(XMMRegister.XMM7, XMMRegister.XMM7, 4)},
	)
	test_equal(
		t,
		"cmpps xmm15, xmm8, 7",
		asm_to_bytes("cmpps xmm15, xmm8, 7"),
		proc() {cmpps_xmm_xmm_imm8(XMMRegister.XMM15, XMMRegister.XMM8, 7)},
	)
}

@(test)
test_cmpeqps_xmm_xmm :: proc(t: ^testing.T) {
	test_equal(
		t,
		"cmpeqps xmm0, xmm1",
		asm_to_bytes("cmpeqps xmm0, xmm1"),
		proc() {cmpeqps_xmm_xmm(XMMRegister.XMM0, XMMRegister.XMM1)},
	)
	test_equal(
		t,
		"cmpeqps xmm7, xmm7",
		asm_to_bytes("cmpeqps xmm7, xmm7"),
		proc() {cmpeqps_xmm_xmm(XMMRegister.XMM7, XMMRegister.XMM7)},
	)
	test_equal(
		t,
		"cmpeqps xmm15, xmm8",
		asm_to_bytes("cmpeqps xmm15, xmm8"),
		proc() {cmpeqps_xmm_xmm(XMMRegister.XMM15, XMMRegister.XMM8)},
	)
}

@(test)
test_cmpneqps_xmm_xmm :: proc(t: ^testing.T) {
	test_equal(
		t,
		"cmpneqps xmm0, xmm1",
		asm_to_bytes("cmpneqps xmm0, xmm1"),
		proc() {cmpneqps_xmm_xmm(XMMRegister.XMM0, XMMRegister.XMM1)},
	)
	test_equal(
		t,
		"cmpneqps xmm7, xmm7",
		asm_to_bytes("cmpneqps xmm7, xmm7"),
		proc() {cmpneqps_xmm_xmm(XMMRegister.XMM7, XMMRegister.XMM7)},
	)
	test_equal(
		t,
		"cmpneqps xmm15, xmm8",
		asm_to_bytes("cmpneqps xmm15, xmm8"),
		proc() {cmpneqps_xmm_xmm(XMMRegister.XMM15, XMMRegister.XMM8)},
	)
}

// --- AVX FMA (Fused Multiply-Add) Tests ---
@(test)
test_vfmadd132ps_xmm_xmm_xmm :: proc(t: ^testing.T) {
	test_equal(
		t,
		"vfmadd132ps xmm0, xmm1, xmm2",
		asm_to_bytes("vfmadd132ps xmm0, xmm1, xmm2"),
		proc() {vfmadd132ps_xmm_xmm_xmm(XMMRegister.XMM0, XMMRegister.XMM1, XMMRegister.XMM2)},
	)
	test_equal(
		t,
		"vfmadd132ps xmm7, xmm7, xmm7",
		asm_to_bytes("vfmadd132ps xmm7, xmm7, xmm7"),
		proc() {vfmadd132ps_xmm_xmm_xmm(XMMRegister.XMM7, XMMRegister.XMM7, XMMRegister.XMM7)},
	)
	test_equal(
		t,
		"vfmadd132ps xmm15, xmm14, xmm13",
		asm_to_bytes("vfmadd132ps xmm15, xmm14, xmm13"),
		proc() {vfmadd132ps_xmm_xmm_xmm(XMMRegister.XMM15, XMMRegister.XMM14, XMMRegister.XMM13)},
	)
}

@(test)
test_vfmadd213ps_xmm_xmm_xmm :: proc(t: ^testing.T) {
	test_equal(
		t,
		"vfmadd213ps xmm0, xmm1, xmm2",
		asm_to_bytes("vfmadd213ps xmm0, xmm1, xmm2"),
		proc() {vfmadd213ps_xmm_xmm_xmm(XMMRegister.XMM0, XMMRegister.XMM1, XMMRegister.XMM2)},
	)
	test_equal(
		t,
		"vfmadd213ps xmm7, xmm7, xmm7",
		asm_to_bytes("vfmadd213ps xmm7, xmm7, xmm7"),
		proc() {vfmadd213ps_xmm_xmm_xmm(XMMRegister.XMM7, XMMRegister.XMM7, XMMRegister.XMM7)},
	)
	test_equal(
		t,
		"vfmadd213ps xmm15, xmm14, xmm13",
		asm_to_bytes("vfmadd213ps xmm15, xmm14, xmm13"),
		proc() {vfmadd213ps_xmm_xmm_xmm(XMMRegister.XMM15, XMMRegister.XMM14, XMMRegister.XMM13)},
	)
}

@(test)
test_vfmadd231ps_xmm_xmm_xmm :: proc(t: ^testing.T) {
	test_equal(
		t,
		"vfmadd231ps xmm0, xmm1, xmm2",
		asm_to_bytes("vfmadd231ps xmm0, xmm1, xmm2"),
		proc() {vfmadd231ps_xmm_xmm_xmm(XMMRegister.XMM0, XMMRegister.XMM1, XMMRegister.XMM2)},
	)
	test_equal(
		t,
		"vfmadd231ps xmm7, xmm7, xmm7",
		asm_to_bytes("vfmadd231ps xmm7, xmm7, xmm7"),
		proc() {vfmadd231ps_xmm_xmm_xmm(XMMRegister.XMM7, XMMRegister.XMM7, XMMRegister.XMM7)},
	)
	test_equal(
		t,
		"vfmadd231ps xmm15, xmm14, xmm13",
		asm_to_bytes("vfmadd231ps xmm15, xmm14, xmm13"),
		proc() {vfmadd231ps_xmm_xmm_xmm(XMMRegister.XMM15, XMMRegister.XMM14, XMMRegister.XMM13)},
	)
}

// --- AVX Advanced Operations Tests ---
@(test)
test_vaddps_ymm :: proc(t: ^testing.T) {
	test_equal(
		t,
		"vaddps ymm0, ymm1, ymm2",
		asm_to_bytes("vaddps ymm0, ymm1, ymm2"),
		proc() {vaddps_ymm(XMMRegister.XMM0, XMMRegister.XMM1, XMMRegister.XMM2)},
	)
	test_equal(
		t,
		"vaddps ymm7, ymm7, ymm7",
		asm_to_bytes("vaddps ymm7, ymm7, ymm7"),
		proc() {vaddps_ymm(XMMRegister.XMM7, XMMRegister.XMM7, XMMRegister.XMM7)},
	)
	test_equal(
		t,
		"vaddps ymm15, ymm14, ymm13",
		asm_to_bytes("vaddps ymm15, ymm14, ymm13"),
		proc() {vaddps_ymm(XMMRegister.XMM15, XMMRegister.XMM14, XMMRegister.XMM13)},
	)
}

@(test)
test_vmulps_ymm :: proc(t: ^testing.T) {
	test_equal(
		t,
		"vmulps ymm0, ymm1, ymm2",
		asm_to_bytes("vmulps ymm0, ymm1, ymm2"),
		proc() {vmulps_ymm(XMMRegister.XMM0, XMMRegister.XMM1, XMMRegister.XMM2)},
	)
	test_equal(
		t,
		"vmulps ymm7, ymm7, ymm7",
		asm_to_bytes("vmulps ymm7, ymm7, ymm7"),
		proc() {vmulps_ymm(XMMRegister.XMM7, XMMRegister.XMM7, XMMRegister.XMM7)},
	)
	test_equal(
		t,
		"vmulps ymm15, ymm14, ymm13",
		asm_to_bytes("vmulps ymm15, ymm14, ymm13"),
		proc() {vmulps_ymm(XMMRegister.XMM15, XMMRegister.XMM14, XMMRegister.XMM13)},
	)
}

@(test)
test_vdivps_ymm :: proc(t: ^testing.T) {
	test_equal(
		t,
		"vdivps ymm0, ymm1, ymm2",
		asm_to_bytes("vdivps ymm0, ymm1, ymm2"),
		proc() {vdivps_ymm(XMMRegister.XMM0, XMMRegister.XMM1, XMMRegister.XMM2)},
	)
	test_equal(
		t,
		"vdivps ymm7, ymm7, ymm7",
		asm_to_bytes("vdivps ymm7, ymm7, ymm7"),
		proc() {vdivps_ymm(XMMRegister.XMM7, XMMRegister.XMM7, XMMRegister.XMM7)},
	)
	test_equal(
		t,
		"vdivps ymm15, ymm14, ymm13",
		asm_to_bytes("vdivps ymm15, ymm14, ymm13"),
		proc() {vdivps_ymm(XMMRegister.XMM15, XMMRegister.XMM14, XMMRegister.XMM13)},
	)
}

@(test)
test_vblendps_ymm :: proc(t: ^testing.T) {
	test_equal(
		t,
		"vblendps ymm0, ymm1, ymm2, 0x1",
		asm_to_bytes("vblendps ymm0, ymm1, ymm2, 0x1"),
		proc() {vblendps_ymm(XMMRegister.XMM0, XMMRegister.XMM1, XMMRegister.XMM2, 0x1)},
	)
	test_equal(
		t,
		"vblendps ymm7, ymm7, ymm7, 0xF",
		asm_to_bytes("vblendps ymm7, ymm7, ymm7, 0xF"),
		proc() {vblendps_ymm(XMMRegister.XMM7, XMMRegister.XMM7, XMMRegister.XMM7, 0xF)},
	)
	test_equal(
		t,
		"vblendps ymm15, ymm14, ymm13, 0xAA",
		asm_to_bytes("vblendps ymm15, ymm14, ymm13, 0xAA"),
		proc() {vblendps_ymm(XMMRegister.XMM15, XMMRegister.XMM14, XMMRegister.XMM13, 0xAA)},
	)
}

@(test)
test_vpand_ymm :: proc(t: ^testing.T) {
	test_equal(
		t,
		"vpand ymm0, ymm1, ymm2",
		asm_to_bytes("vpand ymm0, ymm1, ymm2"),
		proc() {vpand_ymm(XMMRegister.XMM0, XMMRegister.XMM1, XMMRegister.XMM2)},
	)
	test_equal(
		t,
		"vpand ymm7, ymm7, ymm7",
		asm_to_bytes("vpand ymm7, ymm7, ymm7"),
		proc() {vpand_ymm(XMMRegister.XMM7, XMMRegister.XMM7, XMMRegister.XMM7)},
	)
	test_equal(
		t,
		"vpand ymm15, ymm14, ymm13",
		asm_to_bytes("vpand ymm15, ymm14, ymm13"),
		proc() {vpand_ymm(XMMRegister.XMM15, XMMRegister.XMM14, XMMRegister.XMM13)},
	)
}

@(test)
test_vpor_ymm :: proc(t: ^testing.T) {
	test_equal(
		t,
		"vpor ymm0, ymm1, ymm2",
		asm_to_bytes("vpor ymm0, ymm1, ymm2"),
		proc() {vpor_ymm(XMMRegister.XMM0, XMMRegister.XMM1, XMMRegister.XMM2)},
	)
	test_equal(
		t,
		"vpor ymm7, ymm7, ymm7",
		asm_to_bytes("vpor ymm7, ymm7, ymm7"),
		proc() {vpor_ymm(XMMRegister.XMM7, XMMRegister.XMM7, XMMRegister.XMM7)},
	)
	test_equal(
		t,
		"vpor ymm15, ymm14, ymm13",
		asm_to_bytes("vpor ymm15, ymm14, ymm13"),
		proc() {vpor_ymm(XMMRegister.XMM15, XMMRegister.XMM14, XMMRegister.XMM13)},
	)
}

@(test)
test_vpxor_ymm :: proc(t: ^testing.T) {
	test_equal(
		t,
		"vpxor ymm0, ymm1, ymm2",
		asm_to_bytes("vpxor ymm0, ymm1, ymm2"),
		proc() {vpxor_ymm(XMMRegister.XMM0, XMMRegister.XMM1, XMMRegister.XMM2)},
	)
	test_equal(
		t,
		"vpxor ymm7, ymm7, ymm7",
		asm_to_bytes("vpxor ymm7, ymm7, ymm7"),
		proc() {vpxor_ymm(XMMRegister.XMM7, XMMRegister.XMM7, XMMRegister.XMM7)},
	)
	test_equal(
		t,
		"vpxor ymm15, ymm14, ymm13",
		asm_to_bytes("vpxor ymm15, ymm14, ymm13"),
		proc() {vpxor_ymm(XMMRegister.XMM15, XMMRegister.XMM14, XMMRegister.XMM13)},
	)
}

@(test)
test_vpternlogd_ymm_ymm_ymm_imm8 :: proc(t: ^testing.T) {
	test_equal(
		t,
		"vpternlogd ymm0, ymm1, ymm2, 0x1",
		asm_to_bytes("vpternlogd ymm0, ymm1, ymm2, 0x1"),
		proc() {vpternlogd_ymm_ymm_ymm_imm8(
				XMMRegister.XMM0,
				XMMRegister.XMM1,
				XMMRegister.XMM2,
				0x1,
			)},
	)
	test_equal(
		t,
		"vpternlogd ymm7, ymm7, ymm7, 0xFF",
		asm_to_bytes("vpternlogd ymm7, ymm7, ymm7, 0xFF"),
		proc() {vpternlogd_ymm_ymm_ymm_imm8(
				XMMRegister.XMM7,
				XMMRegister.XMM7,
				XMMRegister.XMM7,
				0xFF,
			)},
	)
	test_equal(
		t,
		"vpternlogd ymm15, ymm14, ymm13, 0xAA",
		asm_to_bytes("vpternlogd ymm15, ymm14, ymm13, 0xAA"),
		proc() {vpternlogd_ymm_ymm_ymm_imm8(
				XMMRegister.XMM15,
				XMMRegister.XMM14,
				XMMRegister.XMM13,
				0xAA,
			)},
	)
}

@(test)
test_vextracti128_ymm_ymm_imm8 :: proc(t: ^testing.T) {
	test_equal(
		t,
		"vextracti128 xmm0, ymm1, 0x0",
		asm_to_bytes("vextracti128 xmm0, ymm1, 0x0"),
		proc() {vextracti128_ymm_ymm_imm8(XMMRegister.XMM0, XMMRegister.XMM1, 0x0)},
	)
	test_equal(
		t,
		"vextracti128 xmm7, ymm7, 0x1",
		asm_to_bytes("vextracti128 xmm7, ymm7, 0x1"),
		proc() {vextracti128_ymm_ymm_imm8(XMMRegister.XMM7, XMMRegister.XMM7, 0x1)},
	)
	test_equal(
		t,
		"vextracti128 xmm15, ymm8, 0x1",
		asm_to_bytes("vextracti128 xmm15, ymm8, 0x1"),
		proc() {vextracti128_ymm_ymm_imm8(XMMRegister.XMM15, XMMRegister.XMM8, 0x1)},
	)
}

// --- SIMD Integer Operations Tests ---
@(test)
test_pavgb_xmm_xmm :: proc(t: ^testing.T) {
	test_equal(
		t,
		"pavgb xmm0, xmm1",
		asm_to_bytes("pavgb xmm0, xmm1"),
		proc() {pavgb_xmm_xmm(XMMRegister.XMM0, XMMRegister.XMM1)},
	)
	test_equal(
		t,
		"pavgb xmm7, xmm7",
		asm_to_bytes("pavgb xmm7, xmm7"),
		proc() {pavgb_xmm_xmm(XMMRegister.XMM7, XMMRegister.XMM7)},
	)
	test_equal(
		t,
		"pavgb xmm15, xmm8",
		asm_to_bytes("pavgb xmm15, xmm8"),
		proc() {pavgb_xmm_xmm(XMMRegister.XMM15, XMMRegister.XMM8)},
	)
}

@(test)
test_pavgb_ymm_ymm :: proc(t: ^testing.T) {
	test_equal(
		t,
		"vpavgb ymm0, ymm1, ymm2",
		asm_to_bytes("vpavgb ymm0, ymm1, ymm2"),
		proc() {pavgb_ymm_ymm(XMMRegister.XMM0, XMMRegister.XMM2)},
	)
	test_equal(
		t,
		"vpavgb ymm7, ymm7, ymm7",
		asm_to_bytes("vpavgb ymm7, ymm7, ymm7"),
		proc() {pavgb_ymm_ymm(XMMRegister.XMM7, XMMRegister.XMM7)},
	)
	test_equal(
		t,
		"vpavgb ymm15, ymm14, ymm13",
		asm_to_bytes("vpavgb ymm15, ymm14, ymm13"),
		proc() {pavgb_ymm_ymm(XMMRegister.XMM15, XMMRegister.XMM13)},
	)
}

@(test)
test_pmaddwd_xmm_xmm :: proc(t: ^testing.T) {
	test_equal(
		t,
		"pmaddwd xmm0, xmm1",
		asm_to_bytes("pmaddwd xmm0, xmm1"),
		proc() {pmaddwd_xmm_xmm(XMMRegister.XMM0, XMMRegister.XMM1)},
	)
	test_equal(
		t,
		"pmaddwd xmm7, xmm7",
		asm_to_bytes("pmaddwd xmm7, xmm7"),
		proc() {pmaddwd_xmm_xmm(XMMRegister.XMM7, XMMRegister.XMM7)},
	)
	test_equal(
		t,
		"pmaddwd xmm15, xmm8",
		asm_to_bytes("pmaddwd xmm15, xmm8"),
		proc() {pmaddwd_xmm_xmm(XMMRegister.XMM15, XMMRegister.XMM8)},
	)
}

@(test)
test_pmulhuw_xmm_xmm :: proc(t: ^testing.T) {
	test_equal(
		t,
		"pmulhuw xmm0, xmm1",
		asm_to_bytes("pmulhuw xmm0, xmm1"),
		proc() {pmulhuw_xmm_xmm(XMMRegister.XMM0, XMMRegister.XMM1)},
	)
	test_equal(
		t,
		"pmulhuw xmm7, xmm7",
		asm_to_bytes("pmulhuw xmm7, xmm7"),
		proc() {pmulhuw_xmm_xmm(XMMRegister.XMM7, XMMRegister.XMM7)},
	)
	test_equal(
		t,
		"pmulhuw xmm15, xmm8",
		asm_to_bytes("pmulhuw xmm15, xmm8"),
		proc() {pmulhuw_xmm_xmm(XMMRegister.XMM15, XMMRegister.XMM8)},
	)
}

// --- AVX-512 Operations Tests ---
@(test)
test_vaddpd_zmm_zmm_zmm :: proc(t: ^testing.T) {
	test_equal(
		t,
		"vaddpd zmm0, zmm1, zmm2",
		asm_to_bytes("vaddpd zmm0, zmm1, zmm2"),
		proc() {vaddpd_zmm_zmm_zmm(XMMRegister.XMM0, XMMRegister.XMM1, XMMRegister.XMM2)},
	)
	test_equal(
		t,
		"vaddpd zmm7, zmm7, zmm7",
		asm_to_bytes("vaddpd zmm7, zmm7, zmm7"),
		proc() {vaddpd_zmm_zmm_zmm(XMMRegister.XMM7, XMMRegister.XMM7, XMMRegister.XMM7)},
	)
	test_equal(
		t,
		"vaddpd zmm15, zmm14, zmm13",
		asm_to_bytes("vaddpd zmm15, zmm14, zmm13"),
		proc() {vaddpd_zmm_zmm_zmm(XMMRegister.XMM15, XMMRegister.XMM14, XMMRegister.XMM13)},
	)
}

@(test)
test_vsubpd_zmm_zmm_zmm :: proc(t: ^testing.T) {
	test_equal(
		t,
		"vsubpd zmm0, zmm1, zmm2",
		asm_to_bytes("vsubpd zmm0, zmm1, zmm2"),
		proc() {vsubpd_zmm_zmm_zmm(XMMRegister.XMM0, XMMRegister.XMM1, XMMRegister.XMM2)},
	)
	test_equal(
		t,
		"vsubpd zmm7, zmm7, zmm7",
		asm_to_bytes("vsubpd zmm7, zmm7, zmm7"),
		proc() {vsubpd_zmm_zmm_zmm(XMMRegister.XMM7, XMMRegister.XMM7, XMMRegister.XMM7)},
	)
	test_equal(
		t,
		"vsubpd zmm15, zmm14, zmm13",
		asm_to_bytes("vsubpd zmm15, zmm14, zmm13"),
		proc() {vsubpd_zmm_zmm_zmm(XMMRegister.XMM15, XMMRegister.XMM14, XMMRegister.XMM13)},
	)
}

@(test)
test_vmulpd_zmm_zmm_zmm :: proc(t: ^testing.T) {
	test_equal(
		t,
		"vmulpd zmm0, zmm1, zmm2",
		asm_to_bytes("vmulpd zmm0, zmm1, zmm2"),
		proc() {vmulpd_zmm_zmm_zmm(XMMRegister.XMM0, XMMRegister.XMM1, XMMRegister.XMM2)},
	)
	test_equal(
		t,
		"vmulpd zmm7, zmm7, zmm7",
		asm_to_bytes("vmulpd zmm7, zmm7, zmm7"),
		proc() {vmulpd_zmm_zmm_zmm(XMMRegister.XMM7, XMMRegister.XMM7, XMMRegister.XMM7)},
	)
	test_equal(
		t,
		"vmulpd zmm15, zmm14, zmm13",
		asm_to_bytes("vmulpd zmm15, zmm14, zmm13"),
		proc() {vmulpd_zmm_zmm_zmm(XMMRegister.XMM15, XMMRegister.XMM14, XMMRegister.XMM13)},
	)
}

@(test)
test_vdivpd_zmm_zmm_zmm :: proc(t: ^testing.T) {
	test_equal(
		t,
		"vdivpd zmm0, zmm1, zmm2",
		asm_to_bytes("vdivpd zmm0, zmm1, zmm2"),
		proc() {vdivpd_zmm_zmm_zmm(XMMRegister.XMM0, XMMRegister.XMM1, XMMRegister.XMM2)},
	)
	test_equal(
		t,
		"vdivpd zmm7, zmm7, zmm7",
		asm_to_bytes("vdivpd zmm7, zmm7, zmm7"),
		proc() {vdivpd_zmm_zmm_zmm(XMMRegister.XMM7, XMMRegister.XMM7, XMMRegister.XMM7)},
	)
	test_equal(
		t,
		"vdivpd zmm15, zmm14, zmm13",
		asm_to_bytes("vdivpd zmm15, zmm14, zmm13"),
		proc() {vdivpd_zmm_zmm_zmm(XMMRegister.XMM15, XMMRegister.XMM14, XMMRegister.XMM13)},
	)
}

@(test)
test_vgatherdps_xmm :: proc(t: ^testing.T) {
	test_equal(
		t,
		"vgatherdps xmm0, dword ptr [eax+xmm0*1]",
		asm_to_bytes("vgatherdps xmm0, dword ptr [eax+xmm0*1]"),
		proc() {vgatherdps_xmm(XMMRegister.XMM0)},
	)
	test_equal(
		t,
		"vgatherdps xmm7, dword ptr [eax+xmm7*1]",
		asm_to_bytes("vgatherdps xmm7, dword ptr [eax+xmm7*1]"),
		proc() {vgatherdps_xmm(XMMRegister.XMM7)},
	)
	test_equal(
		t,
		"vgatherdps xmm15, dword ptr [eax+xmm15*1]",
		asm_to_bytes("vgatherdps xmm15, dword ptr [eax+xmm15*1]"),
		proc() {vgatherdps_xmm(XMMRegister.XMM15)},
	)
}

@(test)
test_vscatterdps_xmm :: proc(t: ^testing.T) {
	test_equal(
		t,
		"vscatterdps dword ptr [eax+xmm0*1], xmm0",
		asm_to_bytes("vscatterdps dword ptr [eax+xmm0*1], xmm0"),
		proc() {vscatterdps_xmm(XMMRegister.XMM0)},
	)
	test_equal(
		t,
		"vscatterdps dword ptr [eax+xmm7*1], xmm7",
		asm_to_bytes("vscatterdps dword ptr [eax+xmm7*1], xmm7"),
		proc() {vscatterdps_xmm(XMMRegister.XMM7)},
	)
	test_equal(
		t,
		"vscatterdps dword ptr [eax+xmm15*1], xmm15",
		asm_to_bytes("vscatterdps dword ptr [eax+xmm15*1], xmm15"),
		proc() {vscatterdps_xmm(XMMRegister.XMM15)},
	)
}

@(test)
test_kmovq_k_k :: proc(t: ^testing.T) {
	test_equal(t, "kmovq k0, k1", asm_to_bytes("kmovq k0, k1"), proc() {kmovq_k_k(0, 1)})
	test_equal(t, "kmovq k3, k3", asm_to_bytes("kmovq k3, k3"), proc() {kmovq_k_k(3, 3)})
	test_equal(t, "kmovq k7, k0", asm_to_bytes("kmovq k7, k0"), proc() {kmovq_k_k(7, 0)})
}

@(test)
test_vpxordq_zmm_zmm_zmm :: proc(t: ^testing.T) {
	test_equal(
		t,
		"vpxorq zmm0, zmm1, zmm2",
		asm_to_bytes("vpxorq zmm0, zmm1, zmm2"),
		proc() {vpxordq_zmm_zmm_zmm(XMMRegister.XMM0, XMMRegister.XMM1, XMMRegister.XMM2)},
	)
	test_equal(
		t,
		"vpxorq zmm7, zmm7, zmm7",
		asm_to_bytes("vpxorq zmm7, zmm7, zmm7"),
		proc() {vpxordq_zmm_zmm_zmm(XMMRegister.XMM7, XMMRegister.XMM7, XMMRegister.XMM7)},
	)
	test_equal(
		t,
		"vpxorq zmm15, zmm14, zmm13",
		asm_to_bytes("vpxorq zmm15, zmm14, zmm13"),
		proc() {vpxordq_zmm_zmm_zmm(XMMRegister.XMM15, XMMRegister.XMM14, XMMRegister.XMM13)},
	)
}

@(test)
test_vpscatterdd_ymm_m :: proc(t: ^testing.T) {
	test_equal(
		t,
		"vpscatterdd dword ptr [0x1000+ymm0*1], ymm0",
		asm_to_bytes("vpscatterdd dword ptr [0x1000+ymm0*1], ymm0"),
		proc() {vpscatterdd_ymm_m(XMMRegister.XMM0, 0x1000)},
	)
	test_equal(
		t,
		"vpscatterdd dword ptr [0x0+ymm7*1], ymm7",
		asm_to_bytes("vpscatterdd dword ptr [0x0+ymm7*1], ymm7"),
		proc() {vpscatterdd_ymm_m(XMMRegister.XMM7, 0x0)},
	)
	test_equal(
		t,
		"vpscatterdd dword ptr [0x8000+ymm15*1], ymm15",
		asm_to_bytes("vpscatterdd dword ptr [0x8000+ymm15*1], ymm15"),
		proc() {vpscatterdd_ymm_m(XMMRegister.XMM15, 0x8000)},
	)
}

@(test)
test_vpscatterdq_ymm_m :: proc(t: ^testing.T) {
	test_equal(
		t,
		"vpscatterdq qword ptr [0x1000+ymm0*1], ymm0",
		asm_to_bytes("vpscatterdq qword ptr [0x1000+ymm0*1], ymm0"),
		proc() {vpscatterdq_ymm_m(XMMRegister.XMM0, 0x1000)},
	)
	test_equal(
		t,
		"vpscatterdq qword ptr [0x0+ymm7*1], ymm7",
		asm_to_bytes("vpscatterdq qword ptr [0x0+ymm7*1], ymm7"),
		proc() {vpscatterdq_ymm_m(XMMRegister.XMM7, 0x0)},
	)
	test_equal(
		t,
		"vpscatterdq qword ptr [0x8000+ymm15*1], ymm15",
		asm_to_bytes("vpscatterdq qword ptr [0x8000+ymm15*1], ymm15"),
		proc() {vpscatterdq_ymm_m(XMMRegister.XMM15, 0x8000)},
	)
}

@(test)
test_vpscatterqd_ymm_m :: proc(t: ^testing.T) {
	test_equal(
		t,
		"vpscatterqd dword ptr [0x1000+ymm0*1], xmm0",
		asm_to_bytes("vpscatterqd dword ptr [0x1000+ymm0*1], xmm0"),
		proc() {vpscatterqd_ymm_m(XMMRegister.XMM0, 0x1000)},
	)
	test_equal(
		t,
		"vpscatterqd dword ptr [0x0+ymm7*1], xmm7",
		asm_to_bytes("vpscatterqd dword ptr [0x0+ymm7*1], xmm7"),
		proc() {vpscatterqd_ymm_m(XMMRegister.XMM7, 0x0)},
	)
	test_equal(
		t,
		"vpscatterqd dword ptr [0x8000+ymm15*1], xmm15",
		asm_to_bytes("vpscatterqd dword ptr [0x8000+ymm15*1], xmm15"),
		proc() {vpscatterqd_ymm_m(XMMRegister.XMM15, 0x8000)},
	)
}

@(test)
test_vpcompressd_ymm_ymm :: proc(t: ^testing.T) {
	test_equal(
		t,
		"vpcompressd ymm0, ymm1",
		asm_to_bytes("vpcompressd ymm0, ymm1"),
		proc() {vpcompressd_ymm_ymm(XMMRegister.XMM0, XMMRegister.XMM1)},
	)
	test_equal(
		t,
		"vpcompressd ymm7, ymm7",
		asm_to_bytes("vpcompressd ymm7, ymm7"),
		proc() {vpcompressd_ymm_ymm(XMMRegister.XMM7, XMMRegister.XMM7)},
	)
	test_equal(
		t,
		"vpcompressd ymm15, ymm8",
		asm_to_bytes("vpcompressd ymm15, ymm8"),
		proc() {vpcompressd_ymm_ymm(XMMRegister.XMM15, XMMRegister.XMM8)},
	)
}

@(test)
test_vpcompressq_ymm_ymm :: proc(t: ^testing.T) {
	test_equal(
		t,
		"vpcompressq ymm0, ymm1",
		asm_to_bytes("vpcompressq ymm0, ymm1"),
		proc() {vpcompressq_ymm_ymm(XMMRegister.XMM0, XMMRegister.XMM1)},
	)
	test_equal(
		t,
		"vpcompressq ymm7, ymm7",
		asm_to_bytes("vpcompressq ymm7, ymm7"),
		proc() {vpcompressq_ymm_ymm(XMMRegister.XMM7, XMMRegister.XMM7)},
	)
	test_equal(
		t,
		"vpcompressq ymm15, ymm8",
		asm_to_bytes("vpcompressq ymm15, ymm8"),
		proc() {vpcompressq_ymm_ymm(XMMRegister.XMM15, XMMRegister.XMM8)},
	)
}


// ===== CRYPTOGRAPHY INSTRUCTION TESTS =====
@(test)
test_aesenc :: proc(t: ^testing.T) {
	test_equal(
		t,
		"aesenc xmm0, xmm1",
		asm_to_bytes("aesenc xmm0, xmm1"),
		proc() {aesenc(XMMRegister.XMM0, XMMRegister.XMM1)},
	)
	test_equal(
		t,
		"aesenc xmm7, xmm7",
		asm_to_bytes("aesenc xmm7, xmm7"),
		proc() {aesenc(XMMRegister.XMM7, XMMRegister.XMM7)},
	)
	test_equal(
		t,
		"aesenc xmm8, xmm15",
		asm_to_bytes("aesenc xmm8, xmm15"),
		proc() {aesenc(XMMRegister.XMM8, XMMRegister.XMM15)},
	)
}

@(test)
test_aesdec :: proc(t: ^testing.T) {
	test_equal(
		t,
		"aesdec xmm0, xmm1",
		asm_to_bytes("aesdec xmm0, xmm1"),
		proc() {aesdec(XMMRegister.XMM0, XMMRegister.XMM1)},
	)
	test_equal(
		t,
		"aesdec xmm7, xmm7",
		asm_to_bytes("aesdec xmm7, xmm7"),
		proc() {aesdec(XMMRegister.XMM7, XMMRegister.XMM7)},
	)
	test_equal(
		t,
		"aesdec xmm8, xmm15",
		asm_to_bytes("aesdec xmm8, xmm15"),
		proc() {aesdec(XMMRegister.XMM8, XMMRegister.XMM15)},
	)
}

@(test)
test_pclmulqdq :: proc(t: ^testing.T) {
	test_equal(
		t,
		"pclmulqdq xmm0, xmm1, 0x0",
		asm_to_bytes("pclmulqdq xmm0, xmm1, 0x0"),
		proc() {pclmulqdq(XMMRegister.XMM0, XMMRegister.XMM1, 0x0)},
	)
	test_equal(
		t,
		"pclmulqdq xmm7, xmm7, 0x1",
		asm_to_bytes("pclmulqdq xmm7, xmm7, 0x1"),
		proc() {pclmulqdq(XMMRegister.XMM7, XMMRegister.XMM7, 0x1)},
	)
	test_equal(
		t,
		"pclmulqdq xmm8, xmm15, 0x11",
		asm_to_bytes("pclmulqdq xmm8, xmm15, 0x11"),
		proc() {pclmulqdq(XMMRegister.XMM8, XMMRegister.XMM15, 0x11)},
	)
}

@(test)
test_crc32_r64_r64 :: proc(t: ^testing.T) {
	test_equal(
		t,
		"crc32 rax, rbx",
		asm_to_bytes("crc32 rax, rbx"),
		proc() {crc32_r64_r64(Register64.RAX, Register64.RBX)},
	)
	test_equal(
		t,
		"crc32 rdx, rdx",
		asm_to_bytes("crc32 rdx, rdx"),
		proc() {crc32_r64_r64(Register64.RDX, Register64.RDX)},
	)
	test_equal(
		t,
		"crc32 r8, r15",
		asm_to_bytes("crc32 r8, r15"),
		proc() {crc32_r64_r64(Register64.R8, Register64.R15)},
	)
}

// ===== X87 FLOATING POINT INSTRUCTION TESTS =====
@(test)
test_fadd_st0_st :: proc(t: ^testing.T) {
	test_equal(
		t,
		"fadd st(0), st(0)",
		asm_to_bytes("fadd st(0), st(0)"),
		proc() {fadd_st0_st(x87Register.ST0)},
	)
	test_equal(
		t,
		"fadd st(0), st(1)",
		asm_to_bytes("fadd st(0), st(1)"),
		proc() {fadd_st0_st(x87Register.ST1)},
	)
	test_equal(
		t,
		"fadd st(0), st(7)",
		asm_to_bytes("fadd st(0), st(7)"),
		proc() {fadd_st0_st(x87Register.ST7)},
	)
}

@(test)
test_fsub_st0_st :: proc(t: ^testing.T) {
	test_equal(
		t,
		"fsub st(0), st(0)",
		asm_to_bytes("fsub st(0), st(0)"),
		proc() {fsub_st0_st(x87Register.ST0)},
	)
	test_equal(
		t,
		"fsub st(0), st(1)",
		asm_to_bytes("fsub st(0), st(1)"),
		proc() {fsub_st0_st(x87Register.ST1)},
	)
	test_equal(
		t,
		"fsub st(0), st(7)",
		asm_to_bytes("fsub st(0), st(7)"),
		proc() {fsub_st0_st(x87Register.ST7)},
	)
}

@(test)
test_fmul_st0_st :: proc(t: ^testing.T) {
	test_equal(
		t,
		"fmul st(0), st(0)",
		asm_to_bytes("fmul st(0), st(0)"),
		proc() {fmul_st0_st(x87Register.ST0)},
	)
	test_equal(
		t,
		"fmul st(0), st(1)",
		asm_to_bytes("fmul st(0), st(1)"),
		proc() {fmul_st0_st(x87Register.ST1)},
	)
	test_equal(
		t,
		"fmul st(0), st(7)",
		asm_to_bytes("fmul st(0), st(7)"),
		proc() {fmul_st0_st(x87Register.ST7)},
	)
}

@(test)
test_fdiv_st0_st :: proc(t: ^testing.T) {
	test_equal(
		t,
		"fdiv st(0), st(0)",
		asm_to_bytes("fdiv st(0), st(0)"),
		proc() {fdiv_st0_st(x87Register.ST0)},
	)
	test_equal(
		t,
		"fdiv st(0), st(1)",
		asm_to_bytes("fdiv st(0), st(1)"),
		proc() {fdiv_st0_st(x87Register.ST1)},
	)
	test_equal(
		t,
		"fdiv st(0), st(7)",
		asm_to_bytes("fdiv st(0), st(7)"),
		proc() {fdiv_st0_st(x87Register.ST7)},
	)
}

@(test)
test_fcom_st :: proc(t: ^testing.T) {
	test_equal(t, "fcom st(0)", asm_to_bytes("fcom st(0)"), proc() {fcom_st(x87Register.ST0)})
	test_equal(t, "fcom st(1)", asm_to_bytes("fcom st(1)"), proc() {fcom_st(x87Register.ST1)})
	test_equal(t, "fcom st(7)", asm_to_bytes("fcom st(7)"), proc() {fcom_st(x87Register.ST7)})
}

@(test)
test_fcomp_st :: proc(t: ^testing.T) {
	test_equal(t, "fcomp st(0)", asm_to_bytes("fcomp st(0)"), proc() {fcomp_st(x87Register.ST0)})
	test_equal(t, "fcomp st(1)", asm_to_bytes("fcomp st(1)"), proc() {fcomp_st(x87Register.ST1)})
	test_equal(t, "fcomp st(7)", asm_to_bytes("fcomp st(7)"), proc() {fcomp_st(x87Register.ST7)})
}

@(test)
test_fucom_st :: proc(t: ^testing.T) {
	test_equal(t, "fucom st(0)", asm_to_bytes("fucom st(0)"), proc() {fucom_st(x87Register.ST0)})
	test_equal(t, "fucom st(1)", asm_to_bytes("fucom st(1)"), proc() {fucom_st(x87Register.ST1)})
	test_equal(t, "fucom st(7)", asm_to_bytes("fucom st(7)"), proc() {fucom_st(x87Register.ST7)})
}

@(test)
test_fucomp_st :: proc(t: ^testing.T) {
	test_equal(
		t,
		"fucomp st(0)",
		asm_to_bytes("fucomp st(0)"),
		proc() {fucomp_st(x87Register.ST0)},
	)
	test_equal(
		t,
		"fucomp st(1)",
		asm_to_bytes("fucomp st(1)"),
		proc() {fucomp_st(x87Register.ST1)},
	)
	test_equal(
		t,
		"fucomp st(7)",
		asm_to_bytes("fucomp st(7)"),
		proc() {fucomp_st(x87Register.ST7)},
	)
}

@(test)
test_fldcw :: proc(t: ^testing.T) {
	test_equal(t, "fldcw [0x1000]", asm_to_bytes("fldcw [0x1000]"), proc() {fldcw(0x1000)})
	test_equal(t, "fldcw [0x0]", asm_to_bytes("fldcw [0x0]"), proc() {fldcw(0x0)})
}

@(test)
test_fstcw :: proc(t: ^testing.T) {
	test_equal(t, "fstcw [0x1000]", asm_to_bytes("fstcw [0x1000]"), proc() {fstcw(0x1000)})
	test_equal(t, "fstcw [0x0]", asm_to_bytes("fstcw [0x0]"), proc() {fstcw(0x0)})
}

@(test)
test_fnstcw :: proc(t: ^testing.T) {
	test_equal(t, "fnstcw [0x1000]", asm_to_bytes("fnstcw [0x1000]"), proc() {fnstcw(0x1000)})
	test_equal(t, "fnstcw [0x0]", asm_to_bytes("fnstcw [0x0]"), proc() {fnstcw(0x0)})
}

@(test)
test_fninit :: proc(t: ^testing.T) {
	test_equal(t, "fninit", asm_to_bytes("fninit"), proc() {fninit()})
}

@(test)
test_fwait :: proc(t: ^testing.T) {
	test_equal(t, "fwait", asm_to_bytes("fwait"), proc() {fwait()})
}

// ===== MEMORY AND STRING OPERATIONS TESTS =====
@(test)
test_movs_m64_m64 :: proc(t: ^testing.T) {
	test_equal(t, "movsq", asm_to_bytes("movsq"), proc() {movs_m64_m64()})
}

@(test)
test_stos_m64 :: proc(t: ^testing.T) {
	test_equal(t, "stosq", asm_to_bytes("stosq"), proc() {stos_m64()})
}

@(test)
test_cmps_m64_m64 :: proc(t: ^testing.T) {
	test_equal(t, "cmpsq", asm_to_bytes("cmpsq"), proc() {cmps_m64_m64()})
}

@(test)
test_rep_movs :: proc(t: ^testing.T) {
	test_equal(t, "rep movsq", asm_to_bytes("rep movsq"), proc() {rep_movs()})
}

@(test)
test_rep_stos :: proc(t: ^testing.T) {
	test_equal(t, "rep stosq", asm_to_bytes("rep stosq"), proc() {rep_stos()})
}

@(test)
test_rep_cmps :: proc(t: ^testing.T) {
	test_equal(t, "rep cmpsq", asm_to_bytes("rep cmpsq"), proc() {rep_cmps()})
}

// ===== SYSTEM INSTRUCTIONS TESTS =====
@(test)
test_syscall :: proc(t: ^testing.T) {
	test_equal(t, "syscall", asm_to_bytes("syscall"), proc() {syscall()})
}

@(test)
test_int_imm8 :: proc(t: ^testing.T) {
	test_equal(t, "int 0x80", asm_to_bytes("int 0x80"), proc() {int_imm8(0x80)})
	test_equal(t, "int 0x3", asm_to_bytes("int 0x3"), proc() {int_imm8(0x3)})
	test_equal(t, "int 0x0", asm_to_bytes("int 0x0"), proc() {int_imm8(0x0)})
}

@(test)
test_int3 :: proc(t: ^testing.T) {
	test_equal(t, "int3", asm_to_bytes("int3"), proc() {int3()})
}

@(test)
test_iret :: proc(t: ^testing.T) {
	test_equal(t, "iretq", asm_to_bytes("iretq"), proc() {iret()})
}

@(test)
test_cpuid :: proc(t: ^testing.T) {
	test_equal(t, "cpuid", asm_to_bytes("cpuid"), proc() {cpuid()})
}

@(test)
test_rdtsc :: proc(t: ^testing.T) {
	test_equal(t, "rdtsc", asm_to_bytes("rdtsc"), proc() {rdtsc()})
}

@(test)
test_rdtscp :: proc(t: ^testing.T) {
	test_equal(t, "rdtscp", asm_to_bytes("rdtscp"), proc() {rdtscp()})
}

@(test)
test_rdmsr :: proc(t: ^testing.T) {
	test_equal(t, "rdmsr", asm_to_bytes("rdmsr"), proc() {rdmsr()})
}

@(test)
test_wrmsr :: proc(t: ^testing.T) {
	test_equal(t, "wrmsr", asm_to_bytes("wrmsr"), proc() {wrmsr()})
}

@(test)
test_rdpmc :: proc(t: ^testing.T) {
	test_equal(t, "rdpmc", asm_to_bytes("rdpmc"), proc() {rdpmc()})
}

@(test)
test_hlt :: proc(t: ^testing.T) {
	test_equal(t, "hlt", asm_to_bytes("hlt"), proc() {hlt()})
}

@(test)
test_swapgs :: proc(t: ^testing.T) {
	test_equal(t, "swapgs", asm_to_bytes("swapgs"), proc() {swapgs()})
}

@(test)
test_wrpkru :: proc(t: ^testing.T) {
	test_equal(t, "wrpkru", asm_to_bytes("wrpkru"), proc() {wrpkru()})
}

@(test)
test_rdpkru :: proc(t: ^testing.T) {
	test_equal(t, "rdpkru", asm_to_bytes("rdpkru"), proc() {rdpkru()})
}

@(test)
test_clac :: proc(t: ^testing.T) {
	test_equal(t, "clac", asm_to_bytes("clac"), proc() {clac()})
}

@(test)
test_stac :: proc(t: ^testing.T) {
	test_equal(t, "stac", asm_to_bytes("stac"), proc() {stac()})
}

@(test)
test_ud2 :: proc(t: ^testing.T) {
	test_equal(t, "ud2", asm_to_bytes("ud2"), proc() {ud2()})
}

@(test)
test_vmcall :: proc(t: ^testing.T) {
	test_equal(t, "vmcall", asm_to_bytes("vmcall"), proc() {vmcall()})
}

@(test)
test_vmlaunch :: proc(t: ^testing.T) {
	test_equal(t, "vmlaunch", asm_to_bytes("vmlaunch"), proc() {vmlaunch()})
}

@(test)
test_vmresume :: proc(t: ^testing.T) {
	test_equal(t, "vmresume", asm_to_bytes("vmresume"), proc() {vmresume()})
}

@(test)
test_vmxoff :: proc(t: ^testing.T) {
	test_equal(t, "vmxoff", asm_to_bytes("vmxoff"), proc() {vmxoff()})
}

// ===== ATOMIC OPERATIONS TESTS =====
@(test)
test_lock_xadd_r64_r64 :: proc(t: ^testing.T) {
	test_equal(
		t,
		"lock xadd rax, rbx",
		asm_to_bytes("lock xadd QWORD PTR [rax], rbx"),
		proc() {lock_xadd_r64_r64(Register64.RAX, Register64.RBX)},
	)
	test_equal(
		t,
		"lock xadd rdx, rdx",
		asm_to_bytes("lock xadd QWORD PTR [rdx], rdx"),
		proc() {lock_xadd_r64_r64(Register64.RDX, Register64.RDX)},
	)
	test_equal(
		t,
		"lock xadd r8, r15",
		asm_to_bytes("lock xadd QWORD PTR [r8], r15"),
		proc() {lock_xadd_r64_r64(Register64.R8, Register64.R15)},
	)
}

@(test)
test_lock_cmpxchg_r64_r64 :: proc(t: ^testing.T) {
	test_equal(
		t,
		"lock cmpxchg rax, rbx",
		asm_to_bytes("lock cmpxchg QWORD PTR [rax], rbx"),
		proc() {lock_cmpxchg_r64_r64(Register64.RAX, Register64.RBX)},
	)
	test_equal(
		t,
		"lock cmpxchg rdx, rdx",
		asm_to_bytes("lock cmpxchg QWORD PTR [rdx], rdx"),
		proc() {lock_cmpxchg_r64_r64(Register64.RDX, Register64.RDX)},
	)
	test_equal(
		t,
		"lock cmpxchg r8, r15",
		asm_to_bytes("lock cmpxchg QWORD PTR [r8], r15"),
		proc() {lock_cmpxchg_r64_r64(Register64.R8, Register64.R15)},
	)
}

@(test)
test_lock_inc_r64 :: proc(t: ^testing.T) {
	test_equal(
		t,
		"lock inc rax",
		asm_to_bytes("lock inc QWORD PTR [rax]"),
		proc() {lock_inc_r64(Register64.RAX)},
	)
	test_equal(
		t,
		"lock inc rdx",
		asm_to_bytes("lock inc QWORD PTR [rdx]"),
		proc() {lock_inc_r64(Register64.RDX)},
	)
	test_equal(
		t,
		"lock inc r15",
		asm_to_bytes("lock inc QWORD PTR [r15]"),
		proc() {lock_inc_r64(Register64.R15)},
	)
}

@(test)
test_lock_dec_r64 :: proc(t: ^testing.T) {
	test_equal(
		t,
		"lock dec rax",
		asm_to_bytes("lock dec QWORD PTR [rax]"),
		proc() {lock_dec_r64(Register64.RAX)},
	)
	test_equal(
		t,
		"lock dec rdx",
		asm_to_bytes("lock dec QWORD PTR [rdx]"),
		proc() {lock_dec_r64(Register64.RDX)},
	)
	test_equal(
		t,
		"lock dec r15",
		asm_to_bytes("lock dec QWORD PTR [r15]"),
		proc() {lock_dec_r64(Register64.R15)},
	)
}

@(test)
test_lock_xchg_r64_r64 :: proc(t: ^testing.T) {
	test_equal(
		t,
		"lock xchg rax, rbx",
		asm_to_bytes("lock xchg QWORD PTR [rax], rbx"),
		proc() {lock_xchg_r64_r64(Register64.RAX, Register64.RBX)},
	)
	test_equal(
		t,
		"lock xchg rdx, rdx",
		asm_to_bytes("lock xchg QWORD PTR [rdx], rdx"),
		proc() {lock_xchg_r64_r64(Register64.RDX, Register64.RDX)},
	)
	test_equal(
		t,
		"lock xchg r8, r15",
		asm_to_bytes("lock xchg QWORD PTR [r8], r15"),
		proc() {lock_xchg_r64_r64(Register64.R8, Register64.R15)},
	)
}

@(test)
test_atomic_load_r64 :: proc(t: ^testing.T) {
	test_equal(
		t,
		"mov rax, [0x1000]",
		asm_to_bytes("mov rax, [0x1000]"),
		proc() {atomic_load_r64(Register64.RAX, 0x1000)},
	)
	test_equal(
		t,
		"mov r15, [0x0]",
		asm_to_bytes("mov r15, [0x0]"),
		proc() {atomic_load_r64(Register64.R15, 0x0)},
	)
}

@(test)
test_atomic_store_r64 :: proc(t: ^testing.T) {
	test_equal(
		t,
		"mov [0x1000], rax",
		asm_to_bytes("mov [0x1000], rax"),
		proc() {atomic_store_r64(0x1000, Register64.RAX)},
	)
	test_equal(
		t,
		"mov [0x0], r15",
		asm_to_bytes("mov [0x0], r15"),
		proc() {atomic_store_r64(0x0, Register64.R15)},
	)
}

// ===== TRANSACTIONAL MEMORY INSTRUCTIONS TESTS =====
@(test)
test_xbegin :: proc(t: ^testing.T) {
	test_equal(t, "xbegin +0x10", asm_to_bytes("xbegin +0x10"), proc() {xbegin(0x10)})
	test_equal(t, "xbegin -0x10", asm_to_bytes("xbegin -0x10"), proc() {xbegin(-0x10)})
}

@(test)
test_xend :: proc(t: ^testing.T) {
	test_equal(t, "xend", asm_to_bytes("xend"), proc() {xend()})
}

@(test)
test_xabort :: proc(t: ^testing.T) {
	test_equal(t, "xabort 0x0", asm_to_bytes("xabort 0x0"), proc() {xabort(0x0)})
	test_equal(t, "xabort 0xFF", asm_to_bytes("xabort 0xFF"), proc() {xabort(0xFF)})
}

@(test)
test_xtest :: proc(t: ^testing.T) {
	test_equal(t, "xtest", asm_to_bytes("xtest"), proc() {xtest()})
}

// ===== RANDOM NUMBER GENERATION TESTS =====
@(test)
test_rdrand_r64 :: proc(t: ^testing.T) {
	test_equal(t, "rdrand rax", asm_to_bytes("rdrand rax"), proc() {rdrand_r64(Register64.RAX)})
	test_equal(t, "rdrand rdx", asm_to_bytes("rdrand rdx"), proc() {rdrand_r64(Register64.RDX)})
	test_equal(t, "rdrand r15", asm_to_bytes("rdrand r15"), proc() {rdrand_r64(Register64.R15)})
}

@(test)
test_rdseed_r64 :: proc(t: ^testing.T) {
	test_equal(t, "rdseed rax", asm_to_bytes("rdseed rax"), proc() {rdseed_r64(Register64.RAX)})
	test_equal(t, "rdseed rdx", asm_to_bytes("rdseed rdx"), proc() {rdseed_r64(Register64.RDX)})
	test_equal(t, "rdseed r15", asm_to_bytes("rdseed r15"), proc() {rdseed_r64(Register64.R15)})
}

// ===== PREFETCH INSTRUCTIONS TESTS =====
@(test)
test_prefetcht0 :: proc(t: ^testing.T) {
	test_equal(
		t,
		"prefetcht0 [0x1000]",
		asm_to_bytes("prefetcht0 [0x1000]"),
		proc() {prefetcht0(0x1000)},
	)
	test_equal(t, "prefetcht0 [0x0]", asm_to_bytes("prefetcht0 [0x0]"), proc() {prefetcht0(0x0)})
}

@(test)
test_prefetcht1 :: proc(t: ^testing.T) {
	test_equal(
		t,
		"prefetcht1 [0x1000]",
		asm_to_bytes("prefetcht1 [0x1000]"),
		proc() {prefetcht1(0x1000)},
	)
	test_equal(t, "prefetcht1 [0x0]", asm_to_bytes("prefetcht1 [0x0]"), proc() {prefetcht1(0x0)})
}

@(test)
test_prefetcht2 :: proc(t: ^testing.T) {
	test_equal(
		t,
		"prefetcht2 [0x1000]",
		asm_to_bytes("prefetcht2 [0x1000]"),
		proc() {prefetcht2(0x1000)},
	)
	test_equal(t, "prefetcht2 [0x0]", asm_to_bytes("prefetcht2 [0x0]"), proc() {prefetcht2(0x0)})
}

@(test)
test_prefetchnta :: proc(t: ^testing.T) {
	test_equal(
		t,
		"prefetchnta [0x1000]",
		asm_to_bytes("prefetchnta [0x1000]"),
		proc() {prefetchnta(0x1000)},
	)
	test_equal(
		t,
		"prefetchnta [0x0]",
		asm_to_bytes("prefetchnta [0x0]"),
		proc() {prefetchnta(0x0)},
	)
}

// ===== MEMORY MANAGEMENT AND OPTIMIZATION TESTS =====
@(test)
test_clflush_m64 :: proc(t: ^testing.T) {
	test_equal(
		t,
		"clflush [0x1000]",
		asm_to_bytes("clflush [0x1000]"),
		proc() {clflush_m64(0x1000)},
	)
	test_equal(t, "clflush [0x0]", asm_to_bytes("clflush [0x0]"), proc() {clflush_m64(0x0)})
}

@(test)
test_clflushopt_m64 :: proc(t: ^testing.T) {
	test_equal(
		t,
		"clflushopt [0x1000]",
		asm_to_bytes("clflushopt [0x1000]"),
		proc() {clflushopt_m64(0x1000)},
	)
	test_equal(
		t,
		"clflushopt [0x0]",
		asm_to_bytes("clflushopt [0x0]"),
		proc() {clflushopt_m64(0x0)},
	)
}

@(test)
test_clwb_m64 :: proc(t: ^testing.T) {
	test_equal(t, "clwb [0x1000]", asm_to_bytes("clwb [0x1000]"), proc() {clwb_m64(0x1000)})
	test_equal(t, "clwb [0x0]", asm_to_bytes("clwb [0x0]"), proc() {clwb_m64(0x0)})
}


// ===== POWER MANAGEMENT TESTS =====
@(test)
test_monitor_r64_r64_r64 :: proc(t: ^testing.T) {
	test_equal(
		t,
		"monitor",
		asm_to_bytes("monitor"),
		proc() {monitor_r64_r64_r64(Register64.RAX, Register64.RCX, Register64.RDX)},
	)
}

@(test)
test_mwait_r64_r64 :: proc(t: ^testing.T) {
	test_equal(
		t,
		"mwait",
		asm_to_bytes("mwait"),
		proc() {mwait_r64_r64(Register64.RAX, Register64.RCX)},
	)
}

// ===== MEMORY FENCES TESTS =====
@(test)
test_mfence :: proc(t: ^testing.T) {
	test_equal(t, "mfence", asm_to_bytes("mfence"), proc() {mfence()})
}

@(test)
test_lfence :: proc(t: ^testing.T) {
	test_equal(t, "lfence", asm_to_bytes("lfence"), proc() {lfence()})
}

@(test)
test_sfence :: proc(t: ^testing.T) {
	test_equal(t, "sfence", asm_to_bytes("sfence"), proc() {sfence()})
}

// ===== PERFORMANCE MONITORING TESTS =====
@(test)
test_perfmon_instructions :: proc(t: ^testing.T) {
	// This is a placeholder test for a placeholder function
	// No actual testing is done here, just verifying the function exists
	perfmon_instructions()
}
