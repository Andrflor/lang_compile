package x64_assembler

import "core:fmt"

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

// ===== DATA MOVEMENT INSTRUCTIONS =====
// --- General Purpose Register Movement ---
mov_r64_imm64 :: proc(reg: Register64, imm: u64) -> [10]u8 {
	// REX.W + B8+rd io (48 B8+ rd io)
	return [10]u8 {
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
	}
}

mov_r64_r64 :: proc(dst: Register64, src: Register64) -> [3]u8 {
	// REX.W + 89 /r (48 89 /r)
	return [3]u8 {
		0x48 + ((u8(src) & 0x8) >> 1) + ((u8(dst) & 0x8) >> 3), // REX.W + register extensions
		0x89, // Opcode
		0xC0 + ((u8(src) & 0x7) << 3) + (u8(dst) & 0x7), // ModR/M byte
	}
}

mov_r64_m64 :: proc(dst: Register64, mem_addr: u64) -> [8]u8 {
	// REX.W + 8B /r (48 8B /r)
	return [8]u8 {
		0x48 + ((u8(dst) & 0x8) >> 1), // REX.W + register extension
		0x8B, // Opcode
		0x04 + ((u8(dst) & 0x7) << 3), // ModR/M byte with SIB
		0x25, // SIB byte for absolute addressing

		// Store address (little-endian, 32-bit)
		u8(mem_addr & 0xFF),
		u8((mem_addr >> 8) & 0xFF),
		u8((mem_addr >> 16) & 0xFF),
		u8((mem_addr >> 24) & 0xFF),
	}
}

mov_m64_r64 :: proc(mem_addr: u64, src: Register64) -> [8]u8 {
	// REX.W + 89 /r (48 89 /r)
	return [8]u8 {
		0x48 + ((u8(src) & 0x8) >> 1), // REX.W + register extension
		0x89, // Opcode
		0x04 + ((u8(src) & 0x7) << 3), // ModR/M byte with SIB
		0x25, // SIB byte for absolute addressing

		// Store address (little-endian, 32-bit)
		u8(mem_addr & 0xFF),
		u8((mem_addr >> 8) & 0xFF),
		u8((mem_addr >> 16) & 0xFF),
		u8((mem_addr >> 24) & 0xFF),
	}
}

movabs_r64_imm64 :: proc(reg: Register64, imm: u64) -> [10]u8 {
	// This is the same as mov_r64_imm64 for 64-bit mode
	return mov_r64_imm64(reg, imm)
}

xchg_r64_r64 :: proc(dst: Register64, src: Register64) -> [3]u8 {
	// REX.W + 87 /r (48 87 /r)
	return [3]u8 {
		0x48 + ((u8(src) & 0x8) >> 1) + ((u8(dst) & 0x8) >> 3), // REX.W + register extensions
		0x87, // Opcode
		0xC0 + ((u8(src) & 0x7) << 3) + (u8(dst) & 0x7), // ModR/M byte
	}
}

// --- Sign and Zero Extensions ---
movsx_r64_r32 :: proc(dst: Register64, src: Register64) -> [3]u8 {
	// REX.W + 63 /r (48 63 /r)
	return [3]u8 {
		0x48 + ((u8(dst) & 0x8) >> 1) + ((u8(src) & 0x8) >> 3), // REX.W + register extensions
		0x63, // Opcode
		0xC0 + ((u8(dst) & 0x7) << 3) + (u8(src) & 0x7), // ModR/M byte
	}
}

movzx_r64_r32 :: proc(dst: Register64, src: Register64) -> [3]u8 {
	// No need for movzx for 32-bit to 64-bit in x64, upper bits are zeroed by default
	// But for completeness, we'll encode a 32-bit mov (no REX.W prefix)
	return [3]u8 {
		0x40 + ((u8(dst) & 0x8) >> 1) + ((u8(src) & 0x8) >> 3), // REX prefix (no W bit)
		0x89, // Opcode
		0xC0 + ((u8(src) & 0x7) << 3) + (u8(dst) & 0x7), // ModR/M byte
	}
}

// --- Byte Order Control ---
movbe_r64_m64 :: proc(dst: Register64, mem_addr: u64) -> [10]u8 {
	// REX.W + 0F 38 F0 /r (48 0F 38 F0 /r)
	return [10]u8 {
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
	}
}

movbe_m64_r64 :: proc(mem_addr: u64, src: Register64) -> [10]u8 {
	// REX.W + 0F 38 F1 /r (48 0F 38 F1 /r)
	return [10]u8 {
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
	}
}

bswap_r64 :: proc(reg: Register64) -> [3]u8 { 	// Byte swap
	// REX.W + 0F C8+rd (48 0F C8+rd)
	return [3]u8 {
		0x48 + ((u8(reg) & 0x8) >> 3), // REX.W + register extension
		0x0F, // Two-byte opcode prefix
		0xC8 + (u8(reg) & 0x7), // Opcode + register
	}
}

// --- Special Registers ---
mov_cr_r64 :: proc(cr: u8, reg: Register64) -> [3]u8 {
	// 0F 22 /r (0F 22 /r)
	return [3]u8 {
		0x0F, // Two-byte opcode prefix
		0x22, // Opcode
		0xC0 + ((cr & 0x7) << 3) + (u8(reg) & 0x7), // ModR/M byte
	}
}

mov_r64_cr :: proc(reg: Register64, cr: u8) -> [3]u8 {
	// 0F 20 /r (0F 20 /r)
	return [3]u8 {
		0x0F, // Two-byte opcode prefix
		0x20, // Opcode
		0xC0 + ((cr & 0x7) << 3) + (u8(reg) & 0x7), // ModR/M byte
	}
}

mov_cr0_r64 :: proc(reg: Register64) -> [3]u8 {
	return mov_cr_r64(0, reg)
}

mov_cr2_r64 :: proc(reg: Register64) -> [3]u8 {
	return mov_cr_r64(2, reg)
}

mov_cr3_r64 :: proc(reg: Register64) -> [3]u8 {
	return mov_cr_r64(3, reg)
}

mov_cr4_r64 :: proc(reg: Register64) -> [3]u8 {
	return mov_cr_r64(4, reg)
}

// --- Debug Registers ---
mov_dr0_r64 :: proc(reg: Register64) -> [3]u8 {
	// 0F 23 /r (0F 23 /r)
	return [3]u8 {
		0x0F, // Two-byte opcode prefix
		0x23, // Opcode
		0xC0 + ((0 & 0x7) << 3) + (u8(reg) & 0x7), // ModR/M byte
	}
}

mov_dr1_r64 :: proc(reg: Register64) -> [3]u8 {
	return [3]u8 {
		0x0F,
		0x23,
		0xC8 + (u8(reg) & 0x7), // Base 0xC8 = 0xC0 + (1 << 3)
	}
}

mov_dr2_r64 :: proc(reg: Register64) -> [3]u8 {
	return [3]u8 {
		0x0F,
		0x23,
		0xD0 + (u8(reg) & 0x7), // Base 0xD0 = 0xC0 + (2 << 3)
	}
}

mov_dr3_r64 :: proc(reg: Register64) -> [3]u8 {
	return [3]u8 {
		0x0F,
		0x23,
		0xD8 + (u8(reg) & 0x7), // Base 0xD8 = 0xC0 + (3 << 3)
	}
}

mov_dr6_r64 :: proc(reg: Register64) -> [3]u8 {
	return [3]u8 {
		0x0F,
		0x23,
		0xF0 + (u8(reg) & 0x7), // Base 0xF0 = 0xC0 + (6 << 3)
	}
}

mov_dr7_r64 :: proc(reg: Register64) -> [3]u8 {
	return [3]u8 {
		0x0F,
		0x23,
		0xF8 + (u8(reg) & 0x7), // Base 0xF8 = 0xC0 + (7 << 3)
	}
}

// --- Address Manipulation ---
lea_r64_m64 :: proc(dst: Register64, mem_addr: u64) -> [8]u8 {
	// REX.W + 8D /r (48 8D /r)
	return [8]u8 {
		0x48 + ((u8(dst) & 0x8) >> 1), // REX.W + register extension
		0x8D, // Opcode
		0x04 + ((u8(dst) & 0x7) << 3), // ModR/M byte with SIB
		0x25, // SIB byte for absolute addressing

		// Store address (little-endian, 32-bit)
		u8(mem_addr & 0xFF),
		u8((mem_addr >> 8) & 0xFF),
		u8((mem_addr >> 16) & 0xFF),
		u8((mem_addr >> 24) & 0xFF),
	}
}

lea_r64_m :: proc(dst: Register64, mem: u64) -> [8]u8 {
	// This is a duplicate of lea_r64_m64 with a different name
	return lea_r64_m64(dst, mem)
}

// ===== ARITHMETIC INSTRUCTIONS =====
// --- Basic Arithmetic ---
add_r64_imm32 :: proc(reg: Register64, imm: u32) -> [7]u8 {
	// REX.W + 81 /0 id (48 81 /0 id)
	return [7]u8 {
		0x48 + ((u8(reg) & 0x8) >> 3), // REX.W + register extension
		0x81, // Opcode
		0xC0 + (u8(reg) & 0x7), // ModR/M byte (/0 = opcode extension 0 for ADD)
		// Immediate (little-endian)
		u8(imm & 0xFF),
		u8((imm >> 8) & 0xFF),
		u8((imm >> 16) & 0xFF),
		u8((imm >> 24) & 0xFF),
	}
}

add_r64_r64 :: proc(dst: Register64, src: Register64) -> [3]u8 {
	// REX.W + 01 /r (48 01 /r)
	return [3]u8 {
		0x48 + ((u8(src) & 0x8) >> 1) + ((u8(dst) & 0x8) >> 3), // REX.W + register extensions
		0x01, // Opcode
		0xC0 + ((u8(src) & 0x7) << 3) + (u8(dst) & 0x7), // ModR/M byte
	}
}

sub_r64_imm32 :: proc(reg: Register64, imm: u32) -> [7]u8 {
	// REX.W + 81 /5 id (48 81 /5 id)
	return [7]u8 {
		0x48 + ((u8(reg) & 0x8) >> 3), // REX.W + register extension
		0x81, // Opcode
		0xE8 + (u8(reg) & 0x7), // ModR/M byte (/5 = opcode extension 5 for SUB)
		// Immediate (little-endian)
		u8(imm & 0xFF),
		u8((imm >> 8) & 0xFF),
		u8((imm >> 16) & 0xFF),
		u8((imm >> 24) & 0xFF),
	}
}

sub_r64_r64 :: proc(dst: Register64, src: Register64) -> [3]u8 {
	// REX.W + 29 /r (48 29 /r)
	return [3]u8 {
		0x48 + ((u8(src) & 0x8) >> 1) + ((u8(dst) & 0x8) >> 3), // REX.W + register extensions
		0x29, // Opcode
		0xC0 + ((u8(src) & 0x7) << 3) + (u8(dst) & 0x7), // ModR/M byte
	}
}

inc_r64 :: proc(reg: Register64) -> [3]u8 {
	// REX.W + FF /0 (48 FF /0)
	return [3]u8 {
		0x48 + ((u8(reg) & 0x8) >> 3), // REX.W + register extension
		0xFF, // Opcode
		0xC0 + (u8(reg) & 0x7), // ModR/M byte (/0 = opcode extension 0 for INC)
	}
}

dec_r64 :: proc(reg: Register64) -> [3]u8 {
	// REX.W + FF /1 (48 FF /1)
	return [3]u8{0x48 + ((u8(reg) & 0x8) >> 3), 0xFF, 0xC8 + (u8(reg) & 0x7)}
}

neg_r64 :: proc(reg: Register64) -> [3]u8 {
	// REX.W + F7 /3 (48 F7 /3)
	return [3]u8{0x48 + ((u8(reg) & 0x8) >> 3), 0xF7, 0xD8 + (u8(reg) & 0x7)}
}

// --- Advanced Arithmetic ---
adc_r64_r64 :: proc(dst: Register64, src: Register64) -> [3]u8 { 	// Add with carry
	// REX.W + 11 /r (48 11 /r)
	return [3]u8 {
		0x48 + ((u8(src) & 0x8) >> 1) + ((u8(dst) & 0x8) >> 3),
		0x11, // Opcode
		0xC0 + ((u8(src) & 0x7) << 3) + (u8(dst) & 0x7), // ModR/M byte
	}
}

sbb_r64_r64 :: proc(dst: Register64, src: Register64) -> [3]u8 { 	// Subtract with borrow
	// REX.W + 19 /r (48 19 /r)
	return [3]u8 {
		0x48 + ((u8(src) & 0x8) >> 1) + ((u8(dst) & 0x8) >> 3),
		0x19, // Opcode
		0xC0 + ((u8(src) & 0x7) << 3) + (u8(dst) & 0x7),
	}
}

xadd_r64_r64 :: proc(dst: Register64, src: Register64) -> [4]u8 { 	// Exchange and add
	// REX.W + 0F C1 /r (48 0F C1 /r)
	return [4]u8 {
		0x48 + ((u8(src) & 0x8) >> 1) + ((u8(dst) & 0x8) >> 3),
		0x0F, // Two-byte opcode prefix
		0xC1, // Opcode
		0xC0 + ((u8(src) & 0x7) << 3) + (u8(dst) & 0x7), // ModR/M byte
	}
}

// --- Multiplication and Division ---
mul_r64 :: proc(reg: Register64) -> [3]u8 { 	// Unsigned multiply
	// REX.W + F7 /4 (48 F7 /4)
	return [3]u8{0x48 + ((u8(reg) & 0x8) >> 3), 0xF7, 0xE0 + (u8(reg) & 0x7)}
}

imul_r64_r64 :: proc(dst: Register64, src: Register64) -> [4]u8 { 	// Signed multiply
	// REX.W + 0F AF /r (48 0F AF /r)
	// Corrected: destination is in the reg field and source in r/m field.
	return [4]u8 {
		0x48 + ((u8(src) & 0x8) >> 1) + ((u8(dst) & 0x8) >> 3),
		0x0F, // Two-byte opcode prefix
		0xAF, // Opcode
		0xC0 + ((u8(dst) & 0x7) << 3) + (u8(src) & 0x7), // ModR/M byte
	}
}

imul_r64_r64_imm32 :: proc(dst: Register64, src: Register64, imm: u32) -> [7]u8 {
	// REX.W + 69 /r id (48 69 /r id)
	// Corrected REX prefix order (same as mov_r64_r64) and immediate encoding.
	return [7]u8 {
		0x48 + ((u8(src) & 0x8) >> 1) + ((u8(dst) & 0x8) >> 3),
		0x69, // Opcode
		0xC0 + ((u8(dst) & 0x7) << 3) + (u8(src) & 0x7),
		u8(imm & 0xFF),
		u8((imm >> 8) & 0xFF),
		u8((imm >> 16) & 0xFF),
		u8((imm >> 24) & 0xFF),
	}
}

imul_r64_imm32 :: proc(reg: Register64, imm: u32) -> [7]u8 {
	// This is the same as imul_r64_r64_imm32 where dst and src are the same
	return imul_r64_r64_imm32(reg, reg, imm)
}

div_r64 :: proc(reg: Register64) -> [3]u8 { 	// Unsigned divide
	// REX.W + F7 /6 (48 F7 /6)
	return [3]u8{0x48 + ((u8(reg) & 0x8) >> 3), 0xF7, 0xF0 + (u8(reg) & 0x7)}
}

idiv_r64 :: proc(reg: Register64) -> [3]u8 { 	// Signed divide
	// REX.W + F7 /7 (48 F7 /7)
	return [3]u8 {
		0x48 + ((u8(reg) & 0x8) >> 3),
		0xF7,
		0xF8 + (u8(reg) & 0x7), // ModR/M byte for signed divide
	}
}

// ===== BITWISE INSTRUCTIONS =====
// --- Basic Bitwise Operations ---
and_r64_r64 :: proc(dst: Register64, src: Register64) {}
or_r64_r64 :: proc(dst: Register64, src: Register64) {}
xor_r64_r64 :: proc(dst: Register64, src: Register64) {}
not_r64 :: proc(reg: Register64) {}

// --- Shifts and Rotates ---
shl_r64_imm8 :: proc(reg: Register64, imm: u8) {
	assert(imm <= 63, "Shift amount must be 0-63")
}
shr_r64_imm8 :: proc(reg: Register64, imm: u8) {
	assert(imm <= 63, "Shift amount must be 0-63")
}
rol_r64_imm8 :: proc(reg: Register64, imm: u8) {
	assert(imm <= 63, "Rotate amount must be 0-63")
}
ror_r64_imm8 :: proc(reg: Register64, imm: u8) {
	assert(imm <= 63, "Rotate amount must be 0-63")
}
shld_r64_r64_imm8 :: proc(dst: Register64, src: Register64, imm: u8) {}
shrd_r64_r64_imm8 :: proc(dst: Register64, src: Register64, imm: u8) {}

// --- Bit Manipulation ---
bt_r64_r64 :: proc(reg: Register64, bit_index: Register64) {} 	// Bit test
bts_r64_r64 :: proc(reg: Register64, bit_index: Register64) {} 	// Bit test and set
btr_r64_r64 :: proc(reg: Register64, bit_index: Register64) {} 	// Bit test and reset
btc_r64_r64 :: proc(reg: Register64, bit_index: Register64) {} 	// Bit test and complement
bsf_r64_r64 :: proc(dst: Register64, src: Register64) {} 	// Bit scan forward
bsr_r64_r64 :: proc(dst: Register64, src: Register64) {} 	// Bit scan reverse
popcnt_r64_r64 :: proc(dst: Register64, src: Register64) {} 	// Population count
lzcnt_r64_r64 :: proc(dst: Register64, src: Register64) {} 	// Leading zero count
tzcnt_r64_r64 :: proc(dst: Register64, src: Register64) {} 	// Trailing zero count
pext_r64_r64_r64 :: proc(dst: Register64, src1: Register64, src2: Register64) {} 	// Parallel bits extract
pdep_r64_r64_r64 :: proc(dst: Register64, src1: Register64, src2: Register64) {} 	// Parallel bits deposit

// ===== COMPARISON INSTRUCTIONS =====
cmp_r64_r64 :: proc(reg1: Register64, reg2: Register64) {}
cmp_r64_imm32 :: proc(reg: Register64, imm: u32) {}
test_r64_r64 :: proc(reg1: Register64, reg2: Register64) {}
test_r64_imm32 :: proc(reg: Register64, imm: u32) {}

// --- Conditional Moves ---
cmove_r64_r64 :: proc(dst: Register64, src: Register64) {} 	// Move if equal
cmovne_r64_r64 :: proc(dst: Register64, src: Register64) {} 	// Move if not equal
cmova_r64_r64 :: proc(dst: Register64, src: Register64) {} 	// Move if above
cmovae_r64_r64 :: proc(dst: Register64, src: Register64) {} 	// Move if above or equal
cmovb_r64_r64 :: proc(dst: Register64, src: Register64) {} 	// Move if below
cmovbe_r64_r64 :: proc(dst: Register64, src: Register64) {} 	// Move if below or equal

// ===== CONTROL FLOW INSTRUCTIONS =====
// --- Unconditional Jumps & Calls ---
jmp_rel32 :: proc(offset: i32) {}
jmp_r64 :: proc(reg: Register64) {}
jmp_m64 :: proc(mem_addr: u64) {}
call_rel32 :: proc(offset: i32) {}
call_r64 :: proc(reg: Register64) {}
call_m64 :: proc(mem_addr: u64) {}
ret :: proc() {}

// --- Conditional Jumps ---
je_rel32 :: proc(offset: i32) {} 	// Jump if equal
jne_rel32 :: proc(offset: i32) {} 	// Jump if not equal
jg_rel32 :: proc(offset: i32) {} 	// Jump if greater
jl_rel32 :: proc(offset: i32) {} 	// Jump if less
jge_rel32 :: proc(offset: i32) {} 	// Jump if greater or equal
jle_rel32 :: proc(offset: i32) {} 	// Jump if less or equal
ja_rel32 :: proc(offset: i32) {} 	// Jump if above
jae_rel32 :: proc(offset: i32) {} 	// Jump if above or equal
jb_rel32 :: proc(offset: i32) {} 	// Jump if below
jbe_rel32 :: proc(offset: i32) {} 	// Jump if below or equal

// --- Loop Control ---
loop_rel8 :: proc(offset: i8) {} 	// Decrement ECX/RCX; jump if not zero
loope_rel8 :: proc(offset: i8) {} 	// Loop while equal
loopne_rel8 :: proc(offset: i8) {} 	// Loop while not equal
jecxz_rel8 :: proc(offset: i8) {} 	// Jump if ECX/RCX is zero

// --- Miscellaneous Control Flow ---
setcc_r8 :: proc(dst: u8) {}
endbr64 :: proc() {} 	// End branch (for Control-flow Enforcement Technology)

// ===== STACK MANIPULATION =====
push_r64 :: proc(reg: Register64) {}
pop_r64 :: proc(reg: Register64) {}
pushfq :: proc() {} 	// Push RFLAGS
popfq :: proc() {} 	// Pop RFLAGS

// ===== SIMD INSTRUCTIONS =====
// --- SSE/AVX Data Movement ---
movd_xmm_r64 :: proc(xmm: XMMRegister, reg: Register64) {}
movd_r64_xmm :: proc(reg: Register64, xmm: XMMRegister) {}
movups_xmm_m128 :: proc(dst: XMMRegister, mem: u64) {}
movdqu_xmm_m128 :: proc(dst: XMMRegister, mem: u64) {}

// --- SSE/AVX Arithmetic ---
addps_xmm_xmm :: proc(dst: XMMRegister, src: XMMRegister) {}
mulps_xmm_xmm :: proc(dst: XMMRegister, src: XMMRegister) {}
divps_xmm_xmm :: proc(dst: XMMRegister, src: XMMRegister) {}
sqrtps_xmm :: proc(xmm: XMMRegister) {}

// --- SSE/AVX Comparison ---
cmpps_xmm_xmm_imm8 :: proc(dst: XMMRegister, src: XMMRegister, imm: u8) {}
cmpeqps_xmm_xmm :: proc(dst: XMMRegister, src: XMMRegister) {}
cmpneqps_xmm_xmm :: proc(dst: XMMRegister, src: XMMRegister) {}

// --- AVX FMA (Fused Multiply-Add) ---
vfmadd132ps_xmm_xmm_xmm :: proc(dst: XMMRegister, src1: XMMRegister, src2: XMMRegister) {}
vfmadd213ps_xmm_xmm_xmm :: proc(dst: XMMRegister, src1: XMMRegister, src2: XMMRegister) {}
vfmadd231ps_xmm_xmm_xmm :: proc(dst: XMMRegister, src1: XMMRegister, src2: XMMRegister) {}

// --- AVX Advanced Operations ---
vaddps_ymm :: proc(dst: XMMRegister, src1: XMMRegister, src2: XMMRegister) {}
vmulps_ymm :: proc(dst: XMMRegister, src1: XMMRegister, src2: XMMRegister) {}
vdivps_ymm :: proc(dst: XMMRegister, src1: XMMRegister, src2: XMMRegister) {}
vblendps_ymm :: proc(dst: XMMRegister, src1: XMMRegister, src2: XMMRegister, imm: u8) {}
vpand_ymm :: proc(dst: XMMRegister, src1: XMMRegister, src2: XMMRegister) {}
vpor_ymm :: proc(dst: XMMRegister, src1: XMMRegister, src2: XMMRegister) {}
vpxor_ymm :: proc(dst: XMMRegister, src1: XMMRegister, src2: XMMRegister) {}
vpternlogd_ymm_ymm_ymm_imm8 :: proc(
	dst: XMMRegister,
	src1: XMMRegister,
	src2: XMMRegister,
	imm: u8,
) {}
vextracti128_ymm_ymm_imm8 :: proc(dst: XMMRegister, src: XMMRegister, imm: u8) {}

// --- SIMD Integer Operations ---
pavgb_xmm_xmm :: proc(dst: XMMRegister, src: XMMRegister) {}
pavgb_ymm_ymm :: proc(dst: XMMRegister, src: XMMRegister) {}
pmaddwd_xmm_xmm :: proc(dst: XMMRegister, src: XMMRegister) {}
pmulhuw_xmm_xmm :: proc(dst: XMMRegister, src: XMMRegister) {}

// --- AVX-512 Operations ---
vaddpd_zmm_zmm_zmm :: proc(dst: XMMRegister, src1: XMMRegister, src2: XMMRegister) {}
vsubpd_zmm_zmm_zmm :: proc(dst: XMMRegister, src1: XMMRegister, src2: XMMRegister) {}
vmulpd_zmm_zmm_zmm :: proc(dst: XMMRegister, src1: XMMRegister, src2: XMMRegister) {}
vdivpd_zmm_zmm_zmm :: proc(dst: XMMRegister, src1: XMMRegister, src2: XMMRegister) {}
vgatherdps_xmm :: proc(dst: XMMRegister) {}
vscatterdps_xmm :: proc(dst: XMMRegister) {}
kmovq_k_k :: proc(dst: u8, src: u8) {}
vpxordq_zmm_zmm_zmm :: proc(dst: XMMRegister, src1: XMMRegister, src2: XMMRegister) {}
vpscatterdd_ymm_m :: proc(src: XMMRegister, mem: u64) {}
vpscatterdq_ymm_m :: proc(src: XMMRegister, mem: u64) {}
vpscatterqd_ymm_m :: proc(src: XMMRegister, mem: u64) {}
vpcompressd_ymm_ymm :: proc(dst: XMMRegister, src: XMMRegister) {}
vpcompressq_ymm_ymm :: proc(dst: XMMRegister, src: XMMRegister) {}

// ===== CRYPTOGRAPHY INSTRUCTIONS =====
aesenc :: proc(dst: XMMRegister, src: XMMRegister) {} 	// AES encryption round
aesdec :: proc(dst: XMMRegister, src: XMMRegister) {} 	// AES decryption round
pclmulqdq :: proc(dst: XMMRegister, src: XMMRegister, imm: u8) {} 	// Carryless multiplication
crc32_r64_r64 :: proc(dst: Register64, src: Register64) {} 	// CRC-32 computation

// ===== X87 FLOATING POINT INSTRUCTIONS =====
fadd_st0_st :: proc(src: x87Register) {} 	// Add
fsub_st0_st :: proc(src: x87Register) {} 	// Subtract
fmul_st0_st :: proc(src: x87Register) {} 	// Multiply
fdiv_st0_st :: proc(src: x87Register) {} 	// Divide
fcom_st :: proc(src: x87Register) {} 	// Compare floating point
fcomp_st :: proc(src: x87Register) {} 	// Compare floating point and pop
fucom_st :: proc(src: x87Register) {} 	// Unordered compare floating point
fucomp_st :: proc(src: x87Register) {} 	// Unordered compare floating point and pop
fldcw :: proc(mem: u64) {} 	// Load x87 FPU control word
fstcw :: proc(mem: u64) {} 	// Store x87 FPU control word
fnstcw :: proc(mem: u64) {} 	// Store x87 FPU control word without checking exceptions
fninit :: proc() {} 	// Initialize x87 FPU
fwait :: proc() {} 	// Wait for x87 FPU to complete current instruction

// ===== MEMORY AND STRING OPERATIONS =====
movs_m64_m64 :: proc() {} 	// Move string
stos_m64 :: proc() {} 	// Store string
cmps_m64_m64 :: proc() {} 	// Compare string
rep_movs :: proc() {} 	// Repeat move string
rep_stos :: proc() {} 	// Repeat store string
rep_cmps :: proc() {} 	// Repeat compare string

// ===== SYSTEM INSTRUCTIONS =====
// --- Interrupts and System Calls ---
syscall :: proc() {} 	// Fast system call
sysret :: proc() {} 	// Return from system call
int_imm8 :: proc(imm: u8) {} 	// Generate software interrupt
int3 :: proc() {} 	// Breakpoint
into :: proc() {} 	// Interrupt on overflow
iret :: proc() {} 	// Interrupt return

// --- Processor Control ---
cpuid :: proc() {} 	// CPU identification
rdtsc :: proc() {} 	// Read time-stamp counter
rdtscp :: proc() {} 	// Read time-stamp counter and processor ID
rdmsr :: proc() {} 	// Read model-specific register
wrmsr :: proc() {} 	// Write model-specific register
rdpmc :: proc() {} 	// Read performance-monitoring counter
hlt :: proc() {} 	// Halt processor

// --- Process & Memory Management ---
swapgs :: proc() {} 	// Swap GS base register
wrpkru :: proc() {} 	// Write PKRU register
rdpkru :: proc() {} 	// Read PKRU register
clac :: proc() {} 	// Clear AC flag (prevents SMAP violations)
stac :: proc() {} 	// Set AC flag (allows supervisor mode access)
ud2 :: proc() {} 	// Undefined instruction (for debugging)

// --- Virtualization Instructions ---
vmcall :: proc() {} 	// VMX guest system call
vmlaunch :: proc() {} 	// Launch VMX non-root operation
vmresume :: proc() {} 	// Resume VMX non-root operation
vmxoff :: proc() {} 	// Exit VMX non-root operation

// ===== ATOMIC OPERATIONS =====
lock_xadd_r64_r64 :: proc(dst: Register64, src: Register64) {} 	// Locked exchange and add
lock_cmpxchg_r64_r64 :: proc(dst: Register64, src: Register64) {} 	// Locked compare and exchange
lock_inc_r64 :: proc(reg: Register64) {} 	// Locked increment
lock_dec_r64 :: proc(reg: Register64) {} 	// Locked decrement
lock_xchg_r64_r64 :: proc(dst: Register64, src: Register64) {} 	// Locked exchange
atomic_load_r64 :: proc(dst: Register64, src: u64) {} 	// Atomic load
atomic_store_r64 :: proc(dst: u64, src: Register64) {} 	// Atomic store

// ===== TRANSACTIONAL MEMORY INSTRUCTIONS =====
xbegin :: proc(offset: i32) {} 	// Begin transaction
xend :: proc() {} 	// End transaction
xabort :: proc(imm: u8) {} 	// Abort transaction
xtest :: proc() {} 	// Test if executing in a transaction

// ===== RANDOM NUMBER GENERATION =====
rdrand_r64 :: proc(reg: Register64) {} 	// Read random number
rdseed_r64 :: proc(reg: Register64) {} 	// Read random seed

// ===== PREFETCH INSTRUCTIONS =====
prefetcht0 :: proc(mem: u64) {} 	// Prefetch to all cache levels
prefetcht1 :: proc(mem: u64) {} 	// Prefetch to L2 cache
prefetcht2 :: proc(mem: u64) {} 	// Prefetch to L3 cache
prefetchnta :: proc(mem: u64) {} 	// Prefetch with minimal cache pollution

// ===== MEMORY MANAGEMENT AND OPTIMIZATION =====
clflush_m64 :: proc(mem: u64) {} 	// Flush cache line
clflushopt_m64 :: proc(mem: u64) {} 	// Optimized flush cache line
clwb_m64 :: proc(mem: u64) {} 	// Cache line write back
bound_r64_m128 :: proc(reg: Register64, mem: u64) {} 	// Check array bounds

// ===== POWER MANAGEMENT =====
monitor_r64_r64_r64 :: proc(reg1: Register64, reg2: Register64, reg3: Register64) {} 	// Set up monitor address
mwait_r64_r64 :: proc(reg1: Register64, reg2: Register64) {} 	// Monitor wait

// ===== MEMORY FENCES =====
mfence :: proc() {} 	// Memory fence
lfence :: proc() {} 	// Load fence
sfence :: proc() {} 	// Store fence

// ===== PERFORMANCE MONITORING =====
perfmon_instructions :: proc() {} 	// Placeholder for various performance monitoring instructions

// Helper: automated test equality function.
test_equal :: proc(test_name: string, expected: []u8, actual: []u8) {
	if len(expected) != len(actual) {
		fmt.printf("FAIL: %s: expected length %d, got %d\n", test_name, len(expected), len(actual))
		return
	}
	for v, i in expected {
		if v != actual[i] {
			fmt.printf(
				"FAIL: %s: at index %d, expected 0x%X, got 0x%X\n",
				test_name,
				i,
				v,
				actual[i],
			)
			return
		}
	}
	fmt.printf("PASS: %s\n", test_name)
}

main :: proc() {
	// mov_r64_imm64: mov rax, 0x123456789ABCDEF0
	t1: [10]u8 = mov_r64_imm64(Register64.RAX, 0x123456789ABCDEF0)
	expected1: []u8 = asm_to_bytes("mov rax 0x123456789ABCDEF0")
	test_equal("mov_r64_imm64", expected1, t1[:])

	// mov_r64_r64: mov rbx, rax
	t2: [3]u8 = mov_r64_r64(Register64.RBX, Register64.RAX)
	expected2: [3]u8 = [3]u8{0x48, 0x89, 0xC3}
	test_equal("mov_r64_r64", expected2[:], t2[:])

	// mov_r64_m64: mov rcx, [0x00400000]
	t3: [8]u8 = mov_r64_m64(Register64.RCX, 0x00400000)
	expected3: [8]u8 = [8]u8{0x48, 0x8B, 0x0C, 0x25, 0x00, 0x00, 0x40, 0x00}
	test_equal("mov_r64_m64", expected3[:], t3[:])

	// mov_m64_r64: mov [0x00400000], rdx
	t4: [8]u8 = mov_m64_r64(0x00400000, Register64.RDX)
	expected4: [8]u8 = [8]u8{0x48, 0x89, 0x14, 0x25, 0x00, 0x00, 0x40, 0x00}
	test_equal("mov_m64_r64", expected4[:], t4[:])

	// movabs_r64_imm64: movabs rsi, 0xDEADBEEFCAFEBABE
	t5: [10]u8 = movabs_r64_imm64(Register64.RSI, 0xDEADBEEFCAFEBABE)
	expected5: [10]u8 = [10]u8{0x48, 0xBE, 0xBE, 0xBA, 0xFE, 0xCA, 0xEF, 0xBE, 0xAD, 0xDE}
	test_equal("movabs_r64_imm64", expected5[:], t5[:])

	// xchg_r64_r64: xchg rdi, rbx
	t6: [3]u8 = xchg_r64_r64(Register64.RDI, Register64.RBX)
	expected6: [3]u8 = [3]u8{0x48, 0x87, 0xDF}
	test_equal("xchg_r64_r64", expected6[:], t6[:])

	// movsx_r64_r32: movsx rax, ecx
	t7: [3]u8 = movsx_r64_r32(Register64.RAX, Register64.RCX)
	expected7: [3]u8 = [3]u8{0x48, 0x63, 0xC1}
	test_equal("movsx_r64_r32", expected7[:], t7[:])

	// movzx_r64_r32: movzx rdx, ebx
	t8: [3]u8 = movzx_r64_r32(Register64.RDX, Register64.RBX)
	expected8: [3]u8 = [3]u8{0x40, 0x89, 0xDA}
	test_equal("movzx_r64_r32", expected8[:], t8[:])

	// movbe_r64_m64: movbe rsi, [0x00400000]
	t9: [10]u8 = movbe_r64_m64(Register64.RSI, 0x00400000)
	expected9: [10]u8 = [10]u8{0x48, 0x0F, 0x38, 0xF0, 0x34, 0x25, 0x00, 0x00, 0x40, 0x00}
	test_equal("movbe_r64_m64", expected9[:], t9[:])

	// movbe_m64_r64: movbe [0x00400000], rdi
	t10: [10]u8 = movbe_m64_r64(0x00400000, Register64.RDI)
	expected10: [10]u8 = [10]u8{0x48, 0x0F, 0x38, 0xF1, 0x3C, 0x25, 0x00, 0x00, 0x40, 0x00}
	test_equal("movbe_m64_r64", expected10[:], t10[:])

	// bswap_r64: bswap rcx
	t11: [3]u8 = bswap_r64(Register64.RCX)
	expected11: [3]u8 = [3]u8{0x48, 0x0F, 0xC9}
	test_equal("bswap_r64", expected11[:], t11[:])

	// mov_cr_r64: mov cr3, rbx
	t12: [3]u8 = mov_cr_r64(3, Register64.RBX)
	expected12: [3]u8 = [3]u8{0x0F, 0x22, 0xDB}
	test_equal("mov_cr_r64", expected12[:], t12[:])

	// mov_r64_cr: mov rax, cr0
	t13: [3]u8 = mov_r64_cr(Register64.RAX, 0)
	expected13: [3]u8 = [3]u8{0x0F, 0x20, 0xC0}
	test_equal("mov_r64_cr", expected13[:], t13[:])

	// mov_cr0_r64: mov cr0, rdx
	t14: [3]u8 = mov_cr0_r64(Register64.RDX)
	expected14: [3]u8 = [3]u8{0x0F, 0x22, 0xC2}
	test_equal("mov_cr0_r64", expected14[:], t14[:])

	// mov_dr0_r64: mov dr0, rbx
	t15: [3]u8 = mov_dr0_r64(Register64.RBX)
	expected15: [3]u8 = [3]u8{0x0F, 0x23, 0xC3}
	test_equal("mov_dr0_r64", expected15[:], t15[:])

	// mov_dr1_r64: mov dr1, rcx
	t16: [3]u8 = mov_dr1_r64(Register64.RCX)
	expected16: [3]u8 = [3]u8{0x0F, 0x23, 0xC9}
	test_equal("mov_dr1_r64", expected16[:], t16[:])

	// mov_dr2_r64: mov dr2, rdx
	t17: [3]u8 = mov_dr2_r64(Register64.RDX)
	expected17: [3]u8 = [3]u8{0x0F, 0x23, 0xD2}
	test_equal("mov_dr2_r64", expected17[:], t17[:])

	// mov_dr3_r64: mov dr3, rdi
	t18: [3]u8 = mov_dr3_r64(Register64.RDI)
	expected18: [3]u8 = [3]u8{0x0F, 0x23, 0xDF}
	test_equal("mov_dr3_r64", expected18[:], t18[:])

	// mov_dr6_r64: mov dr6, rax
	t19: [3]u8 = mov_dr6_r64(Register64.RAX)
	expected19: [3]u8 = [3]u8{0x0F, 0x23, 0xF0}
	test_equal("mov_dr6_r64", expected19[:], t19[:])

	// mov_dr7_r64: mov dr7, rbx
	t20: [3]u8 = mov_dr7_r64(Register64.RBX)
	expected20: [3]u8 = [3]u8{0x0F, 0x23, 0xFB}
	test_equal("mov_dr7_r64", expected20[:], t20[:])

	// lea_r64_m64: lea rsi, [0x00400000]
	t21: [8]u8 = lea_r64_m64(Register64.RSI, 0x00400000)
	expected21: [8]u8 = [8]u8{0x48, 0x8D, 0x34, 0x25, 0x00, 0x00, 0x40, 0x00}
	test_equal("lea_r64_m64", expected21[:], t21[:])

	// add_r64_imm32: add rbx, 0x12345678
	t22: [7]u8 = add_r64_imm32(Register64.RBX, 0x12345678)
	expected22: [7]u8 = [7]u8{0x48, 0x81, 0xC3, 0x78, 0x56, 0x34, 0x12}
	test_equal("add_r64_imm32", expected22[:], t22[:])

	// add_r64_r64: add rax, rcx
	t23: [3]u8 = add_r64_r64(Register64.RAX, Register64.RCX)
	expected23: [3]u8 = [3]u8{0x48, 0x01, 0xC8}
	test_equal("add_r64_r64", expected23[:], t23[:])

	// sub_r64_imm32: sub rdx, 0x87654321
	t24: [7]u8 = sub_r64_imm32(Register64.RDX, 0x87654321)
	expected24: [7]u8 = [7]u8{0x48, 0x81, 0xEA, 0x21, 0x43, 0x65, 0x87}
	test_equal("sub_r64_imm32", expected24[:], t24[:])

	// sub_r64_r64: sub rbx, rsi
	t25: [3]u8 = sub_r64_r64(Register64.RBX, Register64.RSI)
	expected25: [3]u8 = [3]u8{0x48, 0x29, 0xF3}
	test_equal("sub_r64_r64", expected25[:], t25[:])

	// inc_r64: inc rax
	t26: [3]u8 = inc_r64(Register64.RAX)
	expected26: [3]u8 = [3]u8{0x48, 0xFF, 0xC0}
	test_equal("inc_r64", expected26[:], t26[:])

	// dec_r64: dec rcx
	t27: [3]u8 = dec_r64(Register64.RCX)
	expected27: [3]u8 = [3]u8{0x48, 0xFF, 0xC9}
	test_equal("dec_r64", expected27[:], t27[:])

	// neg_r64: neg rdx
	t28: [3]u8 = neg_r64(Register64.RDX)
	expected28: [3]u8 = [3]u8{0x48, 0xF7, 0xDA}
	test_equal("neg_r64", expected28[:], t28[:])

	// adc_r64_r64: adc rbx, rsi
	t29: [3]u8 = adc_r64_r64(Register64.RBX, Register64.RSI)
	expected29: [3]u8 = [3]u8{0x48, 0x11, 0xF3}
	test_equal("adc_r64_r64", expected29[:], t29[:])

	// sbb_r64_r64: sbb rdi, rbx
	t30: [3]u8 = sbb_r64_r64(Register64.RDI, Register64.RBX)
	expected30: [3]u8 = [3]u8{0x48, 0x19, 0xDF}
	test_equal("sbb_r64_r64", expected30[:], t30[:])

	// xadd_r64_r64: xadd rdx, rbx
	t31: [4]u8 = xadd_r64_r64(Register64.RDX, Register64.RBX)
	expected31: [4]u8 = [4]u8{0x48, 0x0F, 0xC1, 0xDA}
	test_equal("xadd_r64_r64", expected31[:], t31[:])

	// mul_r64: mul rsi
	t32: [3]u8 = mul_r64(Register64.RSI)
	expected32: [3]u8 = [3]u8{0x48, 0xF7, 0xE6}
	test_equal("mul_r64", expected32[:], t32[:])

	// imul_r64_r64: imul rax, rdi
	t33: [4]u8 = imul_r64_r64(Register64.RAX, Register64.RDI)
	expected33: [4]u8 = [4]u8{0x48, 0x0F, 0xAF, 0xC7}
	test_equal("imul_r64_r64", expected33[:], t33[:])

	// imul_r64_r64_imm32: imul rbx, rcx, 0x11223344
	t34: [7]u8 = imul_r64_r64_imm32(Register64.RBX, Register64.RCX, 0x11223344)
	expected34: [7]u8 = [7]u8{0x48, 0x69, 0xD9, 0x44, 0x33, 0x22, 0x11}
	test_equal("imul_r64_r64_imm32", expected34[:], t34[:])

	// imul_r64_imm32: imul rdx, 0x55667788
	t35: [7]u8 = imul_r64_imm32(Register64.RDX, 0x55667788)
	expected35: []u8 = []u8{0x48, 0x69, 0xD2, 0x88, 0x77, 0x66, 0x55}
	test_equal("imul_r64_imm32", expected35, t35[:])

	// div_r64: div rsi
	t36: [3]u8 = div_r64(Register64.RSI)
	expected36: [3]u8 = [3]u8{0x48, 0xF7, 0xF6}
	test_equal("div_r64", expected36[:], t36[:])

	// idiv_r64: idiv rdi
	t37: [3]u8 = idiv_r64(Register64.RDI)
	expected37: [3]u8 = [3]u8{0x48, 0xF7, 0xFF}
	test_equal("idiv_r64", expected37[:], t37[:])
}
