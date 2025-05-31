use decoder::{IInstr, Instruction, RInstr, SInstr, UInstr};
use std::{
    cell::RefCell,
    collections::HashMap,
    io::{self, Read},
    mem::take,
    ops::{BitAnd, BitOr, BitXor},
    path::Path,
};

mod decoder;
mod memmap;

#[derive(Debug, Clone, Copy)]
struct Flags {
    reserved: bool,
}

impl Default for Flags {
    fn default() -> Self {
        Flags { reserved: false }
    }
}

struct DynamicMemory {
    inner: HashMap<u32, u8>,
    flags: HashMap<u32, Flags>,
    memmapped: Vec<RefCell<Box<dyn memmap::MemMapper>>>,
}

impl DynamicMemory {
    fn new() -> Self {
        DynamicMemory {
            inner: HashMap::new(),
            flags: HashMap::new(),
            memmapped: Vec::new(),
        }
    }

    fn load(&mut self, addr: u32) -> u8 {
        let mut memmapped = take(&mut self.memmapped);
        for mm in &mut memmapped {
            if mm.borrow().mem_bounds().contains(&addr) {
                return mm.borrow_mut().on_read(addr, self).unwrap();
            }
        }
        self.memmapped = memmapped;
        match self.inner.get(&addr) {
            Some(value) => *value,
            None => 0,
        }
    }

    fn load_word(&mut self, addr: u32) -> u32 {
        let first_byte = self.load(addr as u32) as u32;
        let second_byte = self.load(addr as u32 + 1) as u32;
        let third_byte = self.load(addr as u32 + 2) as u32;
        let fourth_byte = self.load(addr as u32 + 3) as u32;
        fourth_byte << 24 | third_byte << 16 | second_byte << 8 | first_byte
    }

    fn store_word(&mut self, addr: u32, value: u32) {
        let first_byte = value as u8;
        let second_byte = (value >> 8) as u8;
        let third_byte = (value >> 16) as u8;
        let fourth_byte = (value >> 24) as u8;
        self.store(addr as u32, first_byte);
        self.store(addr as u32 + 1, second_byte);
        self.store(addr as u32 + 2, third_byte);
        self.store(addr as u32 + 3, fourth_byte);
    }

    fn store(&mut self, addr: u32, value: u8) {
        self.flags.get_mut(&addr).map(|flags| {
            flags.reserved = false;
        });
        if value != 0 {
            self.inner.insert(addr, value);
        } else {
            self.inner.remove(&addr);
        }

        let mut memmapped = take(&mut self.memmapped);
        for mm in &mut memmapped {
            if mm.borrow().mem_bounds().contains(&addr) {
                mm.borrow_mut().on_write(addr, self).unwrap();
            }
        }
        self.memmapped = memmapped;
    }

    fn set_flags<T>(&mut self, addr: u32, operation: T)
    where
        T: Fn(&mut Flags),
    {
        let flags = match self.flags.get_mut(&addr) {
            Some(flags) => flags,
            None => {
                let flags = Flags::default();
                self.flags.insert(addr, flags);
                // this unwrap is guaranteed to succeed
                // because we just inserted the flags
                self.flags.get_mut(&addr).unwrap()
            }
        };
        operation(flags);
    }

    fn get_flags(&self, addr: u32) -> Flags {
        self.flags.get(&addr).copied().unwrap_or_default()
    }
}

struct Machine {
    registers: [u32; 32],
    pc: u32,
    instructions: Vec<decoder::Instruction>,
    memory: DynamicMemory,
}

fn sign_extend(imm: u32, len: u32) -> u32 {
    let sign_bit = 1 << (len - 1);
    let mask = (2u32.pow(32 - len) - 1) << len;
    let signed_val = if imm & sign_bit != 0 { imm | mask } else { imm };
    signed_val
}

impl Machine {
    fn new() -> Self {
        Machine {
            registers: [0; 32],
            pc: 0,
            instructions: Vec::new(),
            memory: DynamicMemory::new(),
        }
    }

    fn load_instructions(&mut self, instructions: Vec<decoder::Instruction>) {
        self.instructions.extend(instructions);
    }

    fn step(&mut self) {
        let instruction = self.instructions[self.pc as usize];
        let oldpc = self.pc;
        match instruction {
            Instruction::LUI(UInstr { imm, rd }) => self.registers[rd as usize] = imm,
            Instruction::AUIPC(UInstr { imm, rd }) => self.registers[rd as usize] = self.pc + imm,
            Instruction::JAL(UInstr { imm, rd }) => {
                self.registers[rd as usize] = self.pc + 1;
                self.pc += imm >> 2;
            }
            Instruction::JALR(IInstr { imm, rs1, rd }) => {
                self.registers[rd as usize] = self.pc + 1;
                self.pc = self.registers[rs1 as usize] + (imm >> 2);
            }
            Instruction::BEQ(SInstr { imm, rs1, rs2 }) => {
                if self.registers[rs1 as usize] == self.registers[rs2 as usize] {
                    self.pc = self
                        .pc
                        .wrapping_add_signed(sign_extend(imm, 12) as i32 >> 2);
                }
            }
            Instruction::BNE(SInstr { imm, rs1, rs2 }) => {
                if self.registers[rs1 as usize] != self.registers[rs2 as usize] {
                    println!("{}", sign_extend(imm, 12) as i32);
                    self.pc = self
                        .pc
                        .wrapping_add_signed(sign_extend(imm, 12) as i32 >> 2);
                }
            }
            Instruction::BLT(SInstr { imm, rs1, rs2 }) => {
                if (self.registers[rs1 as usize] as i32) < (self.registers[rs2 as usize] as i32) {
                    self.pc = self
                        .pc
                        .wrapping_add_signed(sign_extend(imm, 12) as i32 >> 2);
                }
            }
            Instruction::BGE(SInstr { imm, rs1, rs2 }) => {
                if (self.registers[rs1 as usize] as i32) >= (self.registers[rs2 as usize] as i32) {
                    self.pc = self
                        .pc
                        .wrapping_add_signed(sign_extend(imm, 12) as i32 >> 2);
                }
            }
            Instruction::BLTU(SInstr { imm, rs1, rs2 }) => {
                if self.registers[rs1 as usize] < self.registers[rs2 as usize] {
                    self.pc = self
                        .pc
                        .wrapping_add_signed(sign_extend(imm, 12) as i32 >> 2);
                }
            }
            Instruction::BGEU(SInstr { imm, rs1, rs2 }) => {
                if self.registers[rs1 as usize] >= self.registers[rs2 as usize] {
                    self.pc = self
                        .pc
                        .wrapping_add_signed(sign_extend(imm, 12) as i32 >> 2);
                }
            }
            Instruction::LB(IInstr { imm, rs1, rd }) => {
                let addr = self.registers[rs1 as usize].wrapping_add(sign_extend(imm, 12));
                self.registers[rd as usize] = sign_extend(self.memory.load(addr as u32) as u32, 8);
            }
            Instruction::LH(IInstr { imm, rs1, rd }) => {
                let addr = self.registers[rs1 as usize].wrapping_add(sign_extend(imm, 12));
                let first_byte = self.memory.load(addr as u32) as u32;
                let second_byte = self.memory.load(addr as u32 + 1) as u32;
                self.registers[rd as usize] = sign_extend((second_byte << 8) | first_byte, 16);
            }
            Instruction::LW(IInstr { imm, rs1, rd }) => {
                let addr = self.registers[rs1 as usize].wrapping_add(sign_extend(imm, 12));
                self.registers[rd as usize] = self.memory.load_word(addr as u32);
            }
            Instruction::LBU(IInstr { imm, rs1, rd }) => {
                let addr = self.registers[rs1 as usize].wrapping_add(sign_extend(imm, 12));
                self.registers[rd as usize] = self.memory.load(addr as u32) as u32;
            }
            Instruction::LHU(IInstr { imm, rs1, rd }) => {
                let addr = self.registers[rs1 as usize].wrapping_add(sign_extend(imm, 12));
                let first_byte = self.memory.load(addr as u32) as u32;
                let second_byte = self.memory.load(addr as u32 + 1) as u32;
                self.registers[rd as usize] = sign_extend((first_byte << 8) | second_byte, 16);
            }
            Instruction::SB(SInstr { imm, rs1, rs2 }) => {
                let addr = self.registers[rs1 as usize].wrapping_add(sign_extend(imm, 12));
                self.memory
                    .store(addr as u32, self.registers[rs2 as usize] as u8);
            }
            Instruction::SH(SInstr { imm, rs1, rs2 }) => {
                let addr = self.registers[rs1 as usize].wrapping_add(sign_extend(imm, 12));
                let reg_val = self.registers[rs2 as usize];
                let first_byte = reg_val as u8;
                let second_byte = (reg_val >> 8) as u8;
                self.memory.store(addr as u32, first_byte);
                self.memory.store(addr as u32 + 1, second_byte);
            }
            Instruction::SW(SInstr { imm, rs1, rs2 }) => {
                let addr = self.registers[rs1 as usize].wrapping_add(sign_extend(imm, 12));
                let reg_val = self.registers[rs2 as usize];
                self.memory.store_word(addr, reg_val);
            }
            Instruction::ADDI(IInstr { imm, rs1, rd }) => {
                self.registers[rd as usize] =
                    self.registers[rs1 as usize].wrapping_add(sign_extend(imm, 12));
            }
            Instruction::SLTI(IInstr { imm, rs1, rd }) => {
                self.registers[rd as usize] = if self.registers[rs1 as usize] < sign_extend(imm, 12)
                {
                    1
                } else {
                    0
                };
            }
            Instruction::SLTIU(IInstr { imm, rs1, rd }) => {
                self.registers[rd as usize] = if self.registers[rs1 as usize] < imm {
                    1
                } else {
                    0
                };
            }
            Instruction::XORI(IInstr { imm, rs1, rd }) => {
                self.registers[rd as usize] = self.registers[rs1 as usize] ^ sign_extend(imm, 12);
            }
            Instruction::ORI(IInstr { imm, rs1, rd }) => {
                self.registers[rd as usize] = self.registers[rs1 as usize] | sign_extend(imm, 12);
            }
            Instruction::ANDI(IInstr { imm, rs1, rd }) => {
                self.registers[rd as usize] = self.registers[rs1 as usize] & sign_extend(imm, 12);
            }
            Instruction::SLLI(RInstr { rs1, rd, rs2 }) => {
                let shamt = rs2;
                self.registers[rd as usize] = self.registers[rs1 as usize] << shamt;
            }
            Instruction::SRLI(RInstr { rs1, rd, rs2 }) => {
                let shamt = rs2;
                self.registers[rd as usize] = self.registers[rs1 as usize] >> shamt;
            }
            Instruction::SRAI(RInstr { rs1, rd, rs2 }) => {
                let shamt = rs2;
                self.registers[rd as usize] =
                    (self.registers[rs1 as usize] as i32).wrapping_shr(shamt as u32) as u32;
            }
            Instruction::ADD(RInstr { rs1, rd, rs2 }) => {
                self.registers[rd as usize] =
                    self.registers[rs1 as usize] + self.registers[rs2 as usize];
            }
            Instruction::SUB(RInstr { rs1, rd, rs2 }) => {
                self.registers[rd as usize] =
                    self.registers[rs1 as usize] - self.registers[rs2 as usize];
            }
            Instruction::SLL(RInstr { rs1, rd, rs2 }) => {
                self.registers[rd as usize] =
                    self.registers[rs1 as usize] << self.registers[rs2 as usize];
            }
            Instruction::SLT(RInstr { rs1, rd, rs2 }) => {
                self.registers[rd as usize] = if (self.registers[rs1 as usize] as i32)
                    < (self.registers[rs2 as usize] as i32)
                {
                    1
                } else {
                    0
                };
            }
            Instruction::SLTU(RInstr { rs1, rd, rs2 }) => {
                self.registers[rd as usize] =
                    if self.registers[rs1 as usize] < self.registers[rs2 as usize] {
                        1
                    } else {
                        0
                    };
            }
            Instruction::XOR(RInstr { rs1, rd, rs2 }) => {
                self.registers[rd as usize] =
                    self.registers[rs1 as usize] ^ self.registers[rs2 as usize];
            }
            Instruction::SRL(RInstr { rs1, rd, rs2 }) => {
                self.registers[rd as usize] =
                    self.registers[rs1 as usize] >> self.registers[rs2 as usize];
            }
            Instruction::SRA(RInstr { rs1, rd, rs2 }) => {
                self.registers[rd as usize] = (self.registers[rs1 as usize] as i32)
                    .wrapping_shr(self.registers[rs2 as usize] as u32)
                    as u32;
            }
            Instruction::OR(RInstr { rs1, rd, rs2 }) => {
                self.registers[rd as usize] =
                    self.registers[rs1 as usize] | self.registers[rs2 as usize];
            }
            Instruction::AND(RInstr { rs1, rd, rs2 }) => {
                self.registers[rd as usize] =
                    self.registers[rs1 as usize] & self.registers[rs2 as usize];
            }
            Instruction::MUL(RInstr { rs1, rd, rs2 }) => {
                self.registers[rd as usize] =
                    self.registers[rs1 as usize].wrapping_mul(self.registers[rs2 as usize]);
            }
            Instruction::MULH(RInstr { rs1, rd, rs2 }) => {
                let rs1_val = self.registers[rs1 as usize] as u64;
                let rs2_val = self.registers[rs2 as usize] as u64;
                self.registers[rd as usize] = (rs1_val.wrapping_mul(rs2_val)) as u32;
            }
            Instruction::MULSU(RInstr { rs1, rd, rs2 }) => {
                let rs1_val = self.registers[rs1 as usize] as i64;
                let rs2_val = self.registers[rs2 as usize] as u64;
                self.registers[rd as usize] = (rs1_val.wrapping_mul(rs2_val as i64)) as u32;
            }
            Instruction::MULU(RInstr { rs1, rd, rs2 }) => {
                let rs1_val = self.registers[rs1 as usize] as u64;
                let rs2_val = self.registers[rs2 as usize] as u64;
                self.registers[rd as usize] = rs1_val.wrapping_mul(rs2_val) as u32;
            }
            Instruction::DIV(RInstr { rs1, rd, rs2 }) => {
                let rs1_val = self.registers[rs1 as usize] as i32;
                let rs2_val = self.registers[rs2 as usize] as i32;
                self.registers[rd as usize] = rs1_val.wrapping_div(rs2_val) as u32;
            }
            Instruction::DIVU(RInstr { rs1, rd, rs2 }) => {
                let rs1_val = self.registers[rs1 as usize];
                let rs2_val = self.registers[rs2 as usize];
                self.registers[rd as usize] = rs1_val.wrapping_div(rs2_val);
            }
            Instruction::REM(RInstr { rs1, rd, rs2 }) => {
                let rs1_val = self.registers[rs1 as usize] as i32;
                let rs2_val = self.registers[rs2 as usize] as i32;
                self.registers[rd as usize] = rs1_val.wrapping_rem(rs2_val) as u32;
            }
            Instruction::REMU(RInstr { rs1, rd, rs2 }) => {
                let rs1_val = self.registers[rs1 as usize];
                let rs2_val = self.registers[rs2 as usize];
                self.registers[rd as usize] = rs1_val.wrapping_rem(rs2_val);
            }
            Instruction::LR(RInstr { rs1, rd, .. }) => {
                let addr = self.registers[rs1 as usize];
                self.registers[rd as usize] = self.memory.load_word(addr as u32);
                self.memory.set_flags(addr as u32, |flags| {
                    flags.reserved = true;
                });
            }
            Instruction::SC(RInstr { rs1, rs2, rd }) => {
                let addr = self.registers[rs1 as usize];
                let reserved = self.memory.get_flags(addr).reserved;
                if !reserved {
                    self.registers[rd as usize] = 1;
                } else {
                    let reg_val = self.registers[rs2 as usize];
                    self.memory.store_word(addr, reg_val);
                    self.registers[rd as usize] = 0;
                }
            }
            Instruction::AMOSWAP(RInstr { rs1, rs2, rd }) => {
                self.registers[rd as usize] = self.memory.load_word(self.registers[rs1 as usize]);
                self.registers.swap(rs2 as usize, rd as usize);
                self.memory
                    .store_word(self.registers[rs1 as usize], self.registers[rd as usize]);
            }
            Instruction::AMOADD(RInstr { rs1, rs2, rd }) => {
                self.registers[rd as usize] = self
                    .memory
                    .load_word(self.registers[rs1 as usize])
                    .wrapping_add(self.registers[rs2 as usize]);
                self.memory
                    .store_word(self.registers[rs1 as usize], self.registers[rd as usize]);
            }
            Instruction::AMOAND(RInstr { rs1, rs2, rd }) => {
                self.registers[rd as usize] = self
                    .memory
                    .load_word(self.registers[rs1 as usize])
                    .bitand(self.registers[rs2 as usize]);
                self.memory
                    .store_word(self.registers[rs1 as usize], self.registers[rd as usize]);
            }
            Instruction::AMOOR(RInstr { rs1, rs2, rd }) => {
                self.registers[rd as usize] = self
                    .memory
                    .load_word(self.registers[rs1 as usize])
                    .bitor(self.registers[rs2 as usize]);
                self.memory
                    .store_word(self.registers[rs1 as usize], self.registers[rd as usize]);
            }
            Instruction::AMOXOR(RInstr { rs1, rs2, rd }) => {
                self.registers[rd as usize] = self
                    .memory
                    .load_word(self.registers[rs1 as usize])
                    .bitxor(self.registers[rs2 as usize]);
                self.memory
                    .store_word(self.registers[rs1 as usize], self.registers[rd as usize]);
            }
            Instruction::AMOMAX(RInstr { rs1, rs2, rd }) => {
                self.registers[rd as usize] = self
                    .memory
                    .load_word(self.registers[rs1 as usize])
                    .max(self.registers[rs2 as usize]);
                self.memory
                    .store_word(self.registers[rs1 as usize], self.registers[rd as usize]);
            }
            Instruction::AMOMIN(RInstr { rs1, rs2, rd }) => {
                self.registers[rd as usize] = self
                    .memory
                    .load_word(self.registers[rs1 as usize])
                    .min(self.registers[rs2 as usize]);
                self.memory
                    .store_word(self.registers[rs1 as usize], self.registers[rd as usize]);
            }
            _ => {
                println!("Unknown instruction: {:?}", instruction);
            }
        }
        if oldpc == self.pc {
            self.pc += 1;
        }
    }
}

fn read_instructions_binary(fp: &Path) -> Vec<u32> {
    // the file is a binary file with one instruction per 4 bytes, in binary format
    let mut file = std::fs::File::open(fp).unwrap();
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).unwrap();
    buffer
        .chunks(4)
        .map(|x| u32::from_le_bytes(x.try_into().unwrap()))
        .collect()
}

fn main() {
    let instructions_raw = read_instructions_binary(&Path::new("tests/uart.bin"));
    let instructions = instructions_raw
        .iter()
        .map(|x| decoder::decode(*x))
        .collect::<Vec<decoder::Instruction>>();

    for (instr, raw) in instructions.iter().zip(instructions_raw.iter()) {
        println!("{:?}: {:b}", instr, raw);
    }
    let mut machine = Machine::new();
    machine.load_instructions(instructions);

    machine.pc = 0x4c / 4;

    loop {
        println!("{:?}", machine.instructions[machine.pc as usize]);
        machine.step();
        println!("{:?}", machine.registers);
        io::stdin().read_line(&mut String::new()).unwrap();
    }
}
