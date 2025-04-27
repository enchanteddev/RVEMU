use decoder::{IInstr, Instruction, RInstr, SInstr, UInstr};
use std::{
    collections::HashMap,
    io::{self, Read},
    path::Path,
};

mod decoder;

struct DynamicMemory {
    inner: HashMap<u32, u8>,
}

impl DynamicMemory {
    fn new() -> Self {
        DynamicMemory {
            inner: HashMap::new(),
        }
    }

    fn load(&mut self, addr: u32) -> u8 {
        match self.inner.get(&addr) {
            Some(value) => *value,
            None => {
                self.inner.insert(addr, 0);
                0
            }
        }
    }

    fn store(&mut self, addr: u32, value: u8) {
        self.inner.insert(addr, value);
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
            Instruction::AUIPC(UInstr { imm, rd }) => {
                self.registers[rd as usize] = self.pc + imm
            }
            Instruction::JAL(UInstr { imm, rd }) => {
                self.registers[rd as usize] = self.pc + 1;
                self.pc += imm >> 2;
            },
            Instruction::JALR(IInstr { imm, rs1, rd }) => {
                self.registers[rd as usize] = self.pc + 1;
                self.pc = self.registers[rs1 as usize] + (imm >> 2);
            },
            Instruction::BEQ(SInstr { imm, rs1, rs2 }) => {
                if self.registers[rs1 as usize] == self.registers[rs2 as usize] {
                    self.pc = self.pc.wrapping_add_signed(sign_extend(imm, 12) as i32 >> 2);
                }
            },
            Instruction::BNE(SInstr { imm, rs1, rs2 }) => {
                if self.registers[rs1 as usize] != self.registers[rs2 as usize] {
                    println!("{}", sign_extend(imm, 12) as i32);
                    self.pc = self.pc.wrapping_add_signed(sign_extend(imm, 12) as i32 >> 2);
                }
            },
            Instruction::BLT(SInstr { imm, rs1, rs2 }) => {
                if (self.registers[rs1 as usize] as i32) < (self.registers[rs2 as usize] as i32) {
                    self.pc = self.pc.wrapping_add_signed(sign_extend(imm, 12) as i32 >> 2);
                }
            },
            Instruction::BGE(SInstr { imm, rs1, rs2 }) => {
                if (self.registers[rs1 as usize] as i32) >= (self.registers[rs2 as usize] as i32) {
                    self.pc = self.pc.wrapping_add_signed(sign_extend(imm, 12) as i32 >> 2);
                }
            },
            Instruction::BLTU(SInstr { imm, rs1, rs2 }) => {
                if self.registers[rs1 as usize] < self.registers[rs2 as usize] {
                    self.pc = self.pc.wrapping_add_signed(sign_extend(imm, 12) as i32 >> 2);
                }
            },
            Instruction::BGEU(SInstr { imm, rs1, rs2 }) => {
                if self.registers[rs1 as usize] >= self.registers[rs2 as usize] {
                    self.pc = self.pc.wrapping_add_signed(sign_extend(imm, 12) as i32 >> 2);
                }
            },
            Instruction::LB(IInstr { imm, rs1, rd }) => {
                let addr = self.registers[rs1 as usize].wrapping_add(sign_extend(imm, 12));
                self.registers[rd as usize] = sign_extend(self.memory.load(addr as u32) as u32, 8);
            },
            Instruction::LH(IInstr { imm, rs1, rd }) => {
                let addr = self.registers[rs1 as usize].wrapping_add(sign_extend(imm, 12));
                let first_byte = self.memory.load(addr as u32) as u32;
                let second_byte = self.memory.load(addr as u32 + 1) as u32;
                self.registers[rd as usize] = sign_extend((second_byte << 8) | first_byte, 16);
            },
            Instruction::LW(IInstr { imm, rs1, rd }) => {
                let addr = self.registers[rs1 as usize].wrapping_add(sign_extend(imm, 12));
                let first_byte = self.memory.load(addr as u32) as u32;
                let second_byte = self.memory.load(addr as u32 + 1) as u32;
                let third_byte = self.memory.load(addr as u32 + 2) as u32;
                let fourth_byte = self.memory.load(addr as u32 + 3) as u32;
                self.registers[rd as usize] =
                    fourth_byte << 24 | third_byte << 16 | second_byte << 8 | first_byte;
            },
            Instruction::LBU(IInstr { imm, rs1, rd }) => {
                let addr = self.registers[rs1 as usize].wrapping_add(sign_extend(imm, 12));
                self.registers[rd as usize] = self.memory.load(addr as u32) as u32;
            },
            Instruction::LHU(IInstr { imm, rs1, rd }) => {
                let addr = self.registers[rs1 as usize].wrapping_add(sign_extend(imm, 12));
                let first_byte = self.memory.load(addr as u32) as u32;
                let second_byte = self.memory.load(addr as u32 + 1) as u32;
                self.registers[rd as usize] = sign_extend((first_byte << 8) | second_byte, 16);
            },
            Instruction::SB(SInstr { imm, rs1, rs2 }) => {
                let addr = self.registers[rs1 as usize].wrapping_add(sign_extend(imm, 12));
                self.memory
                    .store(addr as u32, self.registers[rs2 as usize] as u8);
            },
            Instruction::SH(SInstr { imm, rs1, rs2 }) => {
                let addr = self.registers[rs1 as usize].wrapping_add(sign_extend(imm, 12));
                let reg_val = self.registers[rs2 as usize];
                let first_byte = reg_val as u8;
                let second_byte = (reg_val >> 8) as u8;
                self.memory.store(addr as u32, first_byte);
                self.memory.store(addr as u32 + 1, second_byte);
            },
            Instruction::SW(SInstr { imm, rs1, rs2 }) => {
                let addr = self.registers[rs1 as usize].wrapping_add(sign_extend(imm, 12));
                let reg_val = self.registers[rs2 as usize];
                let first_byte = reg_val as u8;
                let second_byte = (reg_val >> 8) as u8;
                let third_byte = (reg_val >> 16) as u8;
                let fourth_byte = (reg_val >> 24) as u8;
                self.memory.store(addr as u32, first_byte);
                self.memory.store(addr as u32 + 1, second_byte);
                self.memory.store(addr as u32 + 2, third_byte);
                self.memory.store(addr as u32 + 3, fourth_byte);
            },
            Instruction::ADDI(IInstr { imm, rs1, rd }) => {
                self.registers[rd as usize] = self.registers[rs1 as usize].wrapping_add(sign_extend(imm, 12));
            },
            Instruction::SLTI(IInstr { imm, rs1, rd }) => {
                self.registers[rd as usize] = if self.registers[rs1 as usize] < sign_extend(imm, 12) {
                    1
                } else {
                    0
                };
            },
            Instruction::SLTIU(IInstr { imm, rs1, rd }) => {
                self.registers[rd as usize] = if self.registers[rs1 as usize] < imm {
                    1
                } else {
                    0
                };
            },
            Instruction::XORI(IInstr { imm, rs1, rd }) => {
                self.registers[rd as usize] = self.registers[rs1 as usize] ^ sign_extend(imm, 12);
            },
            Instruction::ORI(IInstr { imm, rs1, rd }) => {
                self.registers[rd as usize] = self.registers[rs1 as usize] | sign_extend(imm, 12);
            },
            Instruction::ANDI(IInstr { imm, rs1, rd }) => {
                self.registers[rd as usize] = self.registers[rs1 as usize] & sign_extend(imm, 12);
            },
            Instruction::SLLI(RInstr { rs1, rd, rs2 }) => {
                let shamt = rs2;
                self.registers[rd as usize] = self.registers[rs1 as usize] << shamt;
            },
            Instruction::SRLI(RInstr { rs1, rd, rs2 }) => {
                let shamt = rs2;
                self.registers[rd as usize] = self.registers[rs1 as usize] >> shamt;
            },
            Instruction::SRAI(RInstr { rs1, rd, rs2 }) => {
                let shamt = rs2;
                self.registers[rd as usize] = (self.registers[rs1 as usize] as i32).wrapping_shr(shamt as u32) as u32;
            },
            Instruction::ADD(RInstr { rs1, rd, rs2 }) => {
                self.registers[rd as usize] = self.registers[rs1 as usize] + self.registers[rs2 as usize];
            },
            Instruction::SUB(RInstr { rs1, rd, rs2 }) => {
                self.registers[rd as usize] = self.registers[rs1 as usize] - self.registers[rs2 as usize];
            },
            Instruction::SLL(RInstr { rs1, rd, rs2 }) => {
                self.registers[rd as usize] = self.registers[rs1 as usize] << self.registers[rs2 as usize];
            },
            Instruction::SLT(RInstr { rs1, rd, rs2 }) => {
                self.registers[rd as usize] = if (self.registers[rs1 as usize] as i32) < (self.registers[rs2 as usize] as i32) {
                    1
                } else {
                    0
                };
            },
            Instruction::SLTU(RInstr { rs1, rd, rs2 }) => {
                self.registers[rd as usize] = if self.registers[rs1 as usize] < self.registers[rs2 as usize] {
                    1
                } else {
                    0
                };
            },
            Instruction::XOR(RInstr { rs1, rd, rs2 }) => {
                self.registers[rd as usize] = self.registers[rs1 as usize] ^ self.registers[rs2 as usize];
            },
            Instruction::SRL(RInstr { rs1, rd, rs2 }) => {
                self.registers[rd as usize] = self.registers[rs1 as usize] >> self.registers[rs2 as usize];
            },
            Instruction::SRA(RInstr { rs1, rd, rs2 }) => {
                self.registers[rd as usize] = (self.registers[rs1 as usize] as i32).wrapping_shr(self.registers[rs2 as usize] as u32) as u32;
            },
            Instruction::OR(RInstr { rs1, rd, rs2 }) => {
                self.registers[rd as usize] = self.registers[rs1 as usize] | self.registers[rs2 as usize];
            },
            Instruction::AND(RInstr { rs1, rd, rs2 }) => {
                self.registers[rd as usize] = self.registers[rs1 as usize] & self.registers[rs2 as usize];
            },
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
    let instructions_raw = read_instructions_binary(&Path::new("tests/hello.bin"));
    let instructions = instructions_raw
        .iter()
        .map(|x| decoder::decode(*x))
        .collect::<Vec<decoder::Instruction>>();

    for (instr, raw) in instructions.iter().zip(instructions_raw.iter()) {
        println!("{:?}: {:b}", instr, raw);
    }
    let mut machine = Machine::new();
    machine.load_instructions(instructions);
    loop {
        println!("{:?}", machine.instructions[machine.pc as usize]);
        machine.step();
        println!("{:?}", machine.registers);
        io::stdin().read_line(&mut String::new()).unwrap();
    }
}
