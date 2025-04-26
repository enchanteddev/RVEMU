#[derive(Debug)]
pub struct RInstr {
    pub rs2: u8,
    pub rs1: u8,
    pub rd: u8,
}

#[derive(Debug)]
pub struct IInstr {
    pub imm: i32,
    pub rs1: u8,
    pub rd: u8,
}

#[derive(Debug)]
pub struct SInstr {
    pub imm: i32,
    pub rs2: u8,
    pub rs1: u8,
}

#[derive(Debug)]
pub struct UInstr {
    pub imm: i32,
    pub rd: u8,
}

#[derive(Debug)]
pub enum Instruction {
    LUI(UInstr),
    AUIPC(UInstr),
    JAL(UInstr),
    JALR(IInstr),
    BEQ(SInstr),
    BNE(SInstr),
    BLT(SInstr),
    BGE(SInstr),
    BLTU(SInstr),
    BGEU(SInstr),
    LB(IInstr),
    LH(IInstr),
    LW(IInstr),
    LBU(IInstr),
    LHU(IInstr),
    SB(SInstr),
    SH(SInstr),
    SW(SInstr),
    ADDI(IInstr),
    SLTI(IInstr),
    SLTIU(IInstr),
    XORI(IInstr),
    ORI(IInstr),
    ANDI(IInstr),
    SLLI(RInstr),
    SRLI(RInstr),
    SRAI(RInstr),
    ADD(RInstr),
    SUB(RInstr),
    SLL(RInstr),
    SLT(RInstr),
    SLTU(RInstr),
    XOR(RInstr),
    SRL(RInstr),
    SRA(RInstr),
    OR(RInstr),
    AND(RInstr),
    FENCE {
        fm: u8,
        pred: u8,
        succ: u8,
        rs1: u8,
        rd: u8,
    },
    ECALL,
    EBREAK,
}

mod opcodes {
    pub const LUI: u32 = 0b0110111;
    pub const AUIPC: u32 = 0b0010111;
    pub const JAL: u32 = 0b1101111;
    pub const JALR: u32 = 0b1100111;
    pub const FENCE: u32 = 0b0001111;
    pub const ECALL_BREAK: u32 = 0b1110011;

    pub const B_TYPE: u32 = 0b1100011;
    pub const R_TYPE: u32 = 0b0110011;
    pub const RI_TYPE: u32 = 0b0010011;
    pub const I_TYPE: u32 = 0b0000011;
    pub const S_TYPE: u32 = 0b0100011;
}

mod f3codes {
    pub const BEQ: u32 = 0b000;
    pub const BNE: u32 = 0b001;
    pub const BLT: u32 = 0b100;
    pub const BGE: u32 = 0b101;
    pub const BLTU: u32 = 0b110;
    pub const BGEU: u32 = 0b111;

    pub const SLLI: u32 = 0b001;
    pub const SRLI_SRAI: u32 = 0b101;
    pub const ADD_SUB: u32 = 0b000;
    pub const SLL: u32 = 0b001;
    pub const SLT: u32 = 0b010;
    pub const SLTU: u32 = 0b011;
    pub const XOR: u32 = 0b100;
    pub const SRL_SRA: u32 = 0b101;
    pub const OR: u32 = 0b110;
    pub const AND: u32 = 0b111;

    pub const LB: u32 = 0b000;
    pub const LH: u32 = 0b001;
    pub const LW: u32 = 0b010;
    pub const LBU: u32 = 0b100;
    pub const LHU: u32 = 0b101;

    pub const SB: u32 = 0b000;
    pub const SH: u32 = 0b001;
    pub const SW: u32 = 0b010;

    pub const ADDI: u32 = 0b000;
    pub const SLTI: u32 = 0b010;
    pub const SLTIU: u32 = 0b011;
    pub const XORI: u32 = 0b100;
    pub const ORI: u32 = 0b110;
    pub const ANDI: u32 = 0b111;
}

mod f7codes {
    pub const SRLI: u32 = 0b0000000;
    pub const SRAI: u32 = 0b0100000;
    pub const ADD: u32 = 0b0000000;
    pub const SUB: u32 = 0b0100000;
    pub const SRL: u32 = 0b0000000;
    pub const SRA: u32 = 0b0100000;
}


fn utoi(imm: u32) -> i32 {
    let bytes: [u8; 4] = imm.to_ne_bytes();
    i32::from_ne_bytes(bytes)
}

fn sign_extend(imm: u32, len: u32) -> i32 {
    let sign_bit = 1 << (len - 1);
    let mask = (2u32.pow(32 - len) - 1) << len;
    let signed_val = if imm & sign_bit != 0 {
        imm | mask
    } else {
        imm
    };
    utoi(signed_val)
}

fn decode_r(instruction: u32) -> RInstr {
    RInstr {
        rs2: ((instruction >> 20) & 0b11111) as u8,
        rs1: ((instruction >> 15) & 0b11111) as u8,
        rd: ((instruction >> 7) & 0b11111) as u8,
    }
}

fn decode_i(instruction: u32) -> IInstr {
    IInstr {
        imm: sign_extend((instruction >> 20) & 0b111111111111, 12),
        rs1: ((instruction >> 15) & 0b11111) as u8,
        rd: ((instruction >> 7) & 0b11111) as u8,
    }
}

fn decode_s(instruction: u32) -> SInstr {
    SInstr {
        imm: sign_extend(
            ((instruction >> 25) << 5) | ((instruction >> 7) & 0b11111),
            12,
        ),
        rs1: ((instruction >> 15) & 0b11111) as u8,
        rs2: ((instruction >> 20) & 0b11111) as u8,
    }
}

fn decode_u(instruction: u32) -> UInstr {
    UInstr {
        imm: utoi(instruction & !0b111111111111),
        rd: ((instruction >> 7) & 0b11111) as u8,
    }
}

fn decode_b(instruction: u32) -> SInstr {
    SInstr {
        imm: sign_extend(
            ((instruction >> 31) << 11)
                | ((instruction >> 7 & 0b1) << 10)
                | (((instruction >> 25) & 0b0111111) << 4)
                | (((instruction >> 8) & 0b1111) << 1),
            13,
        ),
        rs1: ((instruction >> 15) & 0b11111) as u8,
        rs2: ((instruction >> 20) & 0b11111) as u8,
    }
}

fn decode_j(instruction: u32) -> UInstr {
    UInstr {
        imm: sign_extend(
            (((instruction >> 31) & 1) << 20)
                | (((instruction >> 12) & 0b11111111) << 12)
                | (((instruction >> 20) & 1) << 11)
                | (((instruction >> 21) & 0b1111111111) << 1),
            21,
        ),
        rd: ((instruction >> 7) & 0b11111) as u8,
    }
}

fn decode_b_with_f(instruction: u32) -> Instruction {
    let f3 = instruction >> 12 & 0b111;
    match f3 {
        f3codes::BEQ => Instruction::BEQ(decode_b(instruction)),
        f3codes::BNE => Instruction::BNE(decode_b(instruction)),
        f3codes::BLT => Instruction::BLT(decode_b(instruction)),
        f3codes::BGE => Instruction::BGE(decode_b(instruction)),
        f3codes::BLTU => Instruction::BLTU(decode_b(instruction)),
        f3codes::BGEU => Instruction::BGEU(decode_b(instruction)),
        _ => unreachable!(),
    }
}

fn decode_r_with_f(instruction: u32) -> Instruction {
    let f3 = instruction >> 12 & 0b111;
    let f7 = instruction >> 25;
    match f3 {
        f3codes::ADD_SUB => match f7 {
            f7codes::ADD => Instruction::ADD(decode_r(instruction)),
            f7codes::SUB => Instruction::SUB(decode_r(instruction)),
            _ => unreachable!(),
        },
        f3codes::SLL => Instruction::SLL(decode_r(instruction)),
        f3codes::SLT => Instruction::SLT(decode_r(instruction)),
        f3codes::SLTU => Instruction::SLTU(decode_r(instruction)),
        f3codes::XOR => Instruction::XOR(decode_r(instruction)),
        f3codes::SRL_SRA => match f7 {
            f7codes::SRL => Instruction::SRL(decode_r(instruction)),
            f7codes::SRA => Instruction::SRA(decode_r(instruction)),
            _ => unreachable!(),
        },
        f3codes::OR => Instruction::OR(decode_r(instruction)),
        f3codes::AND => Instruction::AND(decode_r(instruction)),
        _ => unreachable!(),
    }
}

fn decode_ri_with_f(instruction: u32) -> Instruction {
    let f3 = instruction >> 12 & 0b111;
    let f7 = instruction >> 25;
    match f3 {
        f3codes::ADDI => Instruction::ADDI(decode_i(instruction)),
        f3codes::SLTI => Instruction::SLTI(decode_i(instruction)),
        f3codes::SLTIU => Instruction::SLTIU(decode_i(instruction)),
        f3codes::XORI => Instruction::XORI(decode_i(instruction)),
        f3codes::ORI => Instruction::ORI(decode_i(instruction)),
        f3codes::ANDI => Instruction::ANDI(decode_i(instruction)),
        f3codes::SLLI => Instruction::SLLI(decode_r(instruction)),
        f3codes::SRLI_SRAI => match f7 {
            f7codes::SRLI => Instruction::SRLI(decode_r(instruction)),
            f7codes::SRAI => Instruction::SRAI(decode_r(instruction)),
            _ => unreachable!(),
        },
        _ => unreachable!(),
    }
}

fn decode_i_with_f(instruction: u32) -> Instruction {
    let f3 = instruction >> 12 & 0b111;
    match f3 {
        f3codes::LB => Instruction::LB(decode_i(instruction)),
        f3codes::LH => Instruction::LH(decode_i(instruction)),
        f3codes::LW => Instruction::LW(decode_i(instruction)),
        f3codes::LBU => Instruction::LBU(decode_i(instruction)),
        f3codes::LHU => Instruction::LHU(decode_i(instruction)),
        _ => {
            unreachable!()
        }
    }
}

fn decode_s_with_f(instruction: u32) -> Instruction {
    let f3 = instruction >> 12 & 0b111;
    match f3 {
        f3codes::SB => Instruction::SB(decode_s(instruction)),
        f3codes::SH => Instruction::SH(decode_s(instruction)),
        f3codes::SW => Instruction::SW(decode_s(instruction)),
        _ => {println!("Unkown instruction: f3 = {:b}, instruction = {:b}", f3, instruction); unreachable!()},
    }
}

pub fn decode(instruction: u32) -> Instruction {
    let opcode = instruction & 0b1111111;
    match opcode {
        opcodes::LUI => Instruction::LUI(decode_u(instruction)),
        opcodes::AUIPC => Instruction::AUIPC(decode_u(instruction)),
        opcodes::JAL => Instruction::JAL(decode_j(instruction)),
        opcodes::JALR => Instruction::JALR(decode_i(instruction)),
        opcodes::FENCE => {
            todo!()
        }
        opcodes::ECALL_BREAK => {
            let f7 = instruction >> 20;
            if f7 == 0 {
                Instruction::ECALL
            } else {
                Instruction::EBREAK
            }
        }
        opcodes::B_TYPE => decode_b_with_f(instruction),
        opcodes::R_TYPE => decode_r_with_f(instruction),
        opcodes::RI_TYPE => decode_ri_with_f(instruction),
        opcodes::I_TYPE => decode_i_with_f(instruction),
        opcodes::S_TYPE => decode_s_with_f(instruction),
        _ => {
            println!("Unkown instruction: {:b}", opcode);
            unreachable!()
        },
    }
}
