use std::{
    io::{self, Read},
    path::Path,
};

mod decoder;
mod machine;
mod memmap;

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
    let mut machine = machine::Machine::new();
    machine.load_instructions(instructions);

    machine.set_pc(0x4c / 4);

    loop {
        println!("{:?}", machine.instructions[machine.pc as usize]);
        machine.step();
        println!("{:?}", machine.registers);
        io::stdin().read_line(&mut String::new()).unwrap();
    }
}
