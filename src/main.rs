use std::{io::Read, path::Path};

mod decoder;

struct Machine {
    registers: [u32; 32],
    pc: u32,
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
    for i in &instructions_raw {
        println!("{:b}", i);
    }
    let instructions = instructions_raw
        .iter()
        .map(|x| decoder::decode(*x))
        .collect::<Vec<decoder::Instruction>>();

    for i in instructions {
        println!("{:?}", i);
    }
}
