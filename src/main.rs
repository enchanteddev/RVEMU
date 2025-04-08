use std::{io::Read, path::Path};

mod decoder;

struct Machine {
    registers: [u32; 32],
    pc: u32,
}

fn read_instructions(fp: &Path) -> Vec<u32> {
    // the file is a text file with one instruction per line, in binary format
    let mut file = std::fs::File::open(fp).unwrap();
    let mut buffer = String::new();
    file.read_to_string(&mut buffer).unwrap();
    buffer
        .split('\n')
        .map(|x| u32::from_str_radix(x, 2).unwrap())
        .collect()
}

fn main() {
    let instructions_raw = read_instructions(&Path::new("tests/add.txt"));
    let instructions = instructions_raw
        .iter()
        .map(|x| decoder::decode(*x))
        .collect::<Vec<decoder::Instruction>>();

    for i in instructions {
        println!("{:?}", i);
    }
}
