use std::{
    io::{self, Read},
    os::fd::AsRawFd,
};

use libc::{ECHO, ICANON, TCSANOW, tcgetattr, tcsetattr, termios};

use super::{MemMapper, MemMapperError};

const UART_START: u32 = 0x1000_0000;
const UART_SIZE: u32 = 8;

pub struct UART {}

fn putc(c: u8) {
    print!("putc: {}", c as char);
}
fn getc() -> u8 {
    let stdin = io::stdin();
    let fd = stdin.as_raw_fd();

    // Backup current termios
    let mut term: termios = unsafe { std::mem::zeroed() };
    unsafe {
        tcgetattr(fd, &mut term);
    }
    let original = term;

    // Set raw mode: disable canonical mode and echo
    let mut raw = term;
    raw.c_lflag &= !(ICANON | ECHO);
    unsafe {
        tcsetattr(fd, TCSANOW, &raw);
    }

    // Read one byte
    let byte = stdin.lock().bytes().next().unwrap().unwrap();

    // Restore original terminal settings
    unsafe {
        tcsetattr(fd, TCSANOW, &original);
    }

    byte
}

impl MemMapper for UART {
    fn on_write(
        &mut self,
        addr: u32,
        dmem: &mut super::DynamicMemory,
    ) -> Result<(), MemMapperError> {
        if !self.mem_bounds().contains(&addr) {
            return Err(MemMapperError::AddressOutOfBounds);
        }

        let intent = addr - UART_START;
        match intent {
            0 => {
                // THR, RBR
                let value = dmem.load(addr);
                putc(value);
                dmem.store(addr, 0);
            }
            1 | 2 | 3 | 4 | 5 | 6 => {}
            7 => {
                dbg!("UART Scratchpad update: {}", dmem.load(addr));
            }
            _ => {
                unreachable!()
            }
        };
        Ok(())
    }

    fn on_read(
        &mut self,
        addr: u32,
        dmem: &mut crate::DynamicMemory,
    ) -> Result<u8, MemMapperError> {
        if !self.mem_bounds().contains(&addr) {
            return Err(MemMapperError::AddressOutOfBounds);
        }
        let intent = addr - UART_START;
        match intent {
            0 => Ok(getc()),
            1 | 2 | 3 | 4 | 6 | 7 => Ok(dmem.load(addr)),
            5 => Ok(0), // always ready as read/write are blocking
            _ => unreachable!(),
        }
    }

    #[inline]
    fn mem_bounds(&self) -> std::ops::Range<u32> {
        UART_START..UART_START + UART_SIZE
    }
}
