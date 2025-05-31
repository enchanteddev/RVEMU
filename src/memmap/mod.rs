use std::ops::Range;

use crate::machine::DynamicMemory;

mod uart;

#[derive(Debug)]
pub enum MemMapperError {
    AddressOutOfBounds,
}

pub trait MemMapper {
    // called JUST AFTER a write, so can still modify before it gives control back to the machine
    fn on_write(&mut self, addr: u32, dmem: &mut DynamicMemory) -> Result<(), MemMapperError>;
    // called JUST BEFORE a read, so can give whatever value it wants
    fn on_read(&mut self, addr: u32, dmem: &mut DynamicMemory) -> Result<u8, MemMapperError>;
    
    fn mem_bounds(&self) -> Range<u32>;
}
