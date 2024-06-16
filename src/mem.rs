use std::borrow::BorrowMut;

// Trait to handle the reading and writing of a memory range.
pub trait MemoryRangeHandler {
    fn read(&self, ofs: usize) -> u8;
    fn write(&mut self, ofs: usize, value: u8);
}

pub struct ByteArrayMemoryRangeHandler {
    data: Box<[u8]>
}

impl MemoryRangeHandler for ByteArrayMemoryRangeHandler {
    fn read(&self, ofs: usize) -> u8 {
        if ofs < self.data.len() {
            self.data[ofs]   
        } else {
            0
        }
    }
    fn write(&mut self, ofs: usize, value: u8) {
        if ofs < self.data.len() {
            self.data[ofs] = value;
        }
    }
}

// A memory range.
struct MemoryRange {
    pub base: usize,
    pub size: usize,
    pub readonly: bool,
    pub handler: Box<dyn MemoryRangeHandler>
}

// The memory range for the CPU emulation.
pub struct Memory {
    ranges: Vec<MemoryRange>,
}

impl Memory {
    fn find_range(&self, addr: usize) -> Option<&MemoryRange> {
        let mut i = self.ranges.iter();
        i.find(|&r| r.base <= addr && r.base + r.size > addr)
    }

    pub fn read_u8(&self, addr: u16) -> u8 {
        match self.find_range(addr as usize) {
            Some(r) => r.handler.read(addr as usize - r.base),
            None => 0,
        }
    }

    pub fn write_u8(&mut self, addr: u16, value: u8) {
        let mut i = 0;
        let uaddr = addr as usize;
        while i < self.ranges.len() {
            let r = self.ranges[i].borrow_mut();
            if uaddr >= r.base && uaddr < r.base + r.size {
                if !r.readonly {
                    r.handler.write(uaddr - r.base, value);
                }
                break
            }
            i += 1;
        }
    }

    pub fn read_u16(&self, addr: u16) -> u16 {
        (self.read_u8(addr) as u16) | ((self.read_u8(addr+1) as u16) << 8)
    }

    /*pub fn write_u16(&mut self, addr: u16, value: u16) {
        self.write_u8(addr, (value & 0xFF) as u8);
        self.write_u8(addr+1, ((value >> 8) & 0xFF) as u8);
    }*/

    pub fn add_range<const S: usize>(&mut self, addr: u16) {
        let data: [u8; S] = [0; S]; 
        
        self.ranges.push(
            MemoryRange {
                base: addr as usize,
                size: S,
                readonly: false,
                handler: Box::new(
                    ByteArrayMemoryRangeHandler {
                        data: Box::new(data)
                    }
                ),
            }
        );
    }

    pub fn add_range_file(&mut self, path: String, addr: u16, readonly: bool) {
        let data = std::fs::read(path).unwrap();
        self.ranges.push(
            MemoryRange {
                base: addr as usize,
                size: data.len(),
                readonly: readonly,
                handler: Box::new(
                    ByteArrayMemoryRangeHandler {
                        data: data.into_boxed_slice()
                    }
                ),
            }
        );
    }

    pub fn add_range_handler(&mut self, addr: u16, size: usize, handler: Box<dyn MemoryRangeHandler>, readonly: bool) {
        self.ranges.push(
            MemoryRange {
                base: addr as usize,
                size,
                readonly,
                handler,
            }
        );
    }

    pub fn new() -> Memory {
        Memory{
            ranges: Vec::new()
        }
    }
}