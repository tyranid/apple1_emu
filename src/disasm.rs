use crate::cpu::CpuInstReader;
use crate::mem;

pub fn disasm(mem: &mem::Memory, addr: usize, count: usize) {
    let mut i: usize = 0;
    let mut curr_addr = addr as u16;
    let reader: CpuInstReader = CpuInstReader::new();

    while i < count {
        let inst = reader.read(mem, curr_addr);
        println!("{}", inst.format());
        curr_addr = curr_addr.wrapping_add(inst.size());
        i += 1;
    }
}

pub fn disasm_range(mem: &mem::Memory, addr: usize, end_addr: usize) {
    let mut curr_addr: usize = addr;
    let reader: CpuInstReader = CpuInstReader::new();

    while curr_addr < end_addr {
        let inst = reader.read(mem, curr_addr as u16);
        println!("{}", inst.format());
        curr_addr += inst.size() as usize;
    }
}