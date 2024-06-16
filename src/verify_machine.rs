use crate::{cpu::{CpuContext, CpuInstReader}, machine::MachineHandler, mem::Memory};

pub struct VerifyMachine {
    ctx: CpuContext,
    mem: Memory,
    reader: CpuInstReader,
}

impl VerifyMachine {
    pub fn new() -> VerifyMachine {
        let mut mem: Memory = Memory::new();
        mem.add_range_file(String::from("./roms/6502_functional_test.bin"), 0, false);

        VerifyMachine {
            ctx: CpuContext::new(),
            mem,
            reader: CpuInstReader::new(),
        }
    }
}

impl MachineHandler for VerifyMachine {
    fn reset(&mut self) {
        self.ctx.set_pc(0x400);
    }

    fn run(&mut self) {
        let mut last_pc = 0;
        let mut count = 0;
        loop {
            let inst = self.ctx.get_next(&self.mem, &self.reader);
            //eprintln!("{0}", inst.format());
            (inst.handler)(&inst, &mut self.ctx, &mut self.mem);
            //self.ctx.dump();
            //eprintln!("");
            if last_pc == self.ctx.pc {
                if self.ctx.pc == 0x3469 {
                    println!("SUCCESS.");
                } else {
                    println!("{0}", inst.format());
                    self.ctx.dump();
                }
                break;
            }
            last_pc = self.ctx.pc;
            count += 1;
        }
        eprintln!("Executed {} instructions.", count);
    }
}