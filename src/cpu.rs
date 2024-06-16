use crate::mem::Memory;

pub struct CpuContext {
    pub a : u8,
    pub x : u8,
    pub y : u8,
    pub pc : u16,
    pub sp : u8,
    pub carry : bool,
    pub zero : bool,
    pub decimal : bool,
    pub overflow : bool,
    pub negative : bool,
    pub interrupt: bool,
}

impl CpuContext {
    pub fn new() -> CpuContext {
        CpuContext {
            a: 0, 
            x: 0, 
            y: 0, 
            pc: 0, 
            sp: 0, 
            carry: false, 
            zero: false, 
            decimal: false, 
            overflow: false, 
            negative: false,
            interrupt: false,
        }
    }

    pub fn dump(&self) {
        eprintln!("A: 0x{0:02x} b{0:08b}", self.a);
        eprintln!("X: 0x{0:02x} b{0:08b}", self.x);
        eprintln!("Y: 0x{0:02x} b{0:08b}", self.y);
        eprintln!("PC: 0x{0:04x}", self.pc);
        eprintln!("SP: 0x{0:04x}", self.sp);
        eprintln!("C: {}", self.carry);
        eprintln!("Z: {}", self.zero);
        eprintln!("D: {}", self.decimal);
        eprintln!("V: {}", self.overflow);
        eprintln!("N: {}", self.negative);
        eprintln!("I: {}", self.interrupt);
    }

    pub fn get_status(&self, for_brk: bool) -> u8 {
        let mut status : u8 = 0;
        if self.negative {
            status |= 0x80;
        }
        if self.overflow {
            status |= 0x40;
        }
        if for_brk {
            // Set BRK and ignore flag.
            status |= 0x30;
        }
        if self.decimal {
            status |= 0x8;
        }
        if self.interrupt {
            status |= 0x4;
        }
        if self.zero {
            status |= 0x2;
        }
        if self.carry {
            status |= 0x1;
        }
        status
    }

    pub fn set_status(&mut self, status: u8) {
        self.negative = (status & 0x80) != 0;
        self.overflow = (status & 0x40) != 0;
        self.decimal = (status & 0x8) != 0;
        self.interrupt = (status & 0x4) != 0;
        self.zero = (status & 0x2) != 0;
        self.carry = (status & 0x1) != 0;
    }

    fn update_nz(&mut self, value: u8) {
        self.negative = (value & 0x80) == 0x80;
        self.zero = value == 0;
    }

    fn set_a(&mut self, value: u8) {
        self.update_nz(value);
        self.a = value;
    }

    fn set_x(&mut self, value: u8) {
        self.update_nz(value);
        self.x = value;
    }

    fn set_y(&mut self, value: u8) {
        self.update_nz(value);
        self.y = value;
    }

    fn push_u8(&mut self, mem: &mut Memory, value: u8) {
        mem.write_u8(self.sp as u16 | 0x100, value);
        self.sp = self.sp.wrapping_sub(1);
    }

    fn push_u16(&mut self, mem: &mut Memory, value: u16) {
        self.push_u8(mem, (value >> 8) as u8);
        self.push_u8(mem, (value & 0xFF) as u8);
    }

    fn pop_u8(&mut self, mem: &Memory) -> u8 {
        self.sp = self.sp.wrapping_add(1);
        let ret = mem.read_u8(self.sp as u16 | 0x100);
        ret
    }

    fn pop_u16(&mut self, mem: &Memory) -> u16 {
        let ret = self.pop_u8(mem) as u16;
        ret | ((self.pop_u8(mem) as u16) << 8)
    }

    pub fn reset(&mut self, mem: &Memory) {
        self.pc = mem.read_u16(0xFFFC);
    }

    pub fn get_next(&mut self, mem: &Memory, reader: &CpuInstReader) -> CpuInst {
        let inst = reader.read(mem, self.pc);
        self.pc = self.pc.wrapping_add(inst.size());
        inst
    }

    pub fn set_pc(&mut self, pc: u16) {
        self.pc = pc;
    }

    fn get_carry(&self) -> u8 {
        match self.carry {
            true => 1,
            false => 0,
        }
    }
}

#[derive(Clone, Copy, PartialEq)]
pub enum CpuInstAddressingMode {
    Absolute,
    AbsoluteX,
    AbsoluteY,
    Accumulator,
    Immediate,
    Implied,
    Indirect,
    IndirectX,
    IndirectY,
    Relative,
    Zeropage,
    ZeropageX,
    ZeropageY
}

type CpuInstHandler = fn(&CpuInst, &mut CpuContext, &mut Memory);

#[derive(Clone)]
pub struct CpuInst {
    pub opcode: u8,
    pub mnemonic: String,
    pub addressing_mode: CpuInstAddressingMode,
    pub cycles: i32,
    pub page_boundary_cycle: bool,
    pub handler: CpuInstHandler,
    pub addr: u16,
    pub arg: u16
}

impl CpuInst {
    fn new(opcode: u8, mnemonic: &str, addressing_mode: CpuInstAddressingMode, cycles: i32, page_boundary_cycle: bool) -> CpuInst {
        CpuInst {
            opcode,
            mnemonic: mnemonic.to_string(),
            addressing_mode,
            cycles,
            page_boundary_cycle,
            handler: |i, _, _| {panic!("{0:02X} {1}", i.opcode, i.mnemonic);},
            addr: 0,
            arg: 0
        }
    }

    fn new_handler(opcode: u8, mnemonic: &str, addressing_mode: CpuInstAddressingMode, cycles: i32, page_boundary_cycle: bool, handler: CpuInstHandler) -> CpuInst {
        CpuInst {
            opcode,
            mnemonic: mnemonic.to_string(),
            addressing_mode,
            cycles,
            page_boundary_cycle,
            handler,
            addr: 0,
            arg: 0
        }
    }

    fn clone_with_addr(&self, addr: u16, arg: u16) -> CpuInst {
        CpuInst {
            opcode: self.opcode,
            mnemonic: self.mnemonic.clone(),
            addressing_mode: self.addressing_mode,
            cycles: self.cycles,
            page_boundary_cycle: self.page_boundary_cycle,
            handler: self.handler,
            addr,
            arg
        }
    }

    pub fn size(&self) -> u16 {
        match self.addressing_mode {
            CpuInstAddressingMode::Absolute => 3,
            CpuInstAddressingMode::AbsoluteY => 3,
            CpuInstAddressingMode::AbsoluteX => 3,
            CpuInstAddressingMode::Accumulator => 1,
            CpuInstAddressingMode::Immediate => 2,
            CpuInstAddressingMode::Implied => 1,
            CpuInstAddressingMode::Indirect => 3,
            CpuInstAddressingMode::IndirectX => 2,
            CpuInstAddressingMode::IndirectY => 2,
            CpuInstAddressingMode::Relative => 2,
            CpuInstAddressingMode::Zeropage => 2,
            CpuInstAddressingMode::ZeropageX => 2,
            CpuInstAddressingMode::ZeropageY => 2,
        }
    }

    pub fn format(&self) -> String {
        let name = format!("0x{0:04X}: {1}", self.addr, self.mnemonic);
        match self.addressing_mode {
            CpuInstAddressingMode::Absolute => format!("{0} ${1:04X}", name, self.arg),
            CpuInstAddressingMode::AbsoluteY => format!("{0} ${1:04X},Y", name, self.arg),
            CpuInstAddressingMode::AbsoluteX => format!("{0} ${1:04X},X", name, self.arg),
            CpuInstAddressingMode::Accumulator => format!("{} A", name),
            CpuInstAddressingMode::Immediate => format!("{0} #${1:02X}", name, self.arg),
            CpuInstAddressingMode::Implied => format!("{}", name),
            CpuInstAddressingMode::Indirect => format!("{0} (${1:04X})", name, self.arg),
            CpuInstAddressingMode::IndirectX => format!("{0} (${1:02X},X)", name, self.arg),
            CpuInstAddressingMode::IndirectY => format!("{0} (${1:02X}),Y", name, self.arg),
            CpuInstAddressingMode::Relative => format!("{0} ${1:04X}", name, self.addr.wrapping_add(2).wrapping_add(self.arg)),
            CpuInstAddressingMode::Zeropage => format!("{0} ${1:02X}", name, self.arg),
            CpuInstAddressingMode::ZeropageX => format!("{0} ${1:02X},X", name, self.arg),
            CpuInstAddressingMode::ZeropageY => format!("{0} ${1:02X},Y", name, self.arg),
        }
    }

    fn get_ea(&self, ctx: &CpuContext, mem: &Memory) -> u16 {
        match self.addressing_mode {
            CpuInstAddressingMode::Absolute => self.arg,
            CpuInstAddressingMode::AbsoluteX => self.arg.wrapping_add(ctx.x as u16),
            CpuInstAddressingMode::AbsoluteY => self.arg.wrapping_add(ctx.y as u16),
            CpuInstAddressingMode::Indirect => mem.read_u16(self.arg),
            CpuInstAddressingMode::IndirectX => mem.read_u16(self.arg.wrapping_add(ctx.x as u16) & 0xFF),
            CpuInstAddressingMode::IndirectY => mem.read_u16(self.arg).wrapping_add(ctx.y as u16),
            CpuInstAddressingMode::Zeropage => self.arg,
            CpuInstAddressingMode::ZeropageX => self.arg.wrapping_add(ctx.x as u16) & 0xFF,
            CpuInstAddressingMode::ZeropageY => self.arg.wrapping_add(ctx.y as u16) & 0xFF,
            _ => 0
        }
    }

    fn get_value(&self, ctx: &CpuContext, mem: &Memory) -> u8 {
        if self.addressing_mode == CpuInstAddressingMode::Immediate {
            self.arg as u8
        } else {
            let ea = self.get_ea(ctx, mem);
            mem.read_u8(ea)
        }
    }

    fn eor_handler(&self, ctx:&mut CpuContext, mem:&mut Memory) {
        ctx.set_a(ctx.a ^ self.get_value(ctx, mem));
    }

    fn ora_handler(&self, ctx:&mut CpuContext, mem:&mut Memory) {
        ctx.set_a(ctx.a | self.get_value(ctx, mem));
    }

    fn lda_handler(&self, ctx:&mut CpuContext, mem:&mut Memory) {
        ctx.set_a(self.get_value(ctx, mem));
    }

    fn ldx_handler(&self, ctx:&mut CpuContext, mem:&mut Memory) {
        ctx.set_x(self.get_value(ctx, mem));
    }

    fn ldy_handler(&self, ctx:&mut CpuContext, mem:&mut Memory) {
        ctx.set_y(self.get_value(ctx, mem));
    }

    fn sta_handler(&self, ctx:&mut CpuContext, mem:&mut Memory) {
        let ea = self.get_ea(ctx, mem);
        mem.write_u8(ea, ctx.a);
    }

    fn stx_handler(&self, ctx:&mut CpuContext, mem:&mut Memory) {
        let ea = self.get_ea(ctx, mem);
        mem.write_u8(ea, ctx.x);
    }

    fn sty_handler(&self, ctx:&mut CpuContext, mem:&mut Memory) {
        let ea = self.get_ea(ctx, mem);
        mem.write_u8(ea, ctx.y);
    }

    fn branch_handler(&self, ctx:& mut CpuContext, condition: bool) {
        if condition {
            ctx.pc = ctx.pc.wrapping_add(self.arg);
        }
    }

    fn jmp_handler(&self, ctx:&mut CpuContext, mem:&mut Memory) {
        ctx.pc = self.get_ea(ctx, mem);
    }

    fn jsr_handler(&self, ctx:&mut CpuContext, mem:&mut Memory) {
        let pc = ctx.pc.wrapping_sub(1);
        ctx.push_u16(mem, pc);
        ctx.pc = self.arg;
    }

    fn dec_handler(&self, ctx:&mut CpuContext, mem:&mut Memory) {
        let ea = self.get_ea(ctx, mem);
        let value = mem.read_u8(ea).wrapping_sub(1);
        ctx.update_nz(value);
        mem.write_u8(ea, value);
    }

    fn inc_handler(&self, ctx:&mut CpuContext, mem:&mut Memory) {
        let ea = self.get_ea(ctx, mem);
        let value = mem.read_u8(ea).wrapping_add(1);
        ctx.update_nz(value);
        mem.write_u8(ea, value);
    }

    fn and_handler(&self, ctx:&mut CpuContext, mem:&mut Memory) {
        let value = self.get_value(ctx, mem);
        ctx.set_a(value & ctx.a);
    }

    fn asl_handler(&self, ctx:&mut CpuContext, mem:&mut Memory) {
        if self.addressing_mode == CpuInstAddressingMode::Accumulator {
            ctx.carry = (ctx.a & 0x80) == 0x80;
            ctx.set_a(ctx.a.wrapping_shl(1));
        } else {
            let ea = self.get_ea(ctx, mem);
            let mut value = mem.read_u8(ea);
            ctx.carry = (value & 0x80) == 0x80;
            value = value.wrapping_shl(1);
            ctx.update_nz(value);
            mem.write_u8(ea, value);
        }
    }

    fn rol_handler(&self, ctx:&mut CpuContext, mem:&mut Memory) {
        let carry = ctx.carry;
        if self.addressing_mode == CpuInstAddressingMode::Accumulator {
            ctx.carry = (ctx.a & 0x80) == 0x80;
            ctx.set_a(ctx.a.wrapping_shl(1) | if carry { 0x1 } else { 0 });
        } else {
            let ea = self.get_ea(ctx, mem);
            let mut value = mem.read_u8(ea);
            ctx.carry = (value & 0x80) == 0x80;
            value = value.wrapping_shl(1) | if carry { 0x1 } else { 0 };
            ctx.update_nz(value);
            mem.write_u8(ea, value);
        }
    }

    fn lsr_handler(&self, ctx:&mut CpuContext, mem:&mut Memory) {
        if self.addressing_mode == CpuInstAddressingMode::Accumulator {
            ctx.carry = (ctx.a & 1) == 1;
            ctx.set_a(ctx.a.wrapping_shr(1));
        } else {
            let ea = self.get_ea(ctx, mem);
            let mut value = mem.read_u8(ea);
            ctx.carry = (value & 1) == 1;
            value = value.wrapping_shr(1);
            ctx.update_nz(value);
            mem.write_u8(ea, value);
        }
    }

    fn ror_handler(&self, ctx:&mut CpuContext, mem:&mut Memory) {
        let carry = ctx.carry;
        if self.addressing_mode == CpuInstAddressingMode::Accumulator {
            ctx.carry = (ctx.a & 1) == 1;
            ctx.set_a(ctx.a.wrapping_shr(1) | if carry { 0x80 } else { 0 });
        } else {
            let ea = self.get_ea(ctx, mem);
            let mut value = mem.read_u8(ea);
            ctx.carry = (value & 1) == 1;
            value = value.wrapping_shr(1) | if carry { 0x80 } else { 0 };
            ctx.update_nz(value);
            mem.write_u8(ea, value);
        }
    }

    fn bit_handler(&self, ctx:&mut CpuContext, mem:&mut Memory) {
        let value = self.get_value(ctx, mem);
        ctx.zero = (ctx.a & value) == 0;
        ctx.negative = (value & 0x80) == 0x80;
        ctx.overflow = (value & 0x40) == 0x40;
    }

    fn cmp_handler(&self, ctx:&mut CpuContext, mem:&mut Memory) {
        let value = self.get_value(ctx, mem);
        CpuInst::cmp_values(ctx, ctx.a, value);
    }

    fn cpx_handler(&self, ctx:&mut CpuContext, mem:&mut Memory) {
        let value = self.get_value(ctx, mem);
        CpuInst::cmp_values(ctx, ctx.x, value);
    }

    fn cpy_handler(&self, ctx:&mut CpuContext, mem:&mut Memory) {
        let value = self.get_value(ctx, mem);
        CpuInst::cmp_values(ctx, ctx.y, value);
    }

    fn cmp_values(ctx:&mut CpuContext, left: u8, right: u8) {
        ctx.update_nz(left.wrapping_sub(right));
        ctx.carry = left >= right;
    }

    fn adc_binary_handler(&self, ctx:&mut CpuContext, value: u8) -> u8 {
        let result = ctx.a as u16 + value as u16 + ctx.get_carry() as u16;
        ctx.carry = result > 0xFF;
        if ((ctx.a ^ value) & 0x80) == 0 {
            ctx.overflow = ((ctx.a as u16) & 0x80) != (result & 0x80);
        } else {
            ctx.overflow = false;
        }

        (result & 0xFF) as u8
    }

    fn adc_decimal_handler(&self, ctx:&mut CpuContext, value: u8) -> u8 {
        let mut tmp = (ctx.a as u16 & 0xF) + (value as u16 & 0xF) + ctx.get_carry() as u16;
        if tmp >= 10 {
            tmp = 0x10 | ((tmp + 6) & 0xF);
        }
        tmp += (ctx.a as u16 & 0xF0) + (value as u16 & 0xF0);
        if tmp >= 160 {
            ctx.carry = true;
            tmp += 0x60;
        } else {
            ctx.carry = false;
        }

        (tmp & 0xFF) as u8
    }

    fn adc_handler(&self, ctx:&mut CpuContext, mem:&mut Memory) {
        let value = self.get_value(ctx, mem);        
        let result = if ctx.decimal {
            self.adc_decimal_handler(ctx, value)
        } else {
            self.adc_binary_handler(ctx, value)
        };
        ctx.set_a(result);
    }

    fn sbc_binary_handler(&self, ctx:&mut CpuContext, value: u8) -> u8 {
        let result = (0xFF_u16 + ctx.a as u16).wrapping_sub(value as u16) + ctx.get_carry() as u16;
        ctx.carry = result >= 0x100;
        if ((ctx.a ^ value) & 0x80) == 0x80 {
            ctx.overflow = ((ctx.a as u16) & 0x80) != (result & 0x80);
        } else {
            ctx.overflow = false;
        }
        (result & 0xFF) as u8
    }

    fn sbc_decimal_handler(&self, ctx:&mut CpuContext, value: u8) -> u8 {
        let mut tmp: u16 = 0xF_u16 + (ctx.a as u16 & 0xF).wrapping_sub(value as u16 & 0xF).wrapping_add(ctx.get_carry() as u16);
        let mut w: u16;
        if tmp < 0x10 {
            w = 0;
            tmp = tmp.wrapping_sub(6);
        } else {
            w = 0x10;
            tmp = tmp.wrapping_sub(0x10);
        }

        w += 0xF0_u16 + (ctx.a as u16 & 0xF0).wrapping_sub(value as u16 & 0xF0);
        if w < 0x100 {
            ctx.carry = false;
            w = w.wrapping_sub(0x60);
        } else {
            ctx.carry = true;
        }
        w += tmp;
        (w & 0xFF) as u8
    }

    fn sbc_handler(&self, ctx:&mut CpuContext, mem:&mut Memory) {
        let value = self.get_value(ctx, mem);
        let result = if ctx.decimal {
            self.sbc_decimal_handler(ctx, value)
        } else {
            self.sbc_binary_handler(ctx, value)
        };

        ctx.set_a(result);
    }

    fn brk_handler(&self, ctx:&mut CpuContext, mem:&mut Memory) {
        ctx.push_u16(mem, ctx.pc.wrapping_add(1));
        ctx.push_u8(mem, ctx.get_status(true));
        ctx.pc = mem.read_u16(0xFFFE);
        ctx.interrupt = true;
    }

    fn rti_handler(&self, ctx:&mut CpuContext, mem:&mut Memory) {
        let status = ctx.pop_u8(mem);
        ctx.set_status(status);
        ctx.pc = ctx.pop_u16(mem);
    }
}

#[cfg(test)]
mod cpu_inst_tests {
    use super::*;

    fn run_test_sbc(a: u8, imm: u16, carry: bool, res: u8, res_negative: bool, res_overflow: bool, res_zero: bool, res_carry: bool) {
        let mut ctx = CpuContext::new();
        ctx.a = a;
        ctx.carry = carry;
        let mut mem = Memory::new();
        let inst = CpuInst::new_handler(0xE9, "SBC", CpuInstAddressingMode::Immediate, 
                2, false, CpuInst::sbc_handler).clone_with_addr(0, imm);
        (inst.handler)(&inst, &mut ctx, &mut mem);
        assert_eq!(ctx.a, res);
        assert_eq!(ctx.negative, res_negative);
        assert_eq!(ctx.overflow, res_overflow);
        assert_eq!(ctx.zero, res_zero);
        assert_eq!(ctx.carry, res_carry);
    }

    #[test]
    fn test_sbc() {
        run_test_sbc(0x20, 0x10, true, 0x10, false, false, false, true);
        run_test_sbc(0x20, 0x10, false, 0xF, false, false, false, true);
        run_test_sbc(0x10, 0x20, true, 0xF0, true, false, false, false);
        run_test_sbc(0x40, 0x80, true, 0xC0, true, true, false, false);
        run_test_sbc(0x80, 0x80, true, 0, false, false, true, true);
    }
}

pub struct CpuInstReader {
    // Instruction templates.
    insts: Vec<CpuInst>,
}

impl CpuInstReader {
    pub fn new() -> CpuInstReader {
        CpuInstReader {
            insts : vec![
            CpuInst::new_handler(0x00, "BRK", CpuInstAddressingMode::Implied, 7, false, CpuInst::brk_handler),
            CpuInst::new_handler(0x01, "ORA", CpuInstAddressingMode::IndirectX, 6, false, CpuInst::ora_handler),
            CpuInst::new_handler(0x05, "ORA", CpuInstAddressingMode::Zeropage, 3, false, CpuInst::ora_handler),
            CpuInst::new_handler(0x06, "ASL", CpuInstAddressingMode::Zeropage, 5, false, CpuInst::asl_handler),
            CpuInst::new_handler(0x08, "PHP", CpuInstAddressingMode::Implied, 3, false, |_,ctx,mem|{ctx.push_u8(mem, ctx.get_status(true));}),
            CpuInst::new_handler(0x09, "ORA", CpuInstAddressingMode::Immediate, 2, false, CpuInst::ora_handler),
            CpuInst::new_handler(0x0A, "ASL", CpuInstAddressingMode::Accumulator, 2, false, CpuInst::asl_handler),
            CpuInst::new_handler(0x0D, "ORA", CpuInstAddressingMode::Absolute, 4, false, CpuInst::ora_handler),
            CpuInst::new_handler(0x0E, "ASL", CpuInstAddressingMode::Absolute, 6, false, CpuInst::asl_handler),
            CpuInst::new_handler(0x10, "BPL", CpuInstAddressingMode::Relative, 2, true, |i,ctx,_|{i.branch_handler(ctx, !ctx.negative);}),
            CpuInst::new_handler(0x11, "ORA", CpuInstAddressingMode::IndirectY, 5, true, CpuInst::ora_handler),
            CpuInst::new_handler(0x15, "ORA", CpuInstAddressingMode::ZeropageX, 4, false, CpuInst::ora_handler),
            CpuInst::new_handler(0x16, "ASL", CpuInstAddressingMode::ZeropageX, 6, false, CpuInst::asl_handler),
            CpuInst::new_handler(0x18, "CLC", CpuInstAddressingMode::Implied, 2, false, |_,ctx,_|{ctx.carry = false;}),
            CpuInst::new_handler(0x19, "ORA", CpuInstAddressingMode::AbsoluteY, 4, true, CpuInst::ora_handler),
            CpuInst::new_handler(0x1D, "ORA", CpuInstAddressingMode::AbsoluteX, 4, true, CpuInst::ora_handler),
            CpuInst::new_handler(0x1E, "ASL", CpuInstAddressingMode::AbsoluteX, 7, false, CpuInst::asl_handler),
            CpuInst::new_handler(0x20, "JSR", CpuInstAddressingMode::Absolute, 6, false, CpuInst::jsr_handler),
            CpuInst::new_handler(0x21, "AND", CpuInstAddressingMode::IndirectX, 6, false, CpuInst::and_handler),
            CpuInst::new_handler(0x24, "BIT", CpuInstAddressingMode::Zeropage, 3, false, CpuInst::bit_handler),
            CpuInst::new_handler(0x25, "AND", CpuInstAddressingMode::Zeropage, 3, false, CpuInst::and_handler),
            CpuInst::new_handler(0x26, "ROL", CpuInstAddressingMode::Zeropage, 5, false, CpuInst::rol_handler),
            CpuInst::new_handler(0x28, "PLP", CpuInstAddressingMode::Implied, 4, false, |_,ctx,mem|{let status = ctx.pop_u8(mem);ctx.set_status(status);}),
            CpuInst::new_handler(0x29, "AND", CpuInstAddressingMode::Immediate, 2, false, CpuInst::and_handler),
            CpuInst::new_handler(0x2A, "ROL", CpuInstAddressingMode::Accumulator, 2, false, CpuInst::rol_handler),
            CpuInst::new_handler(0x2C, "BIT", CpuInstAddressingMode::Absolute, 4, false, CpuInst::bit_handler),
            CpuInst::new_handler(0x2D, "AND", CpuInstAddressingMode::Absolute, 4, false, CpuInst::and_handler),
            CpuInst::new_handler(0x2E, "ROL", CpuInstAddressingMode::Absolute, 6, false, CpuInst::rol_handler),
            CpuInst::new_handler(0x30, "BMI", CpuInstAddressingMode::Relative, 2, true, |i,ctx,_|{i.branch_handler(ctx, ctx.negative);}),
            CpuInst::new_handler(0x31, "AND", CpuInstAddressingMode::IndirectY, 5, true, CpuInst::and_handler),
            CpuInst::new_handler(0x35, "AND", CpuInstAddressingMode::ZeropageX, 4, false, CpuInst::and_handler),
            CpuInst::new_handler(0x36, "ROL", CpuInstAddressingMode::ZeropageX, 6, false, CpuInst::rol_handler),
            CpuInst::new_handler(0x38, "SEC", CpuInstAddressingMode::Implied, 2, false, |_,ctx,_|{ctx.carry = true;}),
            CpuInst::new_handler(0x39, "AND", CpuInstAddressingMode::AbsoluteY, 4, true, CpuInst::and_handler),
            CpuInst::new_handler(0x3D, "AND", CpuInstAddressingMode::AbsoluteX, 4, true, CpuInst::and_handler),
            CpuInst::new_handler(0x3E, "ROL", CpuInstAddressingMode::AbsoluteX, 7, false, CpuInst::rol_handler),
            CpuInst::new_handler(0x40, "RTI", CpuInstAddressingMode::Implied, 6, false, CpuInst::rti_handler),
            CpuInst::new_handler(0x41, "EOR", CpuInstAddressingMode::IndirectX, 6, false, CpuInst::eor_handler),
            CpuInst::new_handler(0x45, "EOR", CpuInstAddressingMode::Zeropage, 3, false, CpuInst::eor_handler),
            CpuInst::new_handler(0x46, "LSR", CpuInstAddressingMode::Zeropage, 5, false, CpuInst::lsr_handler),
            CpuInst::new_handler(0x48, "PHA", CpuInstAddressingMode::Implied, 3, false, |_,ctx,mem|{ctx.push_u8(mem, ctx.a);}),
            CpuInst::new_handler(0x49, "EOR", CpuInstAddressingMode::Immediate, 2, false, CpuInst::eor_handler),
            CpuInst::new_handler(0x4A, "LSR", CpuInstAddressingMode::Accumulator, 2, false, CpuInst::lsr_handler),
            CpuInst::new_handler(0x4C, "JMP", CpuInstAddressingMode::Absolute, 3, false, CpuInst::jmp_handler),
            CpuInst::new_handler(0x4D, "EOR", CpuInstAddressingMode::Absolute, 4, false, CpuInst::eor_handler),
            CpuInst::new_handler(0x4E, "LSR", CpuInstAddressingMode::Absolute, 6, false, CpuInst::lsr_handler),
            CpuInst::new_handler(0x50, "BVC", CpuInstAddressingMode::Relative, 2, true, |i,ctx,_|{i.branch_handler(ctx, !ctx.overflow);}),
            CpuInst::new_handler(0x51, "EOR", CpuInstAddressingMode::IndirectY, 5, true, CpuInst::eor_handler),
            CpuInst::new_handler(0x55, "EOR", CpuInstAddressingMode::ZeropageX, 4, false, CpuInst::eor_handler),
            CpuInst::new_handler(0x56, "LSR", CpuInstAddressingMode::ZeropageX, 6, false, CpuInst::lsr_handler),
            CpuInst::new_handler(0x58, "CLI", CpuInstAddressingMode::Implied, 2, false, |_,ctx,_|{ctx.interrupt = false;}),
            CpuInst::new_handler(0x59, "EOR", CpuInstAddressingMode::AbsoluteY, 4, true, CpuInst::eor_handler),
            CpuInst::new_handler(0x5D, "EOR", CpuInstAddressingMode::AbsoluteX, 4, true, CpuInst::eor_handler),
            CpuInst::new_handler(0x5E, "LSR", CpuInstAddressingMode::AbsoluteX, 7, false, CpuInst::lsr_handler),
            CpuInst::new_handler(0x60, "RTS", CpuInstAddressingMode::Implied, 6, false, |_,ctx,mem|{ctx.pc = ctx.pop_u16(mem).wrapping_add(1);}),
            CpuInst::new_handler(0x61, "ADC", CpuInstAddressingMode::IndirectX, 6, false, CpuInst::adc_handler),
            CpuInst::new_handler(0x65, "ADC", CpuInstAddressingMode::Zeropage, 3, false, CpuInst::adc_handler),
            CpuInst::new_handler(0x66, "ROR", CpuInstAddressingMode::Zeropage, 5, false, CpuInst::ror_handler),
            CpuInst::new_handler(0x68, "PLA", CpuInstAddressingMode::Implied, 4, false, |_,ctx,mem|{let value = ctx.pop_u8(mem); ctx.set_a(value);}),
            CpuInst::new_handler(0x69, "ADC", CpuInstAddressingMode::Immediate, 2, false, CpuInst::adc_handler),
            CpuInst::new_handler(0x6A, "ROR", CpuInstAddressingMode::Accumulator, 2, false, CpuInst::ror_handler),
            CpuInst::new_handler(0x6C, "JMP", CpuInstAddressingMode::Indirect, 5, false, CpuInst::jmp_handler),
            CpuInst::new_handler(0x6D, "ADC", CpuInstAddressingMode::Absolute, 4, false, CpuInst::adc_handler),
            CpuInst::new_handler(0x6E, "ROR", CpuInstAddressingMode::Absolute, 6, false, CpuInst::ror_handler),
            CpuInst::new_handler(0x70, "BVS", CpuInstAddressingMode::Relative, 2, true, |i,ctx,_|{i.branch_handler(ctx, ctx.overflow);}),
            CpuInst::new_handler(0x71, "ADC", CpuInstAddressingMode::IndirectY, 5, true, CpuInst::adc_handler),
            CpuInst::new_handler(0x75, "ADC", CpuInstAddressingMode::ZeropageX, 4, false, CpuInst::adc_handler),
            CpuInst::new_handler(0x76, "ROR", CpuInstAddressingMode::ZeropageX, 6, false, CpuInst::ror_handler),
            CpuInst::new_handler(0x78, "SEI", CpuInstAddressingMode::Implied, 2, false, |_,ctx,_|{ctx.interrupt = true;}),
            CpuInst::new_handler(0x79, "ADC", CpuInstAddressingMode::AbsoluteY, 4, true, CpuInst::adc_handler),
            CpuInst::new_handler(0x7D, "ADC", CpuInstAddressingMode::AbsoluteX, 4, true, CpuInst::adc_handler),
            CpuInst::new_handler(0x7E, "ROR", CpuInstAddressingMode::AbsoluteX, 7, false, CpuInst::ror_handler),
            CpuInst::new_handler(0x81, "STA", CpuInstAddressingMode::IndirectX, 6, false, CpuInst::sta_handler),
            CpuInst::new_handler(0x84, "STY", CpuInstAddressingMode::Zeropage, 3, false, CpuInst::sty_handler),
            CpuInst::new_handler(0x85, "STA", CpuInstAddressingMode::Zeropage, 3, false, CpuInst::sta_handler),
            CpuInst::new_handler(0x86, "STX", CpuInstAddressingMode::Zeropage, 3, false, CpuInst::stx_handler),
            CpuInst::new_handler(0x88, "DEY", CpuInstAddressingMode::Implied, 2, false, |_,ctx,_|{ctx.set_y(ctx.y.wrapping_sub(1));}),
            CpuInst::new_handler(0x8A, "TXA", CpuInstAddressingMode::Implied, 2, false, |_,ctx,_|{ctx.set_a(ctx.x);}),
            CpuInst::new_handler(0x8C, "STY", CpuInstAddressingMode::Absolute, 4, false, CpuInst::sty_handler),
            CpuInst::new_handler(0x8D, "STA", CpuInstAddressingMode::Absolute, 4, false, CpuInst::sta_handler),
            CpuInst::new_handler(0x8E, "STX", CpuInstAddressingMode::Absolute, 4, false, CpuInst::stx_handler),
            CpuInst::new_handler(0x90, "BCC", CpuInstAddressingMode::Relative, 2, true, |i,ctx,_|{i.branch_handler(ctx, !ctx.carry);}),
            CpuInst::new_handler(0x91, "STA", CpuInstAddressingMode::IndirectY, 6, false, CpuInst::sta_handler),
            CpuInst::new_handler(0x94, "STY", CpuInstAddressingMode::ZeropageX, 4, false, CpuInst::sty_handler),
            CpuInst::new_handler(0x95, "STA", CpuInstAddressingMode::ZeropageX, 4, false, CpuInst::sta_handler),
            CpuInst::new_handler(0x96, "STX", CpuInstAddressingMode::ZeropageY, 4, false, CpuInst::stx_handler),
            CpuInst::new_handler(0x98, "TYA", CpuInstAddressingMode::Implied, 2, false, |_,ctx,_|{ctx.set_a(ctx.y);}),
            CpuInst::new_handler(0x99, "STA", CpuInstAddressingMode::AbsoluteY, 5, false, CpuInst::sta_handler),
            CpuInst::new_handler(0x9A, "TXS", CpuInstAddressingMode::Implied, 2, false, |_,ctx,_|{ctx.sp = ctx.x;}),
            CpuInst::new_handler(0x9D, "STA", CpuInstAddressingMode::AbsoluteX, 5, false, CpuInst::sta_handler),
            CpuInst::new_handler(0xA0, "LDY", CpuInstAddressingMode::Immediate, 2, false, CpuInst::ldy_handler),
            CpuInst::new_handler(0xA1, "LDA", CpuInstAddressingMode::IndirectX, 6, false, CpuInst::lda_handler),
            CpuInst::new_handler(0xA2, "LDX", CpuInstAddressingMode::Immediate, 2, false, CpuInst::ldx_handler),
            CpuInst::new_handler(0xA4, "LDY", CpuInstAddressingMode::Zeropage, 3, false, CpuInst::ldy_handler),
            CpuInst::new_handler(0xA5, "LDA", CpuInstAddressingMode::Zeropage, 3, false, CpuInst::lda_handler),
            CpuInst::new_handler(0xA6, "LDX", CpuInstAddressingMode::Zeropage, 3, false, CpuInst::ldx_handler),
            CpuInst::new_handler(0xA8, "TAY", CpuInstAddressingMode::Implied, 2, false, |_,ctx,_|{ctx.set_y(ctx.a);}),
            CpuInst::new_handler(0xA9, "LDA", CpuInstAddressingMode::Immediate, 2, false, CpuInst::lda_handler),
            CpuInst::new_handler(0xAA, "TAX", CpuInstAddressingMode::Implied, 2, false, |_,ctx,_|{ctx.set_x(ctx.a);}),
            CpuInst::new_handler(0xAC, "LDY", CpuInstAddressingMode::Absolute, 4, false, CpuInst::ldy_handler),
            CpuInst::new_handler(0xAD, "LDA", CpuInstAddressingMode::Absolute, 4, false, CpuInst::lda_handler),
            CpuInst::new_handler(0xAE, "LDX", CpuInstAddressingMode::Absolute, 4, false, CpuInst::ldx_handler),
            CpuInst::new_handler(0xB0, "BCS", CpuInstAddressingMode::Relative, 2, true, |i,ctx,_|{i.branch_handler(ctx, ctx.carry);}),
            CpuInst::new_handler(0xB1, "LDA", CpuInstAddressingMode::IndirectY, 5, true, CpuInst::lda_handler),
            CpuInst::new_handler(0xB4, "LDY", CpuInstAddressingMode::ZeropageX, 4, false, CpuInst::ldy_handler),
            CpuInst::new_handler(0xB5, "LDA", CpuInstAddressingMode::ZeropageX, 4, false, CpuInst::lda_handler),
            CpuInst::new_handler(0xB6, "LDX", CpuInstAddressingMode::ZeropageY, 4, false, CpuInst::ldx_handler),
            CpuInst::new_handler(0xB8, "CLV", CpuInstAddressingMode::Implied, 2, false, |_,ctx,_|{ctx.overflow = false;}),
            CpuInst::new_handler(0xB9, "LDA", CpuInstAddressingMode::AbsoluteY, 4, true, CpuInst::lda_handler),
            CpuInst::new_handler(0xBA, "TSX", CpuInstAddressingMode::Implied, 2, false, |_,ctx,_|{ctx.set_x(ctx.sp);}),
            CpuInst::new_handler(0xBC, "LDY", CpuInstAddressingMode::AbsoluteX, 4, true, CpuInst::ldy_handler),
            CpuInst::new_handler(0xBD, "LDA", CpuInstAddressingMode::AbsoluteX, 4, true, CpuInst::lda_handler),
            CpuInst::new_handler(0xBE, "LDX", CpuInstAddressingMode::AbsoluteY, 4, true, CpuInst::ldx_handler),
            CpuInst::new_handler(0xC0, "CPY", CpuInstAddressingMode::Immediate, 2, false, CpuInst::cpy_handler),
            CpuInst::new_handler(0xC1, "CMP", CpuInstAddressingMode::IndirectX, 6, false, CpuInst::cmp_handler),
            CpuInst::new_handler(0xC4, "CPY", CpuInstAddressingMode::Zeropage, 3, false, CpuInst::cpy_handler),
            CpuInst::new_handler(0xC5, "CMP", CpuInstAddressingMode::Zeropage, 3, false, CpuInst::cmp_handler),
            CpuInst::new_handler(0xC6, "DEC", CpuInstAddressingMode::Zeropage, 5, false, CpuInst::dec_handler),
            CpuInst::new_handler(0xC8, "INY", CpuInstAddressingMode::Implied, 2, false, |_,ctx,_|{ctx.set_y(ctx.y.wrapping_add(1));}),
            CpuInst::new_handler(0xC9, "CMP", CpuInstAddressingMode::Immediate, 2, false, CpuInst::cmp_handler),
            CpuInst::new_handler(0xCA, "DEX", CpuInstAddressingMode::Implied, 2, false, |_,ctx,_|{ctx.set_x(ctx.x.wrapping_sub(1));}),
            CpuInst::new_handler(0xCC, "CPY", CpuInstAddressingMode::Absolute, 4, false, CpuInst::cpy_handler),
            CpuInst::new_handler(0xCD, "CMP", CpuInstAddressingMode::Absolute, 4, false, CpuInst::cmp_handler),
            CpuInst::new_handler(0xCE, "DEC", CpuInstAddressingMode::Absolute, 6, false, CpuInst::dec_handler),
            CpuInst::new_handler(0xD0, "BNE", CpuInstAddressingMode::Relative, 2, true, |i,ctx,_|{i.branch_handler(ctx, !ctx.zero);}),
            CpuInst::new_handler(0xD1, "CMP", CpuInstAddressingMode::IndirectY, 5, true, CpuInst::cmp_handler),
            CpuInst::new_handler(0xD5, "CMP", CpuInstAddressingMode::ZeropageX, 4, false, CpuInst::cmp_handler),
            CpuInst::new_handler(0xD6, "DEC", CpuInstAddressingMode::ZeropageX, 6, false, CpuInst::dec_handler),
            CpuInst::new_handler(0xD8, "CLD", CpuInstAddressingMode::Implied, 2, false, |_,ctx,_|{ctx.decimal = false;}),
            CpuInst::new_handler(0xD9, "CMP", CpuInstAddressingMode::AbsoluteY, 4, true, CpuInst::cmp_handler),
            CpuInst::new_handler(0xDD, "CMP", CpuInstAddressingMode::AbsoluteX, 4, true, CpuInst::cmp_handler),
            CpuInst::new_handler(0xDE, "DEC", CpuInstAddressingMode::AbsoluteX, 7, false, CpuInst::dec_handler),
            CpuInst::new_handler(0xE0, "CPX", CpuInstAddressingMode::Immediate, 2, false, CpuInst::cpx_handler),
            CpuInst::new_handler(0xE1, "SBC", CpuInstAddressingMode::IndirectX, 6, false, CpuInst::sbc_handler),
            CpuInst::new_handler(0xE4, "CPX", CpuInstAddressingMode::Zeropage, 3, false, CpuInst::cpx_handler),
            CpuInst::new_handler(0xE5, "SBC", CpuInstAddressingMode::Zeropage, 3, false, CpuInst::sbc_handler),
            CpuInst::new_handler(0xE6, "INC", CpuInstAddressingMode::Zeropage, 5, false, CpuInst::inc_handler),
            CpuInst::new_handler(0xE8, "INX", CpuInstAddressingMode::Implied, 2, false, |_,ctx,_|{ctx.set_x(ctx.x.wrapping_add(1));}),
            CpuInst::new_handler(0xE9, "SBC", CpuInstAddressingMode::Immediate, 2, false, CpuInst::sbc_handler),
            CpuInst::new_handler(0xEA, "NOP", CpuInstAddressingMode::Implied, 2, false, |_,_,_|{}),
            CpuInst::new_handler(0xEC, "CPX", CpuInstAddressingMode::Absolute, 4, false, CpuInst::cpx_handler),
            CpuInst::new_handler(0xED, "SBC", CpuInstAddressingMode::Absolute, 4, false, CpuInst::sbc_handler),
            CpuInst::new_handler(0xEE, "INC", CpuInstAddressingMode::Absolute, 6, false, CpuInst::inc_handler),
            CpuInst::new_handler(0xF0, "BEQ", CpuInstAddressingMode::Relative, 2, true, |i,ctx,_|{i.branch_handler(ctx, ctx.zero);}),
            CpuInst::new_handler(0xF1, "SBC", CpuInstAddressingMode::IndirectY, 5, true, CpuInst::sbc_handler),
            CpuInst::new_handler(0xF5, "SBC", CpuInstAddressingMode::ZeropageX, 4, false, CpuInst::sbc_handler),
            CpuInst::new_handler(0xF6, "INC", CpuInstAddressingMode::ZeropageX, 6, false, CpuInst::inc_handler),
            CpuInst::new_handler(0xF8, "SED", CpuInstAddressingMode::Implied, 2, false, |_,ctx,_|{ctx.decimal = true;}),
            CpuInst::new_handler(0xF9, "SBC", CpuInstAddressingMode::AbsoluteY, 4, true, CpuInst::sbc_handler),
            CpuInst::new_handler(0xFD, "SBC", CpuInstAddressingMode::AbsoluteX, 4, true, CpuInst::sbc_handler),
            CpuInst::new_handler(0xFE, "INC", CpuInstAddressingMode::AbsoluteX, 7, false, CpuInst::inc_handler),
        ]
        }
    }

    pub fn read(&self, mem: &Memory, addr: u16) -> CpuInst {
        let opcode = mem.read_u8(addr);
        let result = self.insts.iter().find(|o| o.opcode == opcode);
        match result {
            None => {
                let name = format!("UNKNOWN_{:02X}", opcode);
                CpuInst::new(opcode, name.as_str(), CpuInstAddressingMode::Implied, 0, false)
            }
            Some(ix) => {
                let arg_addr = addr.wrapping_add(1);
                let arg = match ix.addressing_mode {
                    CpuInstAddressingMode::Absolute | CpuInstAddressingMode::AbsoluteX | CpuInstAddressingMode::AbsoluteY | CpuInstAddressingMode::Indirect => mem.read_u16(arg_addr),
                    CpuInstAddressingMode::Accumulator | CpuInstAddressingMode::Implied => 0,
                    CpuInstAddressingMode::Immediate | CpuInstAddressingMode::IndirectX | CpuInstAddressingMode::IndirectY | 
                        CpuInstAddressingMode::Zeropage | CpuInstAddressingMode::ZeropageX | CpuInstAddressingMode::ZeropageY => mem.read_u8(arg_addr) as u16,
                    CpuInstAddressingMode::Relative => (mem.read_u8(arg_addr) as i8) as u16,
                };

                ix.clone_with_addr(addr, arg)
            }
        }
    }
}
