use std::{io::Write, num::ParseIntError};

use crate::{cpu::CpuContext, disasm::{self, disasm_range}, mem::Memory};

fn parse_hex(s: &str) -> Result<usize, ParseIntError> {
    let mut hex = s;
    if s.to_ascii_uppercase().starts_with("0X") {
        hex = &s[2..];
    }
    usize::from_str_radix(hex, 16)
}

struct AddressRange {
    addr: usize,
    size: usize,
    is_count: bool,
}

fn parse_range(s: &str, c: bool) -> Option<AddressRange> {
    let spl = if c { "@" } else { "." };
    let parts : Vec<&str> = s.split(spl).collect();
    if parts.len() != 2 {
        return None;
    }

    let left = parse_hex(parts[0]);
    let right= parse_hex(parts[1]);
    if left.is_err() || right.is_err() {
        return None;
    }
    Some(AddressRange { 
        addr: left.unwrap(),
        size: right.unwrap(),
        is_count: c,
    })
}

fn parse_range_or_count(range: &str) -> Option<AddressRange> {
    if range.contains(".") {
        parse_range(range, false)
    } else if range.contains("@") {
        parse_range(range, true)
    }
    else {
        let addr = parse_hex(range);
        if addr.is_err() {
            None
        } else {
            Some(AddressRange { 
                addr: addr.unwrap(),
                size: 1,
                is_count: true,
            })
        }
    }
}

fn print_hex_line(mem: &Memory, curr_addr: u16, count: u16) {
    let mut index: u16 = 0;
    print!("{:04X}:", curr_addr);
    while index < 0x10 {
        if index < count {
            print!(" {:02X}", mem.read_u8(curr_addr + index));
        } else {
            print!("   ");
        }
        index += 1;
    }
    print!(" - ");
    index = 0;
    while index < 0x10 {
        if index < count {
            let c= mem.read_u8(curr_addr + index);
            if c > 32 && c < 127 {
                print!("{}", c as char);
            } else {
                print!(".");
            }
        } else {
            print!(" ");
        }
        index += 1;
    }
    println!("");
}

fn print_hex_line_words(mem: &Memory, curr_addr: u16, count: u16) {
    let mut index: u16 = 0;
    print!("{:04X}:", curr_addr);
    while index < 8 {
        if index < count {
            print!(" {:04X}", mem.read_u16(curr_addr + index * 2));
        } else {
            print!("   ");
        }
        index += 1;
    }
    println!("");
}

fn dump_hex(mem: &Memory, range: AddressRange) {
    if range.is_count {
        let mut curr_addr: u16 = range.addr as u16;
        let mut index: u16 = 0;
        let count = range.size as u16;
        while index < count {
            let remaining = count.min(0x10);
            print_hex_line(mem, curr_addr, remaining);
            curr_addr += remaining;
            index += remaining;
        }
    } else {
        let mut curr_addr = range.addr;
        let end_addr = range.size + 1;
        while curr_addr < end_addr {
            let remaining = (end_addr - curr_addr).min(0x10);
            print_hex_line(mem, curr_addr as u16, remaining as u16);
            curr_addr += remaining;
        }
    }
}

fn dump_hex_words(mem: &Memory, range: AddressRange) {
    if range.is_count {
        let mut curr_addr: u16 = range.addr as u16;
        let mut index: u16 = 0;
        let count = range.size as u16;
        while index < count {
            let remaining = count.min(8);
            print_hex_line_words(mem, curr_addr, remaining);
            curr_addr += remaining;
            index += remaining;
        }
    } else {
        let mut curr_addr = range.addr;
        let end_addr = range.size + 1;
        while curr_addr < end_addr {
            let remaining = (end_addr - curr_addr).min(0x10);
            print_hex_line_words(mem, curr_addr as u16, (remaining / 2) as u16);
            curr_addr += remaining;
        }
    }
}

pub struct DebugShell {
    pub quit: bool,
    pub print: bool,
    pub step_count: usize,
}

impl DebugShell {
    pub fn new() -> DebugShell {
        DebugShell{
            quit: false,
            print: false,
            step_count: 0,
        }
    }

    pub fn print_prompt(&self) {
        print!("DBG> ");
        std::io::stdout().flush().unwrap();
    }

    pub fn process_line(&mut self, line: String, ctx: &mut CpuContext, mem: &mut Memory) -> bool {
        let parts : Vec<&str> = line.split_whitespace().collect();
        if parts.len() < 1 {
            return false;
        }

        match parts[0] {
            "r" => {
                ctx.reset(mem);
                true
            },
            "x" => {
                ctx.dump();
                false
            },
            "u" => {
                if parts.len() == 1 {
                    disasm::disasm(mem, ctx.pc as usize, 1);
                } else {
                    match parse_range_or_count(parts[1]) {
                        None => println!("Invalid range or address."),
                        Some(r) => {
                            if r.is_count {
                                disasm::disasm(mem, r.addr, r.size);
                            } else {
                                disasm_range(mem, r.addr, r.size);
                            }
                        }
                    }
                }
                false
            },
            "p" => {
                self.print = !self.print;
                if self.print {
                    println!("Printing current state ENABLED.");
                } else {
                    println!("Printing current state DISABLED.");
                }
                false
            }
            "d" => {
                if parts.len() > 1 {
                    match parse_range_or_count(parts[1]) {
                        None => println!("Invalid range or address."),
                        Some(r) => {
                            dump_hex(mem, r);
                        }
                    }
                }
                false
            }
            "w" => {
                if parts.len() > 1 {
                    match parse_range_or_count(parts[1]) {
                        None => println!("Invalid range or address."),
                        Some(r) => {
                            dump_hex_words(mem, r);
                        }
                    }
                }
                false  
            }
            "e" => {
                if parts.len() > 2 {
                    match parse_hex(parts[1]) {
                        Ok(addr) => {
                            let mut index = 2;
                            while index < parts.len() {
                                match parse_hex(parts[index]) {
                                    Ok(v) => {
                                        mem.write_u8((addr + index - 2) as u16, v as u8);
                                    }
                                    Err(_) => {
                                        println!("Invalid hex digit.");
                                        break;
                                    }
                                }
                                index += 1;
                            }
                        },
                        Err(_) => {
                            println!("Invalid address.");
                        }
                    }
                }
                false
            }
            "s" => {
                if parts.len() > 1 {
                    match parse_hex(parts[1]) {
                        Ok(v) => {
                            self.step_count = v;
                            true
                        }
                        Err(_) => {
                            println!("Invalid step count.");
                            false
                        }
                    }
                } else {
                    self.step_count = 1;
                    true
                }
            }
            "c" => {
                true
            }
            "quit" => {
                self.quit = true;
                true
            }
            "?" => {
                println!("r               - Reset machine.");
                println!("x               - Examine register state.");
                println!("u [addr|range]  - Disassemble.");
                println!("d [addr|range]  - Display memory (bytes).");
                println!("w [addr|range]  - Display memory (words).");
                println!("e addr b0 b1... - Write memory (bytes).");
                println!("p               - Toggle printing of current state.");
                println!("s [count]       - Step one or more instructions.");
                println!("c               - Continue execution.");
                println!("quit            - Quit process.");
                false
            }
            _ => { 
                println!("Unknown command {}", parts[0]);
                false
            }
        }
    }
}