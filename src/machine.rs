use std::{cell::RefCell, collections::VecDeque, io::{self, Write}, rc::Rc, sync::mpsc::{self, Receiver, TryRecvError}, thread};

use crate::{cpu::{CpuContext, CpuInstReader}, debug_shell::DebugShell, mem::{Memory, MemoryRangeHandler}};

pub trait MachineHandler {
    fn reset(&mut self);
    fn run(&mut self);
}

struct IoMemoryRangeHandler {
    input_buf: Rc<RefCell<VecDeque<u8>>>,
}

impl MemoryRangeHandler for IoMemoryRangeHandler {
    fn read(&self, ofs: usize) -> u8 {
        let mut input_buf = self.input_buf.borrow_mut();
        let has_data = !input_buf.is_empty();
        match ofs {
            0x10 => if has_data { 
                let c = input_buf.pop_front().unwrap();
                //println!("Reading in key press {}.", c);
                c | 0x80
            } else { 
                0 
            },
            0x11 => if has_data { 0x80 } else { 0 },
            0x12 => 0,
            0x13 => 0,
            _ => 0,
        }
    }
    fn write(&mut self, ofs: usize, value: u8) {
        //eprintln!("Writing {0:X} to {1:X}", value, ofs);
        match ofs {
            0x10 => {},
            0x11 => {},
            0x12 => {
                let mut c = (value & 0x7F) as char;
                if c == '\r' {
                    c = '\n';
                }
                print!("{}", c);
                std::io::stdout().flush().unwrap();
            },
            0x13 => {},
            _ => {}
        };
    }
}

impl IoMemoryRangeHandler {
    fn new() -> IoMemoryRangeHandler {
        IoMemoryRangeHandler {
            input_buf: Rc::new(RefCell::new(VecDeque::new())),
        }
    }
}

pub struct Machine {
    ctx: CpuContext,
    mem: Memory,
    reader: CpuInstReader,
    input_buf: Rc<RefCell<VecDeque<u8>>>,
    receiver: Receiver<String>,
    shell: DebugShell
}

impl Machine {
    fn create_stdin_reader() -> Receiver<String> {
        let (tx, rx) = mpsc::channel::<String>();
        thread::spawn(move || loop {
            let mut line = String::new();
            io::stdin().read_line(&mut line).unwrap();
            tx.send(line).unwrap();
        });
        rx
    }

    fn add_line(&self, line: String) {
        let mut input_buf = self.input_buf.borrow_mut();
        for b in line.to_ascii_uppercase().trim().bytes() {
            input_buf.push_back(b);
        }
        input_buf.push_back('\r' as u8);
    }

    fn run_shell(&mut self) {
        loop {
            self.shell.print_prompt();
            match self.receiver.recv() {
                Ok(line) => {
                    if self.shell.process_line(line, &mut self.ctx, &mut self.mem) {
                        break;
                    }
                },
                Err(_) => panic!("Stdin reader was disconnected."),
            }
        }
    }

    pub fn new() -> Machine {
        let mut mem: Memory = Memory::new();
        let io_memory = IoMemoryRangeHandler::new();
        let input_buf = io_memory.input_buf.clone();
        mem.add_range::<0x1000>(0);
        mem.add_range_handler(0xD000, 0x100, Box::new(io_memory), false);
        mem.add_range_file(String::from("./roms/a1basic.bin"), 0xE000, true);
        mem.add_range_file(String::from("./roms/wozmon.bin"), 0xFF00, true);

        Machine {
            ctx: CpuContext::new(),
            mem,
            reader: CpuInstReader::new(),
            input_buf,
            receiver: Machine::create_stdin_reader(),
            shell: DebugShell::new(),
        }
    }
}

impl MachineHandler for Machine {
    fn reset(&mut self) {
        self.ctx.reset(&self.mem);
    }

    fn run(&mut self) {
        let mut count = 0;
        while !self.shell.quit {
            let inst = self.ctx.get_next(&self.mem, &self.reader);
            (inst.handler)(&inst, &mut self.ctx, &mut self.mem);
            if self.shell.print || self.shell.step_count > 0 {
                eprintln!("{}", inst.format());
                self.ctx.dump();
                eprintln!("");
            }
            if self.shell.step_count > 0 {
                self.shell.step_count -= 1;
                if self.shell.step_count == 0 {
                    self.run_shell();
                    continue;
                }
            }
            count += 1;
            if count == 100 {
                match self.receiver.try_recv() {
                    Ok(value) => {
                        if value.starts_with("@") {
                            self.run_shell();
                        } else {
                            self.add_line(value);
                        }
                    },
                    Err(TryRecvError::Empty) => {},
                    Err(TryRecvError::Disconnected) => panic!("Stdin reader was disconnected."),
                }
                count = 0;
            }
        }
    }
}