use std::env;

use machine::MachineHandler;

mod cpu;
mod disasm;
mod mem;
mod machine;
mod verify_machine;
mod debug_shell;

fn main() {
    let mut machine: Box<dyn MachineHandler> = if env::args().len() > 1 {
        Box::new(verify_machine::VerifyMachine::new())
    } else {
        Box::new(machine::Machine::new())
    };
    machine.reset();
    machine.run();
}
