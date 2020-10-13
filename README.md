# HACKEmu
HACKEmu is similar to the CPUEmulator program provided by the nand2tetris project, except it is capable of executing instructions faster.

You can import HACK assembly and run it using the built in assembler. The ability to import existing binaries does not exist yet.

The maximum CPU clock speed has been capped at approximately 500 Hz, as JavaScript only has a clock resolution on the order of milliseconds, and `setTimeout` being prone to clock drift. To allow for even higher execution speed, there is a slider for IPC (instructions per clock). IPC is the number of instructions executed per clock cycle, and has been capped at 2000 IPC. Higher IPC may negatively affect the CPU frequency.