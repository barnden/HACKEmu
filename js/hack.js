var ROM = {
    asm: [],
    dec: []
}

RAM = { }

const MODES = ["asm", "dec", "hex", "oct", "bin"];

var FREQ = 10;
var IPC = 1;

var RADIX = 10;
var DIGITS = 5;
var MODE;

function set_mode(mode) {
    mode = mode.toLowerCase();

    if (MODES.includes(mode)) {
        MODE = MODES.indexOf(mode);

        switch (MODE) {
            case 2:
                RADIX = 16;
                break;
            case 3:
                RADIX = 8;
                break;
            case 4:
                RADIX = 2;
                break;
            default:
                RADIX = 10;
        }

        DIGITS = Math.ceil(Math.log(65536) / Math.log(RADIX));
    }
}

set_mode("asm");

var PC = 0;
// TODO: Fix updateRegisters to update before execution, not during
// Using OPC is a workaround for now.
var OPC = 0;
var A = 0;
var D = 0;
let op = "";

function sub(a, b) {
    return into_twos(from_twos(a) - from_twos(b));
}

function add(a, b) {
    return into_twos(from_twos(a) + from_twos(b));
}

function neg(a) {
    return into_twos(-from_twos(a));
}

function into_twos(a) {
    if (a < 0) {
        a *= -1;
        a ^= 0xFFFF;
        a++;
    }

    return a & 0xFFFF;
}

function from_twos(a) {
    // Get the decimal representation of 2's complement
    if ((a >> 15) == 1) {
        a ^= 0xFFFF;
        a++;
        a *= -1;
    }

    return a;
}

function write_RAM(addr, val) {
    // Use wrapper function to set screen_update <- true if we modify a memory address corresponding to a pixel.
    if (addr != 24576) {
        set_active("RAM-" + addr);

        if (addr >= 16384) {
            let temp = (addr - 16384) * 16;
            let x = temp % 512;
            let y = Math.floor(temp / 512);

            for (let i = 0; i < 16; i++)
                draw_pixel(x++, y, (val >> i) & 1)
        }
    } else active_chg = true;

    RAM[addr] = val;
}

function execute() {
    set_active("ROM-" + PC);
    OPC = PC;
    let inst = ROM.dec[PC++]

    if (inst & 0x8000) {
        inst &= 0x1FFF;

        a = (inst & 0x1000) >>> 12;
        comp = (inst & 0xFC0) >>> 6;
        dest = (inst & 0x38) >>> 3;
        jump = inst & 0x7;

        let out = undefined;

        let AM = a ? RAM[A] || 0 : A;
        let AMs = a ? "M" : "A";

        // FIXME: Make updateALU wait until next instruction to update, like the OPC workaround.
        // * Currently the ALU updates one cycle too early
        alu_data[0] = D;
        alu_data[1] = AM;

        switch (comp) {
            case 0b101010:
                out = 0;
                op = 0;
                break;
            case 0b111111:
                out = 1;
                break;
            case 0b111010:
                out = into_twos(-1);
                op = -1;
                break;
            case 0b001100:
                out = into_twos(D);
                op = "D";
                break;
            case 0b110000:
                out = into_twos(AM);
                op = AMs;
                break;
            case 0b001101:
                out = into_twos(~D);
                op = "~D";
                break;
            case 0b110001:
                out = into_twos(~AM);
                op = "~" + AMs;
                break;
            case 0b001111:
                out = neg(D);
                op = "-D";
                break;
            case 0b110011:
                out = neg(AM);
                op = "-" + AMs;
                break;
            case 0b011111:
                out = add(D, 1);
                op = "D + 1";
                break;
            case 0b110111:
                out = add(AM, 1);
                op = a ? "M" : "A" + " + 1";
                break;
            case 0b001110:
                out = sub(D, 1);
                op = "D - 1";
                break;
            case 0b110010:
                out = sub(AM, 1);
                op = a ? "M" : "A" + " - 1";
                break;
            case 0b000010:
                out = add(D, AM);
                op = "D + " + AMs;
                break;
            case 0b010011:
                out = sub(D, AM);
                op = "D - " + AMs;
                break;
            case 0b000111:
                out = sub(AM, D);
                op = AMs + " - D";
                break;
            case 0b000000:
                out = into_twos(D & AM);
                op = "D & " + AMs;
                break;
            case 0b010101:
                out = into_twos(D | AM);
                op = "D | " + AMs;
                break;
            default:
                console.error("[CPU] Unknown comp sequence: " + comp.toString(2))
                return;
        }

        let out2 = from_twos(out);

        switch (dest) {
            case 0: // Do nothing.
                break;
            case 1:
                write_RAM(A, out);
                break;
            case 2:
                D = out;
                break;
            case 3:
                write_RAM(A, out);
                D = out;
                break;
            case 4:
                A = out;
                break;
            case 5:
                write_RAM(A, out);
                A = out;
                break;
            case 6:
                A = out;
                D = out;
                break;
            case 7:
                write_RAM(A, out);
                A = out;
                D = out;
                break;
            default:
                console.error("[CPU] Unknown destination: " + dest.toString(2));
                return;
        }

        let A2 = from_twos(A);

        switch (jump) {
            case 0:
                break;
            case 1:
                if (out2 > 0) PC = A2;
                break;
            case 2:
                if (out2 == 0) PC = A2;
                break;
            case 3:
                if (out2 >= 0) PC = A2;
                break;
            case 4:
                if (out2 < 0) PC = A2;
                break;
            case 5:
                if (out2 != 0) PC = A2;
                break;
            case 6:
                if (out2 <= 0) PC = A2;
                break;
            case 7:
                PC = A2;
                break;
            default:
                console.error("[CPU] Unknown jump instruction: " + jump.toString(2));
                return;
        }

        alu_data[2] = out;
        alu_data[3] = op;
    } else {
        // A-instructions
        A = inst;
    }
}

var RUNNING = false;

function reset() {
    RUNNING = false;

    A = 0;
    D = 0;
    PC = 0;
    OPC = 0;

    set_active("ROM-0");
    set_active("RAM-0");

    updateRegisters(A, D, PC);

    RAM = { }

    for (let x = 0; x < 512; x++)
        for (let y = 0; y < 256; y++)
            draw_pixel(x, y, 0);
}


function run() {
    RUNNING = true;

    loop();
}

function loop() {
    if (RUNNING) {
        for (let i = 0; i < IPC; i++)
            execute();

        if (PC > 32767) RUNNING = false;
        setTimeout(loop, Math.max(1, Math.ceil(1 / FREQ * 1000)));
    }
}