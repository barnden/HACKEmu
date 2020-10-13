function assemble() {
    let machine_code = [];
    // Here we parse/assemble the assembly code provided.
    let text = document.getElementById("asm-inp").value;
    // Keep an object of lines with their corresponding line number in the user's asm file.
    let lines = {};
    let labels = { SCREEN: 16384, KBD: 24576 };

    let asm_lines = text.split('\n');

    for (let i in asm_lines) {
        let line = asm_lines[i];

        // Remove all whitespace and comments from line
        line = line.replace(/\s+|\/\/.*/g, '');

        // If not empty, then add line to the object
        if (line != '') lines[i] = line;
    }

    /** 
    Original code before I decided to do error messages.
    // Get rid of all horizontal whitespace, and comments.
    text = text.replace(/[^\S\n]+|\/\/.* /g, '');
    // Split text into lines, remove any
    let line_list = text.split('\n');
    // Remove any empty lines from the input
    line_list = line_list.filter( (e) => { return e != ''; } );
     */

    // First pass to find the labels names and assign addresses to them.
    let counter = 0;

    for (let i in lines) {
        let line = lines[i];
        if (line[0] == '(') {
            if (line[line.length - 1] != ')') {
                console.error(`Expected ")" on line ${i+1}.`);
                return;
            }

            labels[line.substr(1, line.length - 2)] = counter;

            delete lines[i];
        } else {
            counter++;
        }
    }

    // Second pass to find variables and assign addresses to them
    counter = 0
    for (let i in lines) {
        let line = lines[i];
        if (line[0] == '@') {
            let label = line.substr(1);

            if (label[0] == 'R' && is_int(label.substr(1))) {
                label = label.substr(1);
                lines[i] = '@' + label;
            }

            if (!is_int(label)){
                if (!labels.hasOwnProperty(label)) {
                    // Variable addressing starts at 16, then increments for each new variable.
                    let addr = 16 + counter;
                    let vals = Object.values(labels);
                    let used = vals.includes(addr);

                    if (!COMPATIBILITY) {
                        // For the edge cases where the label happens to occur at the same intance as a variable.
                        // This functionality differs from the assembler implemented by nand2tetris.
                        while (used) {
                            used = false;
                            for (let laddr of vals) {
                                if (laddr == addr) {
                                    used = true;
                                    addr++;
                                    break;
                                }
                            }
                        }
                    }

                    labels[label] = addr;
                    counter++;
                }

                lines[i] = '@' + labels[label];
            }
        }
    }

    for (let i in lines) {
        let line = lines[i];
        let token = line[0];
        let inst = null;

        switch (token) {
            case '@':
                inst = asm_a_instruction(line, i);
                if (inst == null) return;
                machine_code.push(inst);
                break;
            case 'M':
            case 'A':
            case 'D':
                inst = asm_c_instruction(line, i);
                if (inst == null) return;

                machine_code.push(inst);

                break;
            case '0':
            case '1':
                // Get all numeric characters before semicolon.
                let str = line.match(/-?\d+;?/);

                if (str != null) {
                    str = str[0];
                    str = str.replace(';', '');

                    if (str.length == 1) {
                        inst = asm_c_instruction(line, i);
                        if (inst == null) return;

                        machine_code.push(inst);

                        break;
                    }

                    token = str;
                }
            case '-':
            case '!':
                token = line.split(';')[0];

                if (["-1", "-A", "-D", "-M", "!D", "!A", "!M"].includes(token)) {
                    inst = asm_c_instruction(line, i);
                    if (inst == null) return;

                    machine_code.push(inst);

                    break;
                }
            default:
                console.error(`[ASSEMBLER] Invalid token "${token}" in line ${parseInt(i) + 1}.`);
                return; // Cancel assembling if error
        }
    }

    ROM.asm = [];
    for (let i in lines) ROM.asm.push(lines[i]);

    ROM.dec = machine_code;
    reset();
}

function asm_a_instruction(inst, line) {
    // Remove leading @ symbol
    inst = parseInt(inst.substr(1));

    if (inst > 32767 || inst < 0) {
        console.error(`[ASSEMBLER] Address "${inst}" given for A instruction is out of bounds, in line ${parseInt(line) + 1}.`);
        return null;
    }

    return inst;
}

// Lookup tables for the instructions
jumps = [ "", "JGT", "JEQ", "JGE", "JLT", "JNE", "JLE", "JMP" ];
dests = [ "", "M", "D", "MD", "A", "AM", "AD", "AMD" ];
comps = {
    "0":   0b0101010, "1":   0b0111111, "-1":  0b0111010,
    "D":   0b0001100, "A":   0b0110000, "M":   0b1110000,
    "!D":  0b0001101, "!A":  0b0110001, "!M":  0b1110001,
    "-D":  0b0001111, "-A":  0b0110011, "-M":  0b1110011,
    "D+1": 0b0011111, "A+1": 0b0110111, "M+1": 0b1110111,
    "D-1": 0b0001110, "A-1": 0b0110010, "M-1": 0b1110010,
    "D+A": 0b0000010, "D-A": 0b0010011, "A-D": 0b0000111,
    "D+M": 0b1000010, "D-M": 0b1010011, "M-D": 0b1000111,
    "D&A": 0b0000000, "D&M": 0b1000000,
    "D|A": 0b0010101, "D|M": 0b1010101
}

comps["A+D"] = comps["D+A"];
comps["M+D"] = comps["D+M"];
comps["A&D"] = comps["D&A"];
comps["D&M"] = comps["M&D"];
comps["A|D"] = comps["D|A"];
comps["D|M"] = comps["M|D"];

function asm_c_instruction(inst, line) {
    let dest;
    let comp;
    let jump;

    dest = inst.substr(0, inst.indexOf('='));
    inst = inst.substr(inst.indexOf('=') + 1);

    let semi = inst.split(';');
    comp = semi[0];
    jump = semi[1] || "";

    let d = dests.indexOf(dest);
    if (d == -1) {
        console.error(`[ASSEMBLER] Unknown destination: "${dest}".`);
        return null;
    }

    let c = comps[comp];
    if (c == undefined) {
        console.error(`[ASSEMBLER] Unknown comparison: "${comp}".`);
        return null;
    }

    let j = jumps.indexOf(jump);
    if (d == -1) {
        console.error(`[ASSEMBLER] Unknown jump: "${jump}".`);
        return null;
    }

    let binary = 0;
    binary = 0b111 << 13;

    binary |= c << 6;
    binary |= d << 3;
    binary |= j;

    return binary;
}