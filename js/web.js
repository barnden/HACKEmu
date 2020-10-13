var ROM_html = document.getElementById("ROM-view");
var RAM_html = document.getElementById("RAM-view");

var rom_active = undefined;
var ram_active = undefined;
var active_chg = false;

function set_active(hash) {
    // Validate hash before using.
    hash = hash || window.location.hash.substr(1);

    if (!hash.length) return;
    let type = hash.substr(0, 3);

    if (!["RAM", "ROM"].includes(type)) return;

    let num = parseInt(hash.substr(4));
    if (isNaN(num)) return;

    let scr = Math.max(0, (num - 12) * 22);

    if (type == "ROM") {
        ROM_html.scrollTop = scr;
        rom_active = hash;
        active_chg = true;
    } else {
        RAM_html.scrollTop = scr;
        ram_active = hash;
        active_chg = true;
    }
}

function active_loop() {
    if (active_chg) {
        virtual_scroll(ROM_html, "ROM", ROM_TOTAL);
        virtual_scroll(RAM_html, "RAM", RAM_TOTAL);

        updateRegisters(A, D, OPC);
        updateALU(alu_data[0], alu_data[1], alu_data[2], alu_data[3]);

        active_chg = false;
    }

    window.requestAnimationFrame(active_loop);
}

window.addEventListener('hashchange', () => {
    set_active();
});

/**
 * RAM has 24577 addresses, it is impractical to add that many elements to the DOM.
 * Virtual scrolling is a good solution to this issue.
 */
const ROW_HEIGHT = 22;
const MAX_VISIBLE = 25 * 22;
const RAM_TOTAL = 24577;
const ROM_TOTAL = 32768;

function int_str(int, override) {
    let str;

    if (override || MODE == 1) {
        str = from_twos(int);
    } else {
        str = int.toString(RADIX);
        str = str.padStart(DIGITS, '0');
    }

    return str;
}

function get_ROM(i) {
    if (!MODE)
        return ROM.asm[i] ? ROM.asm[i] : 0;

    return int_str(ROM.dec[i] ? ROM.dec[i] : 0);
}

function get_RAM(i) {
    return int_str(RAM.hasOwnProperty(i) ? RAM[i] : 0, !MODE);
}

function virtual_scroll(element, name, total) {
    // Adapted from Adam Klein's VirtualScroll
    let start = Math.max(0, Math.floor(element.scrollTop / ROW_HEIGHT));
    let visible = Math.min(27, total - start);
    let offset = start * ROW_HEIGHT;

    let getter = name == "ROM" ? get_ROM : get_RAM;
    let rows = [];

    for (let i = 0; i < visible; i++) {
        let j = i + start;
        let special = "";
        let hash = name + '-' + j;

        if (name == "RAM") {
            if (j == 24576)
                special = "class=\"kbd\"";
            else if (j >= 16384)
                special = "class=\"scr\"";
        }

        if (ram_active == hash || rom_active == hash)
            special = "class=\"active\"";

        rows.push(
            `<div id="${hash}" ${special}><span>${j}</span>${getter(j)}</div>`
        );
    }

    element.innerHTML = `<div style="height: ${total * ROW_HEIGHT}px; overflow: auto;"><div style="transform: translateY(${offset}px);">${rows.join("")}</div></div>`;
}

ROM_html.addEventListener("scroll", () => {
    virtual_scroll(ROM_html, "ROM", ROM_TOTAL);
});

RAM_html.addEventListener("scroll", () => {
    virtual_scroll(RAM_html, "RAM", RAM_TOTAL);
});

let freq = document.getElementById("freq");
let ipc = document.getElementById("ipc");
let slider = document.getElementById("slider");
let slider2 = document.getElementById("slider2");

slider.addEventListener("change", () => {
    let value = parseInt(slider.value);

    if (value < 1) slider.value = 1;
    else if (value > 500) slider.value = 500;

    freq.value = value;

    FREQ = parseInt(freq.value);
});

freq.addEventListener("change", () => {
    let value = parseInt(freq.value);

    if (value < 1) freq.value = 1;
    else if (value > 500) freq.value = 500;

    slider.value = value;

    FREQ = parseInt(freq.value);
});

ipc.addEventListener("change", () => {
    let value = parseInt(ipc.value);

    if (value < 1) ipc.value = 1;
    else if (value > 2000) ipc.value = 2000;

    slider2.value = value;

    IPC = value;
})

slider2.addEventListener("change", () => {
    let value = parseInt(slider2.value);

    if (value < 1) slider2.value = 1;
    else if (value > 2000) slider2.value = 2000;

    ipc.value = value;

    IPC = parseInt(ipc.value);
});

let mode = document.getElementById("mode");

mode.addEventListener("change", () => {
    set_mode(mode.selectedOptions[0].value);

    virtual_scroll(ROM_html, "ROM", ROM_TOTAL);
    virtual_scroll(RAM_html, "RAM", RAM_TOTAL);
})

window.addEventListener("load", () => {
    virtual_scroll(ROM_html, "ROM", ROM_TOTAL);
    virtual_scroll(RAM_html, "RAM", RAM_TOTAL);

    set_mode(mode.selectedOptions[0].value);

    updateRegisters(0, 0, 0);
    updateALU(0, 0, 0, "");

    FREQ = parseInt(freq.value);
    IPC = parseInt(ipc.value);

    set_active("ROM-0");
    set_active("RAM-0");

    active_loop();
});

document.addEventListener("keydown", (e) => {
    if (e.key.length != 1) return; // Do not listen for control keys
    registers[3].value = e.key;

    // The CPUEmulator for nand2tetris only transmits the capital letter.
    let key = COMPATIBILITY ? e.key.toUpperCase().charCodeAt(0) : e.key.charCodeAt(0);
    write_RAM(24576, key);
});

document.addEventListener("keyup", (e) => {
    registers[3].value = "";
    write_RAM(24576, 0);
});