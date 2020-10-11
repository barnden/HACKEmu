var canvas = document.getElementById("screen");
var ctx = canvas.getContext("2d");

// Set screen to BG color
ctx.beginPath();
ctx.rect(0, 0, 512, 256);
ctx.fillStyle = '#' + BG.toString(16);
ctx.fill();
ctx.beginPath();

// Load pixel array
var img = ctx.getImageData(0, 0, 512, 256);
var pixels = img.data;
// If screen_update, then update pixel array in canvas
var screen_update = false;

function draw_pixel(x, y, c) {
    // Pixel array is a flattened 4D array
    let index = (y * img.width + x) * 4;
    let color = c ? FG : BG;

    pixels[index] = color >> 16; // R
    pixels[index + 1] = (color >> 8) & 0xFF; // G
    pixels[index + 2] = color & 0xFF // B
    pixels[index + 3] = 0xFF; // A

    screen_update = true;
}

function render() {
    // Render loop is tied to refresh rate of monitor
    window.requestAnimationFrame(render);

    if (screen_update) {
        ctx.putImageData(img, 0, 0);
        screen_update = false;
    }
}

render();

// TODO: Make this stuff nicer
var registers = [
    document.getElementById("tar"),
    document.getElementById("trd"),
    document.getElementById("tpc"),
    document.getElementById("tkb"),
];
let input = 0;

document.addEventListener("keydown", (e) => {
    //if (e.key.length != 1) return; // Do not listen for control keys
    input = e.key;
    registers[3].value = e.key;
});

document.addEventListener("keyup", (e) => {
    input = 0;
    registers[3].value = "";
});

var alu_io = [
    document.getElementById("tdin"),
    document.getElementById("tmain"),
    document.getElementById("tout"),
    document.getElementById("tmux")
]

let alu_data = [0,0,0,0];

function updateALU(d, ma, out, mux) {
    alu_io[0].value = from_twos(d);
    alu_io[1].value = from_twos(ma);
    alu_io[2].value = from_twos(out);
    alu_io[3].value = from_twos(mux);
}

function updateRegisters(a, d, pc) {
    registers[0].value = from_twos(a);
    registers[1].value = from_twos(d);
    registers[2].value = from_twos(pc);
}