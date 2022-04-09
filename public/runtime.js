function run(){
    const input = document.getElementById("inputbox").value
    const output = document.getElementById("outputbox")
    console.log("test works");
    (async () => {
        const response = await fetch('main.wasm');
        const buffer = await response.arrayBuffer();
        const module = new WebAssembly.Module(buffer);
        const instance = new WebAssembly.Instance(module);
        const result = instance.exports.main(parseBigInt(input));
        console.log(result);
        output.value = result
      })();
}

// const main_wasm = fs.readFileSync("../main.wasm")

// console.log(main_wasm)

// const main = await WebAssembly.instantiate(new Uint8Array(math_wasm))
//                 .then(res => res.instance.exports)

// console.log(main)

