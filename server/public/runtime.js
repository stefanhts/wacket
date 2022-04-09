function run(){
    const input = document.getElementById("inputbox").value
    const output = document.getElementById("res")
    console.log("test works");
    (async () => {
        const response = await fetch('main.wasm');
        const buffer = await response.arrayBuffer();
        const module = new WebAssembly.Module(buffer);
        const instance = new WebAssembly.Instance(module);
        const result = instance.exports.main(BigInt(input));
        console.log(result);
        output.innerHTML = result
      })();
}