/*
  Bit layout of values

  Values are either:
  - Immediates: end in #b000
  - Pointers

*/
const imm_shift = 3
const ptr_type_mask = ((1 << imm_shift) -1)
const box_type_tag = 1
const cons_type_tag = 2
const vect_type_tag = 3
const str_type_tag = 4
const proc_type_tag = 5
const int_shift = (1 + imm_shift)
const int_type_mask = ((1 << int_shift) - 1)
const int_type_tag = (0 << (int_shift - 1))
const nonint_type_tag = (1 << (int_shift -1))
const char_shift = (int_shift + 1)
const char_type_mask = ((1 << char_shift) - 1)
const char_type_tag = ((0 << (char_shift -1 )) | nonint_type_tag)
const val_true = ((0 << char_shift) | nonchar_type_tag)
const val_false = ((1 << char_shift) | nonchar_type_tag)
const val_eof = ((2 << char_shift) | nonchar_type_tag) 
const val_void = ((3 << char_shift) | nonchar_type_tag)
const val_empty = ((4 << char_shift) | nonchar_type_tag)

















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