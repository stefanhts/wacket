/*
  Bit layout of values

  Values are either:
  - Immediates: end in #b000
  - Pointers

*/
const imm_shift = 3n
const ptr_type_mask = ((1n << imm_shift) -1n)
const box_type_tag = 1n
const cons_type_tag = 2n
const vect_type_tag = 3n
const str_type_tag = 4n
const proc_type_tag = 5n
const int_shift = (1n + imm_shift)
const int_type_mask = ((1n << int_shift) - 1n)
const int_type_tag = (0n << (int_shift - 1n))
const nonint_type_tag = (1n << (int_shift -1n))
const char_shift = (int_shift + 1n)
const char_type_mask = ((1n << char_shift) - 1n)
const char_type_tag = ((0n << (char_shift -1n )) | nonint_type_tag)
const nonchar_type_tag = ((1n << (char_shift -1n)) | nonint_type_tag)
const val_true = ((0n << char_shift) | nonchar_type_tag)
const val_false = ((1n << char_shift) | nonchar_type_tag)
const val_eof = ((2n << char_shift) | nonchar_type_tag) 
const val_void = ((3n << char_shift) | nonchar_type_tag)
const val_empty = ((4n << char_shift) | nonchar_type_tag)


function run(){
    const input = document.getElementById("inputbox").value
    const output = document.getElementById("res")
    console.log("test works");
    (async () => {
        const response = await fetch('main.wasm');
        const buffer = await response.arrayBuffer();
        const module = new WebAssembly.Module(buffer);
        const instance = new WebAssembly.Instance(module);
        const rawResult = instance.exports.main(BigInt(input));
        // console.log(rawResult);
        const result = unwrap(rawResult);
        console.log(result);
        output.innerHTML = result;
      })();
}

const typesEnum = Object.freeze({
  T_INVALID: -1,
  T_INT: 0,
  T_BOOL: 1,
  T_CHAR: 2,
  T_EOF: 3,
  T_VOID: 4,
  T_EMPTY: 5,
  T_BOX: 6,
  T_CONS: 7,
  T_VECT: 8,
  T_STR: 9,
  T_PROC: 10
})

function val_typeof(x){
  switch (x & ptr_type_mask) {
    case box_type_tag:
      return typesEnum.T_BOX
    case cons_type_tag:
      return typesEnum.T_CONS
    case vect_type_tag:
      return typesEnum.T_VECT
    case str_type_tag:
      return typesEnum.T_STR
    case proc_type_tag:
      return typesEnum.T_PROC
  }

  if ((int_type_mask & x) === int_type_tag) return typesEnum.T_INT

  if ((char_type_mask & x) === char_type_tag) return typesEnum.T_CHAR

  switch (x) {
    case val_true:
    case val_false:
      return typesEnum.T_BOOL
    case val_eof:
      return typesEnum.T_EOF
    case val_void:
      return typesEnum.T_VOID
    case val_empty:
      return typesEnum.T_EMPTY
  }

  return typesEnum.T_INVALID
}

function unwrap(raw){
  // console.log(val_typeof(raw))
  switch (val_typeof(raw)){
    case typesEnum.T_INT:
      return val_unwrap_int(raw)
    case typesEnum.T_BOOL:
      return val_unwrap_bool(raw)
    case typesEnum.T_CHAR:
      return val_unwrap_char(raw)
    case typesEnum.T_EOF:
      return "#<eof>"
    case typesEnum.T_VOID:
      return ""
    case typesEnum.T_EMPTY:
    case typesEnum.T_CONS:
    case typesEnum.T_BOX:
    case typesEnum.T_VECT:
      return "'" + result_interior(raw)
    case typesEnum.T_STR:
      return '"' + val_unwrap_str(raw) + '"'
    case typesEnum.T_PROC:
      return "#<procedure>"
    case typesEnum.T_INVALID:
      return "internal error"
  }
}

function result_interior(raw){
  // TODO:
}

function val_unwrap_str(raw){
  // TODO:
}

function str_char_u(c){
  // TODO:
}

function str_char_U(c){
  // TODO:
}

function str_char(c){
  // TODO: big ol ctrl-c ctrl-v from loot/print.c
}

function val_unwrap_char(raw){
  // TODO:
}

function val_unwrap_int(raw){
  return raw >> int_shift
}

function val_unwrap_bool(raw){
  return raw === val_true
}

function val_wrap_int(i){
  return (i << int_shift) | int_type_tag
}

function val_wrap_bool(b){
  return b ? val_true : val_false
}

function val_wrap_char(v){
  // TODO: 
}

function val_wrap_eof(){
  return val_eof
}

function val_wrap_void(){
  return val_void
}
