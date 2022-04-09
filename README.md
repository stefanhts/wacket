# 430_finalproj

## Notes

- To get started we need a runtime for wasm. We can use [wasmer](https://docs.wasmer.io/ecosystem/wasmer/getting-started) it has a CLI which should be usable to run wasm locally

- My understanding from reading [this link](https://developer.mozilla.org/en-US/docs/WebAssembly/Text_format_to_wasm) is that we need to write to .wat which is the WebAssembly Text format. From there we can use wat2wasm to turn that into the actual assembly code which can run in the browser or natively, whatever

- WebAssembly Text Format (.wat)
  - [Understanding WebAssembly text format](https://developer.mozilla.org/en-US/docs/WebAssembly/Understanding_the_text_format)

- Compiling from WebAssembly Text format (.wat) to WebAssembly Binary format (.wasm)
  - [WABT: The WebAssembly Binary Toolkit](https://github.com/webassembly/wabt)
  - Think of this as our version of nasm
  - can install from a package manager (e.g. `sudo apt install wabt`)

- Running a .wasm file
  - [Wasmer](https://docs.wasmer.io/ecosystem/wasmer/getting-started)
  - prerequisites (for linux, at least):
    - `libxkbcommon0` (e.g. `sudo apt install libxkbcommon0`)
    - `libtinfo5` (e.g. `sudo apt install libtinfo5`)
  - install with: `curl https://get.wasmer.io -sSfL | sh`
