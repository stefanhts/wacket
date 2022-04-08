# 430_finalproj

## Notes

- To get started we need a runtime for wasm. We can use [wasmer](https://docs.wasmer.io/ecosystem/wasmer/getting-started) it has a CLI which should be usable to run wasm locally

- My understanding from reading [this link](https://developer.mozilla.org/en-US/docs/WebAssembly/Text_format_to_wasm) is that we need to write to .wat which is the WebAssembly Text format. From there we can use wat2wasm to turn that into the actual assembly code which can run in the browser or natively, whatever
