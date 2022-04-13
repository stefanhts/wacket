default:
	racket -t lang/compile-file.rkt -m example.wkt > example.wat
	wat2wasm example.wat -o server/public/main.wasm

%.wat: %.wkt
	racket -t lang/compile-file.rkt -m $< > $@

%.wasm: %.wat
	wat2wasm $< -o server/public/main.wasm

clean:
	rm *.wat
