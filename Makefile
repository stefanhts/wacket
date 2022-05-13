default:
	racket -t lang/compile-file.rkt -m program.wkt > program.wat
	wat2wasm program.wat -o server/public/main.wasm

%.wat: %.wkt
	racket -t lang/compile-file.rkt -m $< > $@

%.wasm: %.wat
	wat2wasm $< -o server/public/main.wasm

clean:
	rm *.wat
