%.wat: %.rkt
	racket -t lang/compile-file.rkt -m $< > $@

%.wasm: %.wat
	wat2wasm $< -o public/main.wasm

clean:
	rm *.wat