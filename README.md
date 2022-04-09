# 430_finalproj

## Target Language: WebAssembly

We need to write to `.wat` which is the WebAssembly Text format. From there we can use wat2wasm to turn that into the actual assembly code which can run in the browser.

- WebAssembly Text Format (.wat)
  - [Understanding WebAssembly text format](https://developer.mozilla.org/en-US/docs/WebAssembly/Understanding_the_text_format)
  - [Info on Text Format from the WebAssembly GitHub repo](https://webassembly.github.io/spec/core/text/index.html) (this gets _very_ spec-y)
  - [WebAssembly Spec PDF](https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=&ved=2ahUKEwj24szLpob3AhX2l3IEHQHADgEQFnoECAcQAQ&url=https%3A%2F%2Fwebassembly.github.io%2Fspec%2Fcore%2F_download%2FWebAssembly.pdf&usg=AOvVaw008spp5_YkxtS0xQ5c3xJw)

- Compiling from WebAssembly Text format (.wat) to WebAssembly Binary format (.wasm)
  - [Info](https://developer.mozilla.org/en-US/docs/WebAssembly/Text_format_to_wasm)
  - [WABT: The WebAssembly Binary Toolkit](https://github.com/webassembly/wabt)
  - Think of this as our version of nasm
  - can install from a package manager (e.g. `sudo apt install wabt`)

### wat AST

```
type Module = (Module [list of Definitions])

type Definition = Import
                | Export
                | Func
                | Start

type Start = (Start funcname)

;; imports have a 2 level namespace: module then function
type Import = (Import modulename funcname FuncSignature)

type Export = (Export name funcdesc)

type Func = (Func FuncSignature [list of Locals] Body)

type FuncSignature = (FuncSignature name? [list of Params]? Result?)

type Param = (Param name? Type)

type Result = (Result Type)

type Local = (Local name? Type)

type Type = i32
          | i64
          | f32
          | f64

type Body = (Body [list of Instructions])

;; this deviates from the official spec because we'll only use i64
type Instruction = UnInst
                 | BiInst
                 | ZrInst

type ZrInst = 'i64.add
            | 'i64.sub
            | 'i64.mul
            | 'i64.div

type UnInst = (Const i64)
            | (Call funcname)

type BiInst = (Add Instruction Instruction)
            | (Sub Instruction Instruction)
            | (Mul Instruction Instruction)
            | (Div Instruction Instruction)
```

## The Runtime system

Our runtime system will be written in JavaScript, which can interface with compiled WASM bidirectionally.
We will have a minimal HTML file to act as an interface to the runtime system.
While this could be exposed to localhost with a web server, it may simply be opened as a file in the browser.

## Source Language: Racket (Loot)

We'll be aiming to implement all the features present in Loot, the reduced version of Racket created in class already.
This should allow for the re-use of much of the compiler infrastructure, like the lexer, parser, and AST.

## TODO

- Once the runtime system and HTML interface is completed to a minimal level, include instructions in the README for using the runtime system (e.g. from super simple .wat file to seeing result in browser)
- make a make file?
- Import Loot code
- Create WASM representation in racket (do we need to come up with our own version of `seq`?)
- WASM pretty printer
- start working on functionality
