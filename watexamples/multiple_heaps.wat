(module
    (export "main" (func $main))
    (memory (export "memory") 2)
    (func $main (result i64)
        (i64.store (i32.const 16384) (i64.const 672))
        (i64.store (i32.const 16392) (i64.const 1104))
        (i64.load (i32.const 16384))
    )
)