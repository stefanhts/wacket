(module
    (func $square (param i32) (result i64)
        (i64.sub
            (i64.const 42)
            (i64.const 69) 
        )
    )
    (func $main (param i64) (result i64) 
        local.get 0
        call $square 
    )
    (export "main" (func $main))
)
