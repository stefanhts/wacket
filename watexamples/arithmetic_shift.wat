(module
    (export "main" (func $main))
    (func $main (result i64)
        (i64.shr_s 
            (i64.const 672) ;; 42
            (i64.const 1)
        )
    )
)
