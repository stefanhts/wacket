(module
    (export "main" (func $main))
    (func $main (result i64)
        (if
            (result i64)
            (i64.eqz (i64.const 42))
            (i64.const 69)
            (i64.const 420)
        )
    )
)
