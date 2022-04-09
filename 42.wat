(module
    (export "main" (func $main))
    (func $main (result i64)
        (i64.add
            (i64.const 42)
            (i64.const 1)
        )
    )
)
