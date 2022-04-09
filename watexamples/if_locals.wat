(module
    (export "main" (func $main))
    (func $main (result i64)
        (local $res i64)
        (if
            (i64.eqz (i64.const 42))
            (local.set $res (i64.const 69))
            (local.set $res (i64.const 420))
        )
        (local.get $res)
    )
)
