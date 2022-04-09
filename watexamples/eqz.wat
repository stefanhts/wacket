(module
    (export "main" (func $main))
    (func $main (result i64)
        (i64.extend_i32_s         ;; this turns i32 to i64
            (i64.eqz              ;; this returns i32 (1 if true, 0 if false)
                (i64.const 12)
            )
        )
    )
)
