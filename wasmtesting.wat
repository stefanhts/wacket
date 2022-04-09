(module
    (func $testfunc (result i64)
        i64.const 42
        i64.const 21
        (i64.sub)  
    )
    (func $testfunc3 (result i64)
        (i64.sub
            (i64.add
                (i64.const 69)
                (i64.const 101))
            (i64.const 21)
        )  
    )
    (func $main (param $in i64) (result i64) 
        ;;local.get $in
        call $testfunc 
    )
    (export "main" (func $main))
)
