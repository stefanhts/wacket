(module
    (func $testfunc3 (param i64) (result i64)
        i64.const 42
        i64.const 21
        i64.sub    
    )
    (func $testfunc (result i64)
        (i64.sub
            (i64.const 42)
            (i64.const 21)
        )  
    )
    (func $main (param $in i64) (result i64) 
        ;;local.get $in
        call $testfunc 
    )
    (export "main" (func $main))
)
