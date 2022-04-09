(module
    (func $square (param i32) (result i32)
        local.get 0
        local.get 0
        i32.mul    
    )
    (func $main (param i32) (result i32) 
        local.get 0
        call $square 
    )
    (export "main" (func $main))
)
