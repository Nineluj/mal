module REPL

let READ x = x
let EVAL x = x
let PRINT x = x
let rep x =
    x
    |> READ
    |> EVAL
    |> PRINT

