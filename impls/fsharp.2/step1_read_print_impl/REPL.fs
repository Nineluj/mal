module REPL

open Types
open Reader
open Printer

let READ x = Reader.read_str x
let EVAL x = x
let PRINT x = Printer.pr_str false x

let PRINT_ERR err_type err = $"[{err_type}] Error: {err}"

let rep x =
    x
    |> READ
    |> function
        | ReadFailure f -> PRINT_ERR "Read" f
        | ReadSuccess v ->
            v
                |> EVAL
                |> PRINT
