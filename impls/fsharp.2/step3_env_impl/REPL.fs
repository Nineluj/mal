module REPL

open Reader
open Printer
open Evaluator

let READ x = Reader.read_str x

let EVAL x = Evaluator.evalBase x

let PRINT x = Printer.pr_str false x

let PRINT_ERR err_type err = $"[{err_type}] Error: %A{err}"

let rep x =
    x
    |> READ
    |> function
        | Error f -> PRINT_ERR "Read" f
        | Ok v ->
            v
            |> EVAL Builtin.Builtin.getEnv 
            |> function
                | Error err -> PRINT_ERR "Eval" err
                | Ok v -> v |> PRINT
