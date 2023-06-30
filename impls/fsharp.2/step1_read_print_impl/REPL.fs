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
        | Error e -> PRINT_ERR "Read" e |> Ok
        | Ok v ->
            v
                |> EVAL
                |> PRINT
