module REPL

open Reader
open Printer
open Evaluator

let READ x = Reader.read_str x

let EVAL x = Evaluator.evalBase x

let PRINT x : string = Printer.pr_str false x

let PRINT_ERR err_type err = $"[{err_type}] Error: %A{err}"

let (>=>) f1 f2 arg =
    match f1 arg with
    | Ok v -> f2 v
    | Error e -> Error e

let rep x =
    let env = Builtin.Builtin.getEnv
    x |> (READ >=> (EVAL env)) |> PRINT
