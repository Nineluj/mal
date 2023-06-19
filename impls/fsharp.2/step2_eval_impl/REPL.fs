module REPL

open Types
open Reader
open Printer
open Builtin
open Evaluator

let READ x = Reader.read_str x

let EVAL x = Evaluator.evalBase x

let PRINT x = Printer.pr_str false x

let PRINT_ERR err_type err = $"[{err_type}] Error: %A{err}"

let rep x =
    x
    |> READ
    |> function
        | ReadFailure f -> PRINT_ERR "Read" f
        | ReadSuccess v ->
            v
            |> EVAL MALEnvironment.create_with_builtins
            |> function
                | EvalFailure err -> PRINT_ERR "Eval" err
                | EvalSuccess v -> v |> PRINT
