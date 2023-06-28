module REPL

open Types
open Reader
open Printer
open Environment
open Evaluator

let READ x = Reader.read_str x

let baseEnv =
    List.fold (fun x (key, v) -> MALEnvironment.set x key v) (MALEnvironment.create None) Builtin.Builtin.BaseOperations.items
    
let EVAL x = Evaluator.evalBase x

let PRINT x = Printer.pr_str false x

let PRINT_ERR err_type err = $"[{err_type}] Error: %A{err}"

let rep x =
    let env = baseEnv
    x
    |> READ
    |> function
        | Error f -> PRINT_ERR "Read" f
        | Ok v ->
            v
            |> EVAL env
            |> function
                | EvalFailure err -> PRINT_ERR "Eval" err
                | EvalSuccess v -> v |> PRINT
