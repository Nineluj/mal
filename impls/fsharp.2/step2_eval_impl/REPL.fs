module REPL

open Types
open Reader
open Printer
open Builtin

let READ x = Reader.read_str x

let rec EVAL (env: MALEnvironment) (x: MALObject) : EvalResult =
    match x with
    | List [] -> EvalSuccess x
    | List items ->
        match
            ResultList.compute
                items
                (EVAL env)
                (fun x ->
                    match x with
                    | EvalSuccess _ -> true
                    | EvalFailure _ -> false)
                (fun x ->
                    match x with
                    | EvalSuccess v -> v
                    | EvalFailure _ -> failwith "invalid codepath")
        with
        | ResultList.ListResultFailure fail -> fail
        | ResultList.ListResultSuccess listElements ->
            // should never fail since this function is only invoked on non-empty lists
            match listElements with
            // its already getting parsed, replace it here
            | Function f :: args -> (f args)
            // is this needed?
            | Symbol invokedFunctionSymbol :: args ->
                (match Map.tryFind invokedFunctionSymbol env with
                 | None -> invokedFunctionSymbol |> UndefinedToken |> EvalFailure
                 | Some(Function f) -> (f args)
                 | Some _ -> InvokeOnNonFunction |> EvalFailure)

            | _ -> failwith "invalid codepath"

    | _ -> evalAst env x

and evalAst (env: MALEnvironment) (x: MALObject) : EvalResult =
    let evalAstSequence (createSequence: MALObject list -> MALObject) (items: MALObject list) =
        match
            ResultList.compute
                items
                (EVAL env)
                (fun x ->
                    match x with
                    | EvalSuccess _ -> true
                    | EvalFailure _ -> false)
                (fun x ->
                    match x with
                    | EvalSuccess v -> v
                    | EvalFailure _ -> failwith "invalid codepath")
        with
        | ResultList.ListResultSuccess parsed -> parsed |> createSequence |> EvalSuccess
        | ResultList.ListResultFailure fail -> fail
    
    match x with
    | List items -> evalAstSequence MALObject.List items
    | Vector items -> evalAstSequence MALObject.Vector items
    | Symbol s ->
        (match Map.tryFind s env with
         | None -> s |> UndefinedToken |> EvalFailure
         | Some value -> EvalSuccess value)
    | _ -> EvalSuccess x

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
