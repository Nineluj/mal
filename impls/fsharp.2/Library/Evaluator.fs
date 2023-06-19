namespace Evaluator

open Types

module Evaluator =
    let rec evalBase (env: MALEnvironment) (x: MALObject) : EvalResult =
        match x with
        | List [] -> EvalSuccess x
        | List items ->
            match
                ResultList.compute
                    items
                    (evalBase env)
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
                    (evalBase env)
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
        | HashMap mapItems ->
            mapItems
            |> Map.toList
            |> List.collect (fun (key, value) -> [ MALObject.String key; value ])
            |> evalAstSequence (fun elements ->
                elements
                |> List.chunkBySize 2
                |> List.map (fun l ->
                    match l[0], l[1] with
                    | String key, value -> key, value
                    | _ -> failwith "invalid code path")
                |> Map.ofList
                |> MALObject.HashMap)
        | Symbol s ->
            (match Map.tryFind s env with
             | None -> s |> UndefinedToken |> EvalFailure
             | Some value -> EvalSuccess value)
        | _ -> EvalSuccess x
