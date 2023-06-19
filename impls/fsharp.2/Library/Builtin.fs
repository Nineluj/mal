namespace Builtin

open Types

module NumericalOperations =
    let create (operand: string) (operandFun: int -> int -> int) =
        (MALSymbol operand),
        MALObject.Function (fun argList ->
            (match argList with
             | [ Number a; Number b ] -> operandFun a b |> Number |> EvalSuccess
             | [ _; _ ] -> EvalFailure InvalidArgumentType
             | _ -> EvalFailure WrongArgumentLength))

    let items: MALEnvironment =
        [ create "+" (fun a b -> a + b)
          create "-" (fun a b -> a - b)
          create "*" (fun a b -> a * b)
          create "/" (fun a b -> a / b) ]
        |> Map.ofList

    let addItems (env: MALEnvironment) : MALEnvironment =
        Map.fold (fun acc key value -> Map.add key value acc) env items

// how will method / operator overloading work?
// maybe Map<Symbol, Map<Type list, Function>>
module MALEnvironment =
    let create_with_builtins: MALEnvironment =
        Map.empty
        |> NumericalOperations.addItems