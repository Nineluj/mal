namespace Builtin

open Environment
open Types

module Builtin =    
    type NumericalOperations () =
        static member create (operand: string) (operandFun: int -> int -> int) =
            (MALSymbol operand),
            MALObject.Function (fun argList ->
                (match argList with
                 | [ Number a; Number b ] -> operandFun a b |> Number |> Ok 
                 | [ _; _ ] -> Error InvalidArgumentType
                 | _ -> Error WrongArgumentLength))

        static member items =
            [ NumericalOperations.create "+" (fun a b -> a + b)
              NumericalOperations.create "-" (fun a b -> a - b)
              NumericalOperations.create "*" (fun a b -> a * b)
              NumericalOperations.create "/" (fun a b -> a / b) ]
    
    let getEnv = 
        List.fold (fun x (key, v) -> MALEnvironment.set x key v) (MALEnvironment.create None) NumericalOperations.items
