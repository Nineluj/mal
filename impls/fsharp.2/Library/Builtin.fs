namespace Builtin

open Environment
open Types

module Builtin =    
    type NumericalOperations () =
        static member create (operand: string) (operandFun: int -> int -> int) =
            (MALSymbol operand),
            MALObject.Function (fun argList ->
                (match argList with
                 | [ Number a; Number b ] -> operandFun a b |> Number |> EvalSuccess
                 | [ _; _ ] -> EvalFailure InvalidArgumentType
                 | _ -> EvalFailure WrongArgumentLength))

        static member items =
            [ NumericalOperations.create "+" (fun a b -> a + b)
              NumericalOperations.create "-" (fun a b -> a - b)
              NumericalOperations.create "*" (fun a b -> a * b)
              NumericalOperations.create "/" (fun a b -> a / b) ]
    
    type BaseOperations () =
        static member items: (MALSymbol * MALObject) list = NumericalOperations.items