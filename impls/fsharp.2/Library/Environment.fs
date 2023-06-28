namespace Environment

open Types

type MALEnvironment = { outer: MALEnvironment option; data: Map<MALSymbol, MALObject> }

// how will method / operator overloading work?
// maybe Map<Symbol, Map<Type list, Function>>
module MALEnvironment =
    let create outer: MALEnvironment = { outer = outer; data = Map.empty }
    
    let set (env: MALEnvironment) (symbol: MALSymbol) (value: MALObject) =
        { env with data = Map.add symbol value env.data }
    
    let rec tryFind (symbol: MALSymbol) (env: MALEnvironment) =
        match (Map.containsKey symbol env.data), env.outer with
        | true, _ -> Some env
        | false, Some outerEnv -> tryFind symbol outerEnv
        | false, None -> None
    
    let get (symbol: MALSymbol) (env: MALEnvironment) =
        match tryFind symbol env with
        | Some env -> Map.tryFind symbol env.data
        | _ -> None
