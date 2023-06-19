namespace Types

type MALSymbol = string

type MALObject =
    | Symbol of MALSymbol
    | Nil
    | List of MALObject list
    | Vector of MALObject list
    | HashMap of Map<string, MALObject>
    | Number of int
    | Bool of bool
    | String of string
    | Function of (MALObject list -> EvalResult)

and EvalResult =
    | EvalSuccess of MALObject
    | EvalFailure of EvalFailure

and EvalFailure =
    | InvalidArgumentType
    | WrongArgumentLength
    | UndefinedToken of string
    | InvokeOnNonFunction

type MALEnvironment = Map<MALSymbol, MALObject>

module Keyword =
    let keywordPrefix = char 0x29E

    let create (s: string) =
        MALObject.String $"%c{keywordPrefix}%s{s[1..]}"

    let to_string (s: string) = $":{s[1..]}"
    let is_keyword (s: string) = s.Length > 0 && s[0] = keywordPrefix

type Reader = string list

[<RequireQualifiedAccess>]
module ResultList =
    type ListResultResult<'a, 'b> =
        | ListResultSuccess of 'a list
        | ListResultFailure of 'b

    let compute
        (lst: 'a list)
        (mapFn: 'a -> 'result)
        (isGood: 'result -> bool)
        (toDesired: 'result -> 'successType)
        =
        let successes, failures = lst |> List.map mapFn |> List.partition isGood

        if failures.Length > 0 then
            ListResultFailure failures[0]
        else
            successes |> List.map toDesired |> ListResultSuccess



    (*
    Two lists:
    1. Success values
    2. Error messages
    *)
    let unroll (lst: 'result list) (predicate: 'result -> (bool * string option)) =
        lst |> List.partition (fun x -> fst (predicate x))



type ReaderResult =
    | ReadSuccess of MALObject
    | ReadFailure of string

type ParserResult = Reader * ReaderResult
