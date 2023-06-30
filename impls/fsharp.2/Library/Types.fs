namespace Types

type MALSymbol = string

/// Use a boolean to mark if the string is a keyword
type MALString = bool * string

type MALObject =
    | Symbol of MALSymbol
    | Nil
    | List of MALObject list
    | Vector of MALObject list
    | HashMap of Map<MALString, MALObject>
    | Number of int
    | Bool of bool
    | String of MALString
    | Function of (MALObject list -> EvalResult)

and EvalResult = Result<MALObject, MALError>

and MALError =
    // Generic
    | NotImplemented
    // Reader
    | InvalidFormat of kind: string * actual: string
    | UnexpectedReaderValue of unexpected: string
    | MissingReaderValue of expected: string
    | NotAllProcessed of remaining: string
    | Comment // not actually an error, is handled specially
    
    // Function invocation errors
    | InvalidArgumentType
    | WrongArgumentLength
    | UndefinedToken of string
    | InvokeOnNonFunction

type Rep = string -> string

type Reader = string list

// TODO: get rid of this
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

type ReaderResult = Result<MALObject * string list, MALError>