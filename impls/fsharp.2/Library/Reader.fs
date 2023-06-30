namespace Reader

open System
open Types

module Reader =
    open System.Text.RegularExpressions

    let tokenize (str: string) =
        Regex.Matches(str, """[\s,]*(~@|[\[\]{}()'`~^@]|"(?:\\.|[^\\"])*"?|;.*|[^\s\[\]{}('"`,;)]*)""")
        |> Seq.cast<Match>
        |> Seq.map (fun x -> x.Groups[1].Value)
        |> List.ofSeq

    let isRestrictedChar c =
        "[]{}()'`~^@" |> List.ofSeq |> List.contains c

    /// Given a string starting with quotes, get the data until the quote at the end of the string.
    /// Accounts for escaped quotes and returns none if the end quote comes prior to the string's end.
    let getStringWithinQuotes (s: string) : string option =
        let rec innerFn (isEscaped: bool) (acc: char list) (chars: char list) =
            match chars, isEscaped with
            | [], _ -> None
            | [ last ], false when last = '"' -> Some(acc |> List.rev |> System.String.Concat)
            | [ _ ], _ -> None
            | h :: _, false when h = '"' -> None
            | h :: t, false when h = '\\' -> innerFn true acc t
            | h :: t, true ->
                (match h with
                 // a backslash followed by a double quote is translated into a plain double quote character
                 | '"' -> innerFn false ('"' :: acc) t
                 // a backslash followed by "n" is translated into a newline
                 | 'n' -> innerFn false ('\n' :: acc) t
                 // backslash followed by another backslash is translated into a single backslash
                 | '\\' -> innerFn false ('\\' :: acc) t
                 | _ -> None)
            | h :: t, _ -> innerFn false (h :: acc) t

        innerFn false [] (List.skip 1 (List.ofSeq s))

    let readString s =
        let stringContentOpt = getStringWithinQuotes s

        match stringContentOpt with
        | None -> Error @"Got EOF before closing quote for string"
        | Some stringContent -> Ok(String(false, stringContent))

    let read_atom (r: Reader) : ReaderResult =
        match r with
        | [] -> MissingReaderValue "Can't parse atom for empty input" |> Error
        | s :: rest ->
            let firstChar = s[0]

            if isRestrictedChar firstChar then
                UnexpectedReaderValue $"Cannot use restricted char \"{firstChar}\" for atom: {s}"
                |> Error
            else if firstChar = '-' && s.Length > 1 && Char.IsDigit s[1] then
                (s[1..] |> int |> (fun x -> -x) |> Number, rest) |> Ok
            else if Char.IsDigit firstChar then
                (s |> int |> Number, rest) |> Ok
            else if firstChar = '"' then
                match readString s with
                | Ok v -> Ok(v, rest)
                | Error e -> InvalidFormat ("string", e) |> Error
            else if firstChar = ':' then
                ((true, s) |> MALObject.String, rest) |> Ok
            else
                match s with
                | "true" -> Bool true
                | "false" -> Bool false
                | "nil" -> Nil
                | x -> Symbol x
                |> (function
                | v -> Ok(v, rest))

    // TODO: clean this up
    let createMapFromList (lst: MALObject list list) =
        let pairsWithValidKeys, pairsWithInvalidKeys =
            lst
            |> List.map (fun x ->
                let key = x[0]
                let value = x[1]

                match key with
                | String s -> Ok(String s), value
                | x -> Error $"Can't use type %s{x.GetType() |> string} for key", value)
            |> List.partition (fun (keyReadResult, _) ->
                match keyReadResult with
                | Ok _ -> true
                | Error _ -> false)

        if pairsWithInvalidKeys.Length > 0 then
            // return the first error if we have more than one error
            fst (pairsWithInvalidKeys[0])
        else
            //
            (pairsWithValidKeys
             |> List.map (fun (keyResult, value) ->
                 match keyResult with
                 | Ok(String s) -> s, value
                 | _ -> failwith "invalid code path")
             |> Map.ofList)
            |> MALObject.HashMap
            |> Ok

    let rec read_form (r: Reader) : ReaderResult =
        match r with
        | [] -> MissingReaderValue "Can't parse empty token" |> Error
        | "(" :: t -> read_list t
        | "[" :: t -> read_vec t
        | "{" :: t -> read_hashmap t
        | "'" :: t -> read_quoted "quote" t
        | "`" :: t -> read_quoted "quasiquote" t
        | "~" :: t -> read_quoted "unquote" t
        | "~@" :: t -> read_quoted "splice-unquote" t
        | "@" :: t -> read_quoted "deref" t
        | "^" :: t -> read_meta t
        | firstString :: _ when firstString.StartsWith(";") -> Error Comment
        | other -> read_atom other
    
    and read_meta t =
        match read_form t with
        | Error e -> Error e
        | Ok (part1, rest) ->
            match read_form rest with
            | Error e -> Error e
            | Ok (part2, rest2) ->
                Ok (MALObject.List [(MALObject.Symbol "with-meta"); part2; part1], rest2)

    and read_seq (closingToken: string) (resultingType: MALObject list -> MALObject) (r: Reader) : ReaderResult =
        let rec innerFn (r: Reader) (acc: MALObject list) : ReaderResult =
            match r with
            | [] -> MissingReaderValue $"unbalanced sequence data, expecting %s{closingToken}" |> Error 
            | token :: t when token = closingToken -> Ok(resultingType (List.rev acc), t)
            | other ->
                match read_form other with
                | Ok(result, remaining) -> innerFn remaining (result :: acc)
                | Error e -> Error e

        innerFn r []

    and read_hashmap (r: Reader) : ReaderResult =
        let innerResult = read_seq "}" MALObject.List r

        match innerResult with
        | Ok(MALObject.List lst, remaining) ->
            let chunked = lst |> List.chunkBySize 2

            match createMapFromList chunked with
            | Error e -> UnexpectedReaderValue e |> Error
            | Ok v -> Ok(v, remaining)
        | Error error -> Error error
        | Ok (other, _) -> UnexpectedReaderValue (other.GetType().ToString()) |> Error

    and read_list (r: Reader) : ReaderResult = read_seq ")" MALObject.List r
    and read_vec (r: Reader) : ReaderResult = read_seq "]" MALObject.Vector r
    
    and read_quoted (symbolName: string) (r: Reader): ReaderResult =
        match r with
        | [] -> MissingReaderValue "quoted" |> Error
        | content ->
            match read_form content with
            | Ok (v, rest) -> Ok (MALObject.List [(MALObject.Symbol symbolName); v], rest)
            | Error e -> Error e

    let read_str str: Result<MALObject, MALError> =
        tokenize str
        |> List.filter (fun x -> x.Length > 0)
        |> read_form
        |> function
            | Ok(x, []) -> Ok x
            | Ok(x, rest) ->
                match rest with
                | [remainingString] when remainingString.StartsWith(";") -> Ok x
                | other -> $"%A{List.toArray other}" |> NotAllProcessed |> Error
            | Error e -> Error e
