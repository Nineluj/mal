module Reader

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

    let readString s : ReaderResult =
        let stringContentOpt = getStringWithinQuotes s

        match stringContentOpt with
        | None -> ReadFailure @"Got EOF before closing quote for string"
        | Some stringContent -> ReadSuccess(String(stringContent))

    let read_atom (r: Reader) : ParserResult =
        match r with
        | [] -> [], ReadFailure "Can't parse atom for empty input"
        | s :: rest ->
            rest,
            let firstChar = s[0]

            if isRestrictedChar firstChar then
                ReadFailure $"Cannot use restricted char \"{firstChar}\" for atom: {s}"
            else if firstChar >= '0' && firstChar <= '9' then
                s |> int |> Number |> ReadSuccess
            else if firstChar = '"' then
                readString s
            else if firstChar = ':' then
                ReadSuccess(Keyword.create s)
            else
                match s with
                | "true" -> Bool true
                | "false" -> Bool false
                | "nil" -> Nil
                | x -> Symbol x
                |> ReadSuccess

    let createMapFromList (lst: MALObject list list) : ReaderResult =
        let pairsWithValidKeys, pairsWithInvalidKeys =
            lst
            |> List.map (fun x ->
                let key = x[0]
                let value = x[1]

                match key with
                | String s -> ReadSuccess(String s), value
                | x -> ReadFailure $"Can't use type %s{x.GetType() |> string} for key", value)
            |> List.partition (fun (keyReadResult, _) ->
                match keyReadResult with
                | ReadSuccess _ -> true
                | ReadFailure _ -> false)

        if pairsWithInvalidKeys.Length > 0 then
            // return the first error if we have more than one error
            fst (pairsWithInvalidKeys[0])
        else
            //
            (pairsWithValidKeys
             |> List.map (fun (keyResult, value) ->
                 match keyResult with
                 | ReadSuccess(String s) -> s, value
                 | _ -> "", value)
             |> Map.ofList)
            |> MALObject.HashMap
            |> ReadSuccess

    let rec read_form (r: Reader) : ParserResult =
        match r with
        | [] -> [], ReadFailure "Can't parse empty token"
        | "(" :: t -> read_list t
        | "[" :: t -> read_vec t
        | "{" :: t -> read_hashmap t
        | "'" :: t -> [], ReadFailure $"quoted values are not implemented yet. Tried to parse: {t}" // read_quoted t
        | other -> read_atom other

    and read_seq (closingToken: string) (resultingType: (MALObject list -> MALObject)) (r: Reader) : ParserResult =
        let rec innerFn (r: Reader) (acc: MALObject list) : Reader * ReaderResult =
            match r with
            | [] -> [], ReadFailure @"Got EOF before end of sequence"
            | token :: t when token = closingToken -> t, ReadSuccess(resultingType (List.rev acc))
            | other ->
                match read_form other with
                | remaining, ReadSuccess result -> innerFn remaining (result :: acc)
                | _, ReadFailure err -> [], ReadFailure err

        innerFn r []

    and read_hashmap (r: Reader) : ParserResult =
        let innerResult = read_seq "}" MALObject.List r

        match innerResult with
        | remaining, ReadFailure err -> remaining, ReadFailure err
        | remaining, ReadSuccess(MALObject.List lst) -> remaining, lst |> List.chunkBySize 2 |> createMapFromList
        | _ -> [], ReadFailure "couldn't parse hashmap items as list (required for hashmap parsing)"

    and read_list (r: Reader) : ParserResult = read_seq ")" MALObject.List r
    and read_vec (r: Reader) : ParserResult = read_seq "]" MALObject.Vector r

    let read_str str =
        tokenize str
        |> List.filter (fun x -> x.Length > 0)
        |> read_form
        |> function
            | [], ReadSuccess x -> ReadSuccess x
            | remaining, ReadSuccess _ ->
                ReadFailure $"Not all tokens were processed. Remaining: %A{List.toArray remaining}"
            | _, ReadFailure err -> ReadFailure err


// and read_quoted (r: Reader): ParserResult =
//     match r with
//     | [] -> [], ReadFailure @"Got EOF, expected quoted value"
//     | inner -> (match read_form inner with
//                 | remaining, ReadSuccess (MALObject.List lst) -> remaining, ReadSuccess (
//                         MALObject.List ((MALObject.Symbol "quote")::lst)
//                     )
//                 | remaining, ReadSuccess  -> remaining ReadFailure "only lists for now"
//                         )
