namespace Printer

open Types

module Printer =
    let newlineTempSubstitute = 0xF0 |> char |> string

    let rec pr_str (print_readably: bool) (input: Result<MALObject, MALError>) : string =
        match input with
        | Error e ->
            match e with
            | NotImplemented -> "Not implemented"
            | InvalidFormat(kind, actual) -> $"[Reader] Unexpected EOF, %s{actual}"
            | UnexpectedReaderValue unexpected -> $"[Reader] extra value, unexpected: %s{unexpected}"
            | MissingReaderValue expected -> $"[Reader] missing value, expected: %s{expected}"
            | NotAllProcessed remaining -> failwith "todo"
            | Comment -> ""
            | InvalidArgumentType -> failwith "todo"
            | WrongArgumentLength -> failwith "todo"
            | UndefinedToken s -> failwith "todo"
            | InvokeOnNonFunction -> failwith "todo"
        | Ok v ->
            match v with
            | Number n -> n |> string
            | Symbol s -> s |> string
            | String(isKeyword, s) ->
                // Translate keywords to the keyword representation
                if isKeyword then
                    s
                else
                    match print_readably with
                    | true -> $"%s{s}"
                    | false ->
                        s
                            // slight hack here since the second replace (\ -> \\) will mess with \n.
                            .Replace("\n", newlineTempSubstitute)
                            .Replace(@"\", @"\\")
                            .Replace("\"", "\\\"")
                            .Replace(newlineTempSubstitute, @"\n")
                        |> sprintf "\"%s\""
            | Bool true -> "true"
            | Bool false -> "false"
            | Nil -> "nil"
            | List items -> printSequenceWithoutOuter print_readably items |> sprintf "(%s)"
            | Vector items -> printSequenceWithoutOuter print_readably items |> sprintf "[%s]"
            | HashMap mapping ->
                mapping
                |> Map.toList
                |> List.collect (fun (x, y) -> [ MALObject.String x; y ])
                |> printSequenceWithoutOuter print_readably
                |> sprintf "{%s}"
            | _ -> "not implemented"

    and printSequenceWithoutOuter (print_readably: bool) (items: MALObject list) : string =
        items
        |> List.map (Ok >> pr_str print_readably)
        |> List.toSeq
        |> String.concat " "
