module Printer

open Types

let rec pr_str (print_readably: bool) (input: MALObject) : string =
    match input with
    | Number n -> n |> string
    | Symbol s -> s |> string
    | String s ->
        // Translate keywords to the keyword representation
        if Keyword.is_keyword s then
            Keyword.to_string s
        else 
            match print_readably with
            | true -> $"%s{s}"
            | false ->
                s.Replace("\n", @"\n").Replace(@"\", @"\\").Replace("\"", "\\\"")
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

and printSequenceWithoutOuter (print_readably: bool) (items: MALObject list) =
    items |> List.map (pr_str print_readably) |> String.concat " "
