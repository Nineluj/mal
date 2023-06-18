module Printer

open Types

let rec pr_str (print_readably: bool) (input: MALObject) : string =
    match input with
    | Number n -> n |> string
    | Symbol s -> s |> string
    | String s ->
        match print_readably with
        | true -> $"%s{s}"
        | false ->
            s.Replace("\n", @"\n").Replace(@"\", @"\\").Replace("\"", "\\\"")
            |> sprintf "\"%s\""
    | Bool true -> "true"
    | Bool false -> "false"
    | Nil -> "nil"
    | List items | Vector items -> items |> List.map (pr_str print_readably) |> String.concat " " |> sprintf "(%s)"
