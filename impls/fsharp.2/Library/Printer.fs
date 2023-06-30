namespace Printer

open Library.ResultExtensions

module Printer =
    open Types    

    let rec pr_str (print_readably: bool) (input: MALObject) : Result<string, string> =
        match input with
        | Number n -> n |> string |> Ok
        | Symbol s -> s |> string |> Ok
        | String (isKeyword, s) ->
            // Translate keywords to the keyword representation
            if isKeyword then
                Ok s
            else 
                match print_readably with
                | true -> Ok $"%s{s}"
                | false ->
                    s.Replace("\n", @"\n").Replace(@"\", @"\\").Replace("\"", "\\\"")
                    |> sprintf "\"%s\"" |> Ok
        | Bool true -> Ok "true"
        | Bool false -> Ok "false"
        | Nil -> Ok "nil"
        | List items -> printSequenceWithoutOuter print_readably items |> Result.map (sprintf "(%s)")
        | Vector items -> printSequenceWithoutOuter print_readably items |> Result.map (sprintf "[%s]")
        | HashMap mapping ->
            mapping
            |> Map.toList
            |> List.collect (fun (x, y) -> [ MALObject.String x; y ])
            |> printSequenceWithoutOuter print_readably
            |> Result.map (sprintf "{%s}")
        | _ -> Error "not implemented"

    and printSequenceWithoutOuter (print_readably: bool) (items: MALObject list): Result<string, string> =
        items
        |> List.map (pr_str print_readably)
        |> Result.convertResults
        |> Result.map List.toSeq
        |> Result.map (String.concat " ")
