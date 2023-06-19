module Types

type MALObject =
    | Symbol of string
    | Nil
    | List of MALObject list
    | Vector of MALObject list
    | HashMap of Map<string, MALObject>
    | Number of int
    | Bool of bool
    | String of string
    // | Bool of bool


module Keyword =
    let keywordPrefix = char 0x29E
    
    let create (s: string) = MALObject.String $"%c{keywordPrefix}%s{s[1..]}"
    let to_string (s: string) = $":{s[1..]}"
    let is_keyword (s: string) = s.Length > 0 && s[0] = keywordPrefix

type Reader = string list

type ReaderResult =
    | ReadSuccess of MALObject
    | ReadFailure of string

type EvaluateResult =
    | EvalSuccess of MALObject
    | EvalFailure of string

type ParserResult = Reader * ReaderResult