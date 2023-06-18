module Types

type MALObject =
    | Symbol of string
    | Nil
    | List of MALObject list
    | Vector of MALObject list
    | Number of int
    | Bool of bool
    | String of string
    // | Bool of bool

type Reader = string list

type ReaderResult =
    | ReadSuccess of MALObject
    | ReadFailure of string

type EvaluateResult =
    | EvalSuccess of MALObject
    | EvalFailure of string

type ParserResult = Reader * ReaderResult