module REPL

open Types
open Reader
open Printer

let READ x = Reader.read_str x
let EVAL x = x
let PRINT x = Printer.pr_str false x

let rep x =
    x |> READ |> EVAL |> PRINT
