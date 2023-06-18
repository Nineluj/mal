open System

[<EntryPoint>]
let rec main args =
    let isInteractive = not (Array.contains "--raw" args)
    InterpreterConsole.run isInteractive
    0
