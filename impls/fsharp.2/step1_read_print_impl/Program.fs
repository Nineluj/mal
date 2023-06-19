open Library

[<EntryPoint>]
let rec main args =
    let isInteractive = not (Array.contains "--raw" args)
    let console = InterpreterConsole.create REPL.rep isInteractive
    InterpreterConsole.run console
    0
