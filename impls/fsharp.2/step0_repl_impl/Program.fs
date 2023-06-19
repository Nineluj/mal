open Library

[<EntryPoint>]
let rec main args =
    let isInteractive = not (Array.contains "--raw" args)
    let console = InterpreterConsole.createConsole REPL.rep isInteractive
    console.run
    0
