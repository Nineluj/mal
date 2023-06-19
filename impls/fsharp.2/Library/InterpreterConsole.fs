namespace Library

module InterpreterConsole =
    open System

    type UserInput = { text: string }

    let defaultInput = { text = "" }

    type CommandHistory =
        { historyPast: string list
          historyFuture: string list }

    let defaultCommandHistory = { historyPast = []; historyFuture = [] }

    /// Add a new item to the history
    let pushHistory (history: CommandHistory) (newCommand: string) =
        let rec innerFn (ch: CommandHistory) =
            match ch.historyFuture with
            | [] -> ch
            | h :: t ->
                innerFn
                    { historyFuture = t
                      historyPast = h :: ch.historyPast }

        let restoredHistory = innerFn history

        { restoredHistory with
            historyPast = newCommand :: restoredHistory.historyPast }

    /// State tracks the state for the input on the line that the user is entering
    type ConsoleState =
        { input: UserInput
          history: CommandHistory }

    type UserActionType =
        | CharacterInput of char
        | DeleteLastChar
        | HistoryForward
        | HistoryBack
        | SubmitCommand
        // Basic version of submitCommand for non-interactive
        | SubmitCommandBasic of string
        | ClearInput

    type CommandResult =
        | None
        | Execute of string

    let readOne () =
        let newReadKey = Console.ReadKey(true)

        match newReadKey.Key, newReadKey.Modifiers with
        | ConsoleKey.Enter, _ -> SubmitCommand
        | ConsoleKey.Backspace, _ -> DeleteLastChar
        | ConsoleKey.UpArrow, _ -> HistoryBack
        | ConsoleKey.DownArrow, _ -> HistoryForward
        | ConsoleKey.U, ConsoleModifiers.Control -> ClearInput
        | _ -> CharacterInput newReadKey.KeyChar

    let readBasic () = SubmitCommandBasic(Console.ReadLine())

    let updateState (state: ConsoleState) (action: UserActionType) : ConsoleState * CommandResult =
        match action with
        | CharacterInput c ->
            { state with
                input = { text = state.input.text + string c } },
            None
        | SubmitCommand ->
            { input = defaultInput
              history = pushHistory state.history state.input.text },
            Execute state.input.text
        | DeleteLastChar ->
            let newText =
                match state.input.text, state.input.text.Length with
                | s, 0 -> s
                | s, sLength -> s[.. sLength - 2]

            { state with
                input = { text = newText } },
            None
        | HistoryBack ->
            match (state.history.historyPast, state.history.historyFuture) with
            | [], _ -> { state with input = defaultInput }, None
            | h :: t, future ->
                { input = { text = h }
                  history =
                    { historyFuture = h :: future
                      historyPast = t } },
                None
        | HistoryForward ->
            match (state.history.historyPast, state.history.historyFuture) with
            | _, [] -> { state with input = defaultInput }, None
            | past, h :: t ->
                { input = { text = h }
                  history =
                    { historyFuture = t
                      historyPast = h :: past } },
                None
        | ClearInput -> { state with input = defaultInput }, None
        | SubmitCommandBasic s -> state, Execute s

    let renderBasic (_: ConsoleState) : unit = Console.Write "user> "

    let render (state: ConsoleState) : unit =
        let prompt = "(*)> "
        let currentRow = Console.CursorTop
        let currentCol = Console.CursorLeft

        // send delete characters if there has been a deletion
        match currentCol - (state.input.text.Length + prompt.Length) with
        | n when n > 0 -> Console.Write(String.replicate n "\b \b")
        | _ -> ()

        Console.SetCursorPosition(0, currentRow)
        Console.Write(prompt)
        Console.Write(state.input.text)

    let renderResultBasic (rep: string -> string) result =
        match result with
        | Execute txt -> rep txt |> sprintf "%s\n" |> Console.Write
        | None -> ()

    let renderResult (rep: string -> string) result =
        match result with
        | Execute txt -> rep txt |> sprintf "\n%s\n" |> Console.Write
        | None -> ()
    
    type InterpreterConsole = { isInteractive: bool; rep: string -> string } with
        member this.run =
            let render, read, renderResult =
                if this.isInteractive then
                    render, readOne, renderResult this.rep
                else
                    renderBasic, readBasic, renderResultBasic this.rep
            
            let mutable state =
                { input = defaultInput
                  history = defaultCommandHistory }
            
            while true do
                render state
                let newState, result = (read () |> updateState state)
                renderResult result
                state <- newState
    
    let createConsole rep isInteractive = { rep = rep; isInteractive = isInteractive }