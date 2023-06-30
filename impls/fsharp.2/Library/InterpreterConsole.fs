namespace Library

open Microsoft.FSharp.Core
open Types

[<RequireQualifiedAccess>]
module InterpreterConsole =
    open System

    type T =
        { isInteractive: bool
          rep: Rep }

    type UserInput =
        { text: string }

        static member defaultValue = { text = "" }

    type CommandHistory =
        { historyPast: string list
          historyFuture: string list }

        static member defaultValue = { historyPast = []; historyFuture = [] }

        /// Add a new item to the start of the historyPast after moving all the
        /// future items to the past.
        static member push (history: CommandHistory) (newCommand: string) =

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

        static member forward(history: CommandHistory) : string option * CommandHistory =
            match (history.historyPast, history.historyFuture) with
            | _, [] -> None, history
            | past, h :: t ->
                Some h,
                { historyFuture = t
                  historyPast = h :: past }

        static member backwards(history: CommandHistory) : string option * CommandHistory =
            match (history.historyPast, history.historyFuture) with
            | [], _ -> None, history
            | h :: t, future ->
                Some h,
                { historyFuture = h :: future
                  historyPast = t }

    /// State tracks the state for the input on the line that the user is entering
    type State =
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

    let updateState (state: State) (action: UserActionType) : State * CommandResult =
        match action with
        | CharacterInput c ->
            { state with
                input = { text = state.input.text + string c } },
            None
        | SubmitCommand ->
            { input = UserInput.defaultValue
              history = CommandHistory.push state.history state.input.text },
            Execute state.input.text
        | DeleteLastChar ->
            { state with
                input =
                    { text =
                        (match state.input.text, state.input.text.Length with
                         | s, 0 -> s
                         | s, sLength -> s[.. sLength - 2]) } },
            None
        | HistoryBack ->
            match CommandHistory.backwards state.history with
            | newInput, newHistory ->
                { input =
                    (match newInput with
                     | Some s -> { text = s }
                     | Option.None -> UserInput.defaultValue)
                  history = newHistory },
                None

        | HistoryForward ->
            match CommandHistory.forward state.history with
            | newInput, newHistory ->
                { input =
                    (match newInput with
                     | Some s -> { text = s }
                     | Option.None -> UserInput.defaultValue)
                  history = newHistory },
                None
        | ClearInput ->
            { state with
                input = UserInput.defaultValue },
            None
        | SubmitCommandBasic s -> state, Execute s

    let renderBasic (_: State) : unit = Console.Write "user> "

    let render (state: State) : unit =
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

    let renderResultBasic (rep: Rep) result =
        match result with
        | Execute txt ->
            rep txt
            |> sprintf "%s\n"
            |> Console.Write
        | None -> ()

    let renderResult (rep: Rep) result =
        match result with
        | Execute txt ->
            rep txt
            |> sprintf "\n%s\n"
            |> Console.Write
        | None -> ()

    let public create rep isInteractive =
        { rep = rep
          isInteractive = isInteractive }

    let public run (console: T) =
        let render, read, renderResult =
            if console.isInteractive then
                render, readOne, renderResult console.rep
            else
                renderBasic, readBasic, renderResultBasic console.rep

        let mutable state =
            { input = UserInput.defaultValue
              history = CommandHistory.defaultValue }

        while true do
            render state
            let newState, result = (read () |> updateState state)
            renderResult result
            state <- newState
