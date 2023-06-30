module Library.ResultExtensions

type Result<'T, 'Error> with
    member this.traverse f =
        match this with
        | Ok x -> f x |> Result.map Ok
        | Error e -> Error e
        
    static member convertResults (results: Result<'a, 'b> list) : Result<'a list, 'b> =
        let initialAcc = Ok []
        let combineAcc result acc =
            match acc, result with
            | Ok accList, Ok value -> Ok (value :: accList)
            | Error error, _ -> Error error
            | _, Error error -> Error error
    
        List.foldBack combineAcc results initialAcc
