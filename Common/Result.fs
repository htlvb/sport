module Result

let ofOption errorValue = function
    | Some v -> Ok v
    | None -> Error errorValue

let apply v f =
    match f, v with
    | Ok f, Ok v -> Ok (f v)
    | Ok _f, Error e -> Error e
    | Error es, Ok _v -> Error es
    | Error es, Error e -> Error (es @ e)
