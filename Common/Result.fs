module Result

let ofOption errorValue = function
    | Some v -> Ok v
    | None -> Error errorValue

let apply v f =
    match f, v with
    | Ok f, Ok v -> Ok (f v)
    | Ok f, Error e -> Error e
    | Error es, Ok v -> Error es
    | Error es, Error e -> Error (es @ e)
