module List

let sequenceResultApplicative list =
    let rec fn list acc =
        match list, acc with
        | [], Ok acc -> Ok (List.rev acc)
        | [], Error acc -> Error (List.rev acc)
        | Ok x :: xs, Ok acc -> fn xs (Ok (x :: acc))
        | Ok _x :: xs, Error acc -> fn xs (Error acc)
        | Error x :: xs, Ok _acc -> fn xs (Error [x])
        | Error x :: xs, Error acc -> fn xs (Error (x :: acc))
    fn list (Ok [])
