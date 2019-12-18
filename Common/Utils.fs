[<AutoOpen>]
module Utils

let curry fn a b = fn (a, b)
let uncurry fn (a, b) = fn a b

let tryDo fn arg =
    match fn arg with
    | (true, value) -> Some value
    | _ -> None

let tryParseFloat = tryDo System.Double.TryParse
let tryParseInt = tryDo System.Int32.TryParse
let tryParseTimespan = tryDo (fun v -> System.TimeSpan.TryParseExact(v, "mm\\:ss\\:FF", null) )
