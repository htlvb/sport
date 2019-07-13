[<AutoOpen>]
module Utils

let curry fn a b = fn (a, b)
let uncurry fn (a, b) = fn a b
