[<AutoOpen>]
module Utils

let uncurry fn (a, b) = fn a b
