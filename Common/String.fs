module String

open System

let noneIfNullOrWhitespace text =
    if String.IsNullOrWhiteSpace text then None
    else Some text
