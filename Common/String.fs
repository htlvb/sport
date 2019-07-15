module String

open System
open System.Text.RegularExpressions

let noneIfNullOrWhitespace text =
    if String.IsNullOrWhiteSpace text then None
    else Some text

let removeDuplicateWhitespace text =
    Regex.Replace(text, @"\s+", " ")
