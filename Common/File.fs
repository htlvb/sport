module File

open System.IO

let writeAllText path text =
    Path.GetDirectoryName path |> Directory.CreateDirectory |> ignore
    File.WriteAllText (path, text)
