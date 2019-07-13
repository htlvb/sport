module Path

open System.IO

let combine paths path =
    path :: paths
    |> List.toArray
    |> Path.Combine
