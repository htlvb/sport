namespace global

open System.Text.RegularExpressions
#if FABLE_COMPILER
open Thoth.Json
#else
open Thoth.Json.Net
#endif

type Class = {
    Level: int
    ParallelClass: string
    Type: string
    Department: string
}

module Class =
    let tryParse text =
        let m = Regex.Match(text, @"^(\d+)(\w)(\w)(\w+)$")
        if m.Success then
            Some {
                Level = int m.Groups.[1].Value
                ParallelClass = m.Groups.[2].Value
                Type = m.Groups.[3].Value
                Department = m.Groups.[4].Value
            }
        else None

    let toString v =
        sprintf "%d%s%s%s" v.Level v.ParallelClass v.Type v.Department

    let decoder : Decoder<_> =
        Decode.string |> Decode.andThen (fun text ->
            match tryParse text with
            | Some result -> Decode.succeed result
            | None -> Decode.fail (sprintf "Can't decode \"%s\" as class" text))

    let encode v =
        toString v
        |> Encode.string

type Student = {
    LastName: string
    FirstName: string
}

module Student =
    let decoder : Decoder<_> =
        Decode.object (fun get ->
            {
                LastName = get.Required.Field "lastName" Decode.string
                FirstName = get.Required.Field "firstName" Decode.string
            }
        )

    let encode v =
        Encode.object [
            "lastName", Encode.string v.LastName
            "firstName", Encode.string v.FirstName
        ]

    let create lastName firstName =
        { LastName = lastName; FirstName = firstName }

    let fullName student =
        sprintf "%s %s" (student.LastName.ToUpper()) student.FirstName

type Cell = Cell of row: int * column: int
