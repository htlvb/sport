module ParseWorksheet

open Newtonsoft.Json.Linq
open System
open System.Text.RegularExpressions
open Thoth.Json.Net

type Class = {
    Level: int
    ParallelClass: string
    Type: string
    Department: string
}

module Class =
    let tryParse text =
        let m = Regex.Match(text, @"^(?<level>\d+)(?<parallelClass>\w)(?<type>\w)(?<department>\w+)$")
        if m.Success then
            Some {
                Level = int m.Groups.["level"].Value
                ParallelClass = m.Groups.["parallelClass"].Value
                Type = m.Groups.["type"].Value
                Department = m.Groups.["department"].Value
            }
        else None

    let decoder : Decoder<_> =
        Decode.string |> Decode.andThen (fun text ->
            match tryParse text with
            | Some result -> Decode.succeed result
            | None -> Decode.fail (sprintf "Can't decode \"%s\" as class" text))

    let encode v =
        sprintf "%d%s%s%s" v.Level v.ParallelClass v.Type v.Department
        |> Encode.string

let tryParseFloat text =
    match System.Double.TryParse text with
    | (true, value) -> Some value
    | _ -> None

let tryParseInt text =
    match System.Int32.TryParse text with
    | (true, value) -> Some value
    | _ -> None


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

type Discipline = {
    Name: string
    Measurement: string
}

module Discipline =
    let decoder : Decoder<_> =
        Decode.object (fun get ->
            {
                Name = get.Required.Field "name" Decode.string
                Measurement = get.Required.Field "measurement" Decode.string
            }
        )

    let encode v =
        Encode.object [
            "name", Encode.string v.Name
            "measurement", Encode.string v.Measurement
        ]

type Performance = {
    Discipline: Discipline
    Value: float
    Points: int
}

module Performance =
    let decoder : Decoder<_> =
        Decode.object (fun get ->
            {
                Discipline = get.Required.Field "discipline" Discipline.decoder
                Value = get.Required.Field "value" Decode.float
                Points = get.Required.Field "points" Decode.int
            }
        )

    let encode v =
        Encode.object [
            "discipline", Discipline.encode v.Discipline
            "value", Encode.float v.Value
            "points", Encode.int v.Points
        ]

type StudentPerformances = {
    Student: Student
    Performances: Performance list
}

module StudentPerformances =
    let decoder : Decoder<_> =
        Decode.object (fun get ->
            {
                Student = get.Required.Field "student" Student.decoder
                Performances = get.Required.Field "performances" (Decode.list Performance.decoder)
            }
        )

    let encode v =
        Encode.object [
            "student", Student.encode v.Student
            "performances", v.Performances |> List.map Performance.encode |> Encode.list
        ]

type ClassPerformances = {
    Class: Class
    Performances: StudentPerformances list
}

module ClassPerformances =
    let decoder : Decoder<_> =
        Decode.object (fun get ->
            {
                Class = get.Required.Field "class" Class.decoder
                Performances = get.Required.Field "performances" (Decode.list StudentPerformances.decoder)
            }
        )

    let encode v =
        Encode.object [
            "class", Class.encode v.Class
            "performances", v.Performances |> List.map StudentPerformances.encode |> Encode.list
        ]

type ParseError =
    | NotEnoughRows

module Worksheet =
    let tryParse (data: JToken) =
        match Seq.toList data with
        | disciplines :: header :: values ->
            values
            |> List.takeWhile (fun row -> not <| String.IsNullOrWhiteSpace (string row.[0]))
            |> List.choose (fun row ->
                let student = { LastName = string row.[1]; FirstName = string row.[2] }
                if String.IsNullOrWhiteSpace student.LastName then None
                else
                    {
                        Student = student
                        Performances =
                            row
                            |> Seq.indexed
                            |> Seq.skip 3
                            |> Seq.chunkBySize 2
                            |> Seq.filter (fun cols -> cols.Length = 2)
                            |> Seq.map (fun cols ->
                                let (col1, result) = cols.[0]
                                let (_col2, points) = cols.[1]
                                let discipline = { Name = string disciplines.[col1]; Measurement = string header.[col1] }
                                if not<| String.IsNullOrWhiteSpace discipline.Name then
                                    {
                                        Discipline = discipline
                                        Value = string result |> tryParseFloat |> Option.defaultValue 0.
                                        Points = string points |> tryParseInt |> Option.defaultValue 0
                                    }
                                    |> Some
                                else None
                            )
                            |> Seq.takeWhile Option.isSome
                            |> Seq.choose id
                            |> Seq.toList
                    }
                    |> Some
            )
            |> Ok
        | _ -> Error NotEnoughRows
