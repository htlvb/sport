module ParseWorksheet

open Newtonsoft.Json.Linq
open System
open System.Text.RegularExpressions

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

type Discipline = {
    Name: string
    Measurement: string
}

type Performance = {
    Discipline: Discipline
    Value: float
    Points: int
}

type StudentPerformances = {
    Student: Student
    Performances: Performance list
}

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
                                let (col2, points) = cols.[1]
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
