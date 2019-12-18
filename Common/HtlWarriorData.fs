module HtlWarriorData

open Newtonsoft.Json.Linq
open System
#if FABLE_COMPILER
open Thoth.Json
#else
open Thoth.Json.Net
#endif

type Performance = {
    RunTime: TimeSpan
    NumberOfFails: int
    TotalTime: TimeSpan
}

module Performance =
    let decoder : Decoder<_> =
        Decode.object (fun get ->
            {
                RunTime = get.Required.Field "runTime" Decode.timespan
                NumberOfFails = get.Required.Field "numberOfFails" Decode.int
                TotalTime = get.Required.Field "totalTime" Decode.timespan
            }
        )

    let encode v =
        Encode.object [
            "runTime", Encode.timespan v.RunTime
            "numberOfFails", Encode.int v.NumberOfFails
            "totalTime", Encode.timespan v.TotalTime
        ]

    let create runTime numberOfFails totalTime =
        { RunTime = runTime; NumberOfFails = numberOfFails; TotalTime = totalTime }

type StudentPerformance = {
    Student: Student
    Performance: Performance
}

module StudentPerformance =
    let decoder : Decoder<_> =
        Decode.object (fun get ->
            {
                Student = get.Required.Field "student" Student.decoder
                Performance = get.Required.Field "performance" Performance.decoder
            }
        )

    let encode v =
        Encode.object [
            "student", Student.encode v.Student
            "performance", Performance.encode v.Performance
        ]

    let create student performance =
        { Student = student; Performance = performance }

type ClassPerformances = {
    Class: Class
    Performances: StudentPerformance list
}

module ClassPerformances =
    let decoder : Decoder<_> =
        Decode.object (fun get ->
            {
                Class = get.Required.Field "class" Class.decoder
                Performances = get.Required.Field "performances" (Decode.list StudentPerformance.decoder)
            }
        )

    let encode v =
        Encode.object [
            "class", Class.encode v.Class
            "performances", v.Performances |> List.map StudentPerformance.encode |> Encode.list
        ]

    let create schoolClass performances =
        { Class = schoolClass; Performances = performances }

type PerformanceParseError =
    | InvalidRunTime of Cell * value: string
    | InvalidNumberOfFails of Cell * value: string
    | InvalidTotalTime of Cell * value: string

type StudentParseError =
    | EmptyStudentLastName of Cell
    | EmptyStudentFirstName of Cell
    | PerformanceParseErrors of PerformanceParseError list

type ParseError =
    | StudentParseErrors of StudentParseError list

#if !FABLE_COMPILER
module Worksheet =
    let private tryParseStudentPerformance rowIndex (row: JToken) =
        let lastName =
            let col = 1
            row
            |> Seq.tryItem col
            |> Option.map string
            |> Option.bind String.noneIfNullOrWhitespace
            |> Result.ofOption [ EmptyStudentLastName (Cell (rowIndex, col)) ]
        let firstName =
            let col = 2
            row
            |> Seq.tryItem col
            |> Option.map string
            |> Option.bind String.noneIfNullOrWhitespace
            |> Result.ofOption [ EmptyStudentFirstName (Cell (rowIndex, col)) ]
        let student =
            Ok Student.create
            |> Result.apply lastName
            |> Result.apply firstName

        let performance =
            row
            |> Seq.indexed
            |> Seq.skip 3
            |> Seq.take 3
            |> Seq.toArray
            |> fun cols ->
                let runTime =
                    let (colIndex, col) = cols.[0]
                    string col
                    |> tryParseTimespan
                    |> Result.ofOption [ InvalidRunTime (Cell (rowIndex, colIndex), col.ToString()) ]
                    
                let numberOfFails =
                    let (colIndex, col) = cols.[1]
                    string col
                    |> tryParseInt
                    |> Result.ofOption [ InvalidNumberOfFails (Cell (rowIndex, colIndex), col.ToString()) ]

                let totalTime =
                    let (colIndex, col) = cols.[2]
                    string col
                    |> tryParseTimespan
                    |> Result.ofOption [ InvalidTotalTime (Cell (rowIndex, colIndex), col.ToString()) ]

                Ok Performance.create
                |> Result.apply runTime
                |> Result.apply numberOfFails
                |> Result.apply totalTime
            |> Result.mapError (PerformanceParseErrors >> List.singleton)

        Ok StudentPerformance.create
        |> Result.apply student
        |> Result.apply performance
            
    let tryParse (data: JToken) =
        data
        |> Seq.toList
        |> List.indexed
        |> List.skip 1
        |> List.takeWhile (fun (_index, row) -> not <| String.IsNullOrWhiteSpace (string row.[0]))
        |> List.map (uncurry tryParseStudentPerformance)
        |> List.sequenceResultApplicative
        |> Result.mapError (List.collect id >> StudentParseErrors)
#endif
