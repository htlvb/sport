module ParseWorksheet

open Newtonsoft.Json.Linq
open System
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

    let create lastName firstName =
        { LastName = lastName; FirstName = firstName }

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

    let create name measurement =
        { Name = name; Measurement = measurement }

type Performance = {
    Discipline: Discipline
    MeasurementValue: float option
    Points: int option
}

module Performance =
    let decoder : Decoder<_> =
        Decode.object (fun get ->
            {
                Discipline = get.Required.Field "discipline" Discipline.decoder
                MeasurementValue = get.Required.Field "measurementValue" (Decode.option Decode.float)
                Points = get.Required.Field "points" (Decode.option Decode.int)
            }
        )

    let encode v =
        Encode.object [
            "discipline", Discipline.encode v.Discipline
            "measurementValue", Encode.option Encode.float v.MeasurementValue
            "points", Encode.option Encode.int v.Points
        ]

    let create discipline measurementValue points =
        { Discipline = discipline; MeasurementValue = measurementValue; Points = points }

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

    let create student performances =
        { Student = student; Performances = performances }

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

    let create schoolClass performances =
        { Class = schoolClass; Performances = performances }

type Cell = Cell of row: int * column: int

type DisciplineParseError =
    | DisciplineNotFound of Cell
    | MeasurementNotFound of Cell
    | InvalidMeasurementValue of Cell * value: string
    | InvalidPoints of Cell * value: string

type StudentParseError =
    | EmptyStudentLastName of Cell
    | EmptyStudentFirstName of Cell
    | DisciplineParseErrors of DisciplineParseError list

type ParseError =
    | NotEnoughRows
    | StudentParseErrors of StudentParseError list

#if !FABLE_COMPILER
module Worksheet =
    let private tryParseStudentPerformance disciplines header rowIndex (row: JToken) =
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

        let performances =
            row
            |> Seq.indexed
            |> Seq.skip 3
            |> Seq.chunkBySize 2
            |> Seq.filter (fun cols -> cols.Length = 2)
            |> Seq.map (fun cols -> (cols.[0], cols.[1]))
            |> Seq.takeWhile (fun ((col1, _result), (_col2, _points)) ->
                disciplines
                |> Seq.tryItem col1
                |> Option.map string
                |> Option.bind String.noneIfNullOrWhitespace
                |> Option.isSome
            )
            |> Seq.map (fun ((col1, measurement), (col2, points)) ->
                let parsedDisciplineName =
                    disciplines
                    |> Seq.tryItem col1
                    |> Option.map string
                    |> Option.bind String.noneIfNullOrWhitespace
                    |> Result.ofOption [ DisciplineNotFound (Cell (rowIndex, col1)) ]

                let parsedMeasurementName =
                    header
                    |> Seq.tryItem col1
                    |> Option.map string
                    |> Option.bind String.noneIfNullOrWhitespace
                    |> Result.ofOption [ MeasurementNotFound (Cell (rowIndex, col1)) ]

                let parsedMeasurement =
                    match String.noneIfNullOrWhitespace (string measurement) with
                    | None -> Ok None
                    | Some v ->
                        tryParseFloat v
                        |> Result.ofOption [ InvalidMeasurementValue (Cell (rowIndex, col1), measurement.ToString()) ]
                        |> Result.map Some

                let parsedPoints =
                    match String.noneIfNullOrWhitespace (string points) with
                    | None -> Ok None
                    | Some v ->
                        tryParseInt v
                        |> Result.ofOption [ InvalidPoints (Cell (rowIndex, col2), points.ToString()) ]
                        |> Result.map Some

                let parsedDiscipline =
                    Ok Discipline.create
                    |> Result.apply parsedDisciplineName
                    |> Result.apply parsedMeasurementName

                Ok Performance.create
                |> Result.apply parsedDiscipline
                |> Result.apply parsedMeasurement
                |> Result.apply parsedPoints
            )
            |> Seq.toList
            |> List.sequenceResultApplicative
            |> Result.mapError (List.collect id >> DisciplineParseErrors >> List.singleton)

        Ok StudentPerformances.create
        |> Result.apply student
        |> Result.apply performances
            
    let tryParse (data: JToken) =
        match data |> Seq.indexed |> Seq.toList with
        | (_, disciplines) :: (_, header) :: values ->
            values
            |> List.takeWhile (fun (_index, row) -> not <| String.IsNullOrWhiteSpace (string row.[0]))
            |> List.map (uncurry (tryParseStudentPerformance disciplines header))
            |> List.sequenceResultApplicative
            |> Result.mapError (List.collect id >> StudentParseErrors)
        | _ -> Error NotEnoughRows
#endif
