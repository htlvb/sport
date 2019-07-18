module Achtkampf

open Elmish.Streams
open Fable.FontAwesome
open FSharp.Control
open Fulma
open ParseWorksheet
open Thoth.Fetch
open Thoth.Json

type FetchError =
    | HttpError of exn
    | DecodeError of string

type Group =
    | All
    | Level of int
    | Class of Class

type CalculatedStudentPerformances = {
    Class: Class
    Student: Student
    Performances: Performance list
    TotalPoints: int
}

module CalculatedStudentPerformances =
    let fromData (data: ClassPerformances list) =
        data
        |> List.collect (fun classPerformances ->
            classPerformances.Performances
            |> List.map (fun studentPerformances ->
                {
                    Class = classPerformances.Class
                    Student = studentPerformances.Student
                    Performances = studentPerformances.Performances
                    TotalPoints = List.sumBy (fun v -> v.Points |> Option.defaultValue 0) studentPerformances.Performances
                }
            )
        )

type Msg =
    | LoadAchtkampfResponse of Result<ClassPerformances list, FetchError>
    | SelectGroup of Group
    | AddStudentToComparison of CalculatedStudentPerformances
    | RemoveStudentFromComparison of CalculatedStudentPerformances

module Group =
    let toString = function
        | All -> "Gesamt"
        | Level level -> sprintf "%d. Klassen" level
        | Class schoolClass -> Class.toString schoolClass

    let filter group data =
        match group with
        | All ->
            data
            |> List.sortByDescending (fun studentPerformances -> studentPerformances.TotalPoints)
            |> List.takeWhile (fun studentPerformances -> studentPerformances.TotalPoints > 0)
            |> List.truncate 40
        | Level level ->
            data
            |> List.filter (fun studentPerformances -> studentPerformances.Class.Level = level)
            |> List.sortByDescending (fun studentPerformances -> studentPerformances.TotalPoints)
            |> List.takeWhile (fun studentPerformances -> studentPerformances.TotalPoints > 0)
            |> List.truncate 40
        | Class schoolClass ->
            data
            |> List.filter (fun classPerformances -> classPerformances.Class = schoolClass)
            |> List.sortBy (fun studentPerformances -> studentPerformances.Student.LastName, studentPerformances.Student.FirstName)

type LoadedModel = {
    Data: ClassPerformances list
    SelectedGroup: Group
    StudentsToCompare: CalculatedStudentPerformances list
}

type Model =
    | NotLoaded
    | Loaded of LoadedModel
    | LoadError of FetchError

let init = NotLoaded

let update msg model =
    match model, msg with
    | _, LoadAchtkampfResponse (Ok classPerformances) ->
        Loaded { Data = classPerformances; SelectedGroup = All; StudentsToCompare = [] }
    | _, LoadAchtkampfResponse (Error error) ->
        LoadError error
    | Loaded model, SelectGroup group ->
        Loaded { model with SelectedGroup = group }
    | _, SelectGroup _ -> model
    | Loaded model, AddStudentToComparison student ->
        Loaded { model with StudentsToCompare = model.StudentsToCompare @ [ student ] }
    | _, AddStudentToComparison _ -> model
    | Loaded model, RemoveStudentFromComparison student ->
        Loaded { model with StudentsToCompare = model.StudentsToCompare |> List.except [ student ] }
    | _, RemoveStudentFromComparison _ -> model

open Fable.React
open Fable.React.Props

let view model dispatch =
    match model with
    | NotLoaded ->
        [
            Container.container [ Container.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ] [
                Fa.i [ Fa.Solid.Cog; Fa.Spin; Fa.Size Fa.Fa5x ] []
            ]
        ]
    | Loaded model ->
        let data =
            CalculatedStudentPerformances.fromData model.Data
            |> Group.filter model.SelectedGroup
        let disciplines =
            data
            |> List.collect (fun studentPerformances -> studentPerformances.Performances)
            |> List.map (fun performance -> performance.Discipline)
            |> List.distinct
        let groups =
            [
                yield Group.All

                yield!
                    model.Data
                    |> List.map (fun v -> v.Class.Level)
                    |> List.distinct
                    |> List.sort
                    |> List.map Level

                yield!
                    model.Data
                    |> List.map (fun v -> v.Class)
                    |> List.distinct
                    |> List.sortBy (fun v -> (v.Level, v.ParallelClass, v.Type, v.Department))
                    |> List.map Group.Class
            ]
        let tableHeader =
            [
                tr [] [
                    yield th [ ColSpan 4 ] []
                    for discipline in disciplines ->
                        th [ ColSpan 2 ] [ str discipline.Name ]
                ]
                tr [] [
                    yield th [] [ Fa.i [ Fa.Solid.Hashtag ] [] ]
                    yield th [] [ Fa.i [ Fa.Solid.Users ] [] ]
                    yield th [] [ Fa.i [ Fa.Solid.User ] [] ]
                    yield th [] [ Fa.i [ Fa.Solid.Trophy; Fa.Props [ Title "Gesamtpunkte" ] ] [] ]
                    for discipline in disciplines do
                        yield th [] [ str discipline.Measurement ]
                        yield th [] [ Fa.i [ Fa.Solid.Trophy; Fa.Props [ Title "Punkte" ] ] [] ]
                ]
            ]
        let studentPerformanceRow studentPerformances =
            [
                yield td [] [ str (sprintf "%d" studentPerformances.TotalPoints) ]
                for performance in studentPerformances.Performances do
                    yield td [] [ str (performance.MeasurementValue |> Option.map (sprintf "%g") |> Option.defaultValue "") ]
                    yield td [] [ str (performance.Points |> Option.map (sprintf "%d") |> Option.defaultValue "") ]
            ]
        [
            yield Section.section [] [
                Container.container [] [
                    Button.list [ Button.List.IsCentered ] [
                        for group in groups ->
                            Button.button
                                [
                                    Button.OnClick (fun _ev -> dispatch (SelectGroup group))
                                    Button.Color (if group = model.SelectedGroup then IsDark else NoColor)
                                ]
                                [ str (Group.toString group) ]
                    ]
                ]
            ]

            if List.length model.StudentsToCompare > 1 then
                yield Section.section [] [
                    Container.container [] [
                        Heading.h2 [] [ str "Vergleich " ]
                        Table.table [ Table.IsBordered; Table.IsFullWidth ] [
                            thead [] tableHeader
                            tbody [] [
                                let studentRow i studentPerformances =
                                    tr [] [
                                        yield td [] [ str (sprintf "%d" (i + 1)) ]
                                        yield td [] [ str (Class.toString studentPerformances.Class) ]
                                        yield td [] [
                                            div [ Style [ Display DisplayOptions.Flex; JustifyContent "space-between"; AlignItems AlignItemsOptions.Center ] ] [
                                                span [] [ str (Student.fullName studentPerformances.Student) ]
                                                Delete.delete
                                                    [
                                                        Delete.Props [ Title "Vom Vergleich entfernen"; Style [ MarginLeft "10px" ] ]
                                                        Delete.OnClick (fun _ev -> dispatch (RemoveStudentFromComparison studentPerformances))
                                                    ]
                                                    []
                                            ]
                                        ]
                                        yield! studentPerformanceRow studentPerformances
                                    ]
                                let diffColPoints v1 v2 =
                                    let className =
                                        if v1 > v2 then "has-background-success"
                                        elif v1 = v2 then "has-background-warning"
                                        else "has-background-danger"
                                    td [ Class className ] [ str (sprintf "%+d" (v1 - v2)) ]

                                let diffColsPerformance p1 p2 =
                                    let p1Points = Option.defaultValue 0 p1.Points
                                    let p2Points = Option.defaultValue 0 p2.Points
                                    let p1Value = Option.defaultValue 0. p1.MeasurementValue
                                    let p2Value = Option.defaultValue 0. p2.MeasurementValue

                                    let className =
                                        if p1Points > p2Points then "has-background-success"
                                        elif p1Points = p2Points then "has-background-warning"
                                        else "has-background-danger"
                                    let (sign, diff) =
                                        let r = System.Math.Round(p1Value - p2Value, 5)
                                        if r < 0. then ("", r)
                                        else ("+", r)
                                    [
                                        yield td [ Class className ] [ str (sprintf "%s%g" sign diff) ]
                                        yield diffColPoints p1Points p2Points
                                    ]

                                let diffRow s1 s2 =
                                    tr [] [
                                        yield td [ ColSpan 3 ] []
                                        yield diffColPoints s1.TotalPoints s2.TotalPoints
                                        yield!
                                            List.zip s1.Performances s2.Performances
                                            |> List.collect (uncurry diffColsPerformance)
                                    ]

                                for (i1, s1), (i2, s2) in model.StudentsToCompare |> List.indexed |> List.pairwise do
                                    if i1 = 0 then
                                        yield studentRow i1 s1
                                    yield diffRow s1 s2
                                    yield studentRow i2 s2
                            ]
                        ]
                    ]
                ]

            yield Section.section [] [
                Container.container [] [
                    Heading.h2 [] [ str (sprintf "Achtkampf %s" (Group.toString model.SelectedGroup)) ]
                    Table.table [ Table.IsHoverable; Table.IsBordered; Table.IsStriped; Table.IsFullWidth ] [
                        thead [] tableHeader
                        tfoot [] (List.rev tableHeader)
                        tbody [] [
                            for (i, studentPerformances) in List.indexed data ->
                                tr
                                    [
                                        if List.contains studentPerformances model.StudentsToCompare then
                                            yield OnClick (fun _ev -> dispatch (RemoveStudentFromComparison studentPerformances))
                                            yield ClassName "is-selected"
                                        else
                                            yield OnClick (fun _ev -> dispatch (AddStudentToComparison studentPerformances))
                                    ]
                                    [
                                        yield td [] [ str (sprintf "%d" (i + 1)) ]
                                        yield td [] [ str (Class.toString studentPerformances.Class) ]
                                        yield td [] [ str (Student.fullName studentPerformances.Student) ]
                                        yield! studentPerformanceRow studentPerformances
                                    ]
                        ]
                    ]
                ]
            ]
        ]
    | LoadError (HttpError e) ->
        []
    | LoadError (DecodeError e) ->
        []

let stream states msgs =
    let loadAchtkampfData =
        AsyncRx.ofPromise (promise {
            let! response = Fetch.tryFetchAs ("/api/achtkampf/data.json", Decode.list ClassPerformances.decoder)
            return response |> Result.mapError DecodeError
        })
        |> AsyncRx.catch (HttpError >> Error >> AsyncRx.single)

    [
        msgs

        states
        |> AsyncRx.flatMapLatest (snd >> function | NotLoaded -> loadAchtkampfData | _ -> AsyncRx.empty ())
        |> AsyncRx.map LoadAchtkampfResponse
    ]
    |> AsyncRx.mergeSeq
