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
}

type Model =
    | NotLoaded
    | Loaded of LoadedModel
    | LoadError of FetchError

let init = NotLoaded

let update msg model =
    match model, msg with
    | _, LoadAchtkampfResponse (Ok classPerformances) ->
        Loaded { Data = classPerformances; SelectedGroup = All }
    | _, LoadAchtkampfResponse (Error error) ->
        LoadError error
    | Loaded model, SelectGroup group ->
        Loaded { model with SelectedGroup = group }
    | _, SelectGroup _ -> model

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
        [
            Section.section [] [
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

            Section.section [] [
                Container.container [] [
                    Heading.h1 [] [ str (sprintf "Achtkampf %s" (Group.toString model.SelectedGroup)) ]
                    Table.table [ Table.IsHoverable; Table.IsBordered; Table.IsStriped; Table.IsFullWidth ] [
                        thead [] [
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
                        tbody [] [
                            for (i, studentPerformances) in List.indexed data ->
                                tr [] [
                                    yield td [] [ str (sprintf "%d" (i + 1)) ]
                                    yield td [] [ str (Class.toString studentPerformances.Class) ]
                                    yield td [] [ str (sprintf "%s %s" (studentPerformances.Student.LastName.ToUpper()) studentPerformances.Student.FirstName) ]
                                    yield td [] [ str (sprintf "%d" studentPerformances.TotalPoints) ]
                                    for performance in studentPerformances.Performances do
                                        yield td [] [ str (performance.MeasurementValue |> Option.map (sprintf "%g") |> Option.defaultValue "") ]
                                        yield td [] [ str (performance.Points |> Option.map (sprintf "%d") |> Option.defaultValue "") ]
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
