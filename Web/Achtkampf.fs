module Achtkampf

open Elmish.Streams
open Fable.FontAwesome
open FSharp.Control
open Fulma
open AchtkampfData
open Thoth.Fetch
open Thoth.Json

type FetchError =
    | HttpError of exn
    | DecodeError of string

type Group =
    | All
    | Discipline of Discipline

type RankType =
    | Normal
    | SameAsBefore

type Rank = {
    Value: int
    Type: RankType
}

module Rank =
    let toString rank =
        match rank.Type with
        | Normal -> sprintf "%d" rank.Value
        | SameAsBefore -> ""

type CalculatedStudentPerformances = {
    TotalRank: Rank
    Class: Class
    Student: Student
    Performances: Performance list
    TotalPoints: int
}

let sortAndAddRanks pointsFn list =
    let rec fn acc list =
        match list with
        | [] -> List.rev acc
        | x :: xs ->
            let currentRank =
                match List.tryHead acc with
                | None ->
                    { Value = 1; Type = Normal }
                | Some (previousRank, previousItem) when pointsFn previousItem = pointsFn x ->
                    { Value = previousRank.Value; Type = SameAsBefore }
                | Some _ ->
                    { Value = List.length acc + 1; Type = Normal}
            fn ((currentRank, x) :: acc) xs
    list
    |> List.sortByDescending pointsFn
    |> fn []

module CalculatedStudentPerformances =
    let fromData (data: ClassPerformances list) =
        data
        |> List.collect (fun classPerformances ->
            classPerformances.Performances
            |> List.map (fun studentPerformances ->
                let totalPoints =
                    studentPerformances.Performances
                    |> List.sumBy (fun v -> v.Points |> Option.defaultValue 0)
                (totalPoints, classPerformances.Class, studentPerformances)
            )
        )
        |> sortAndAddRanks (fun (totalPoints, _, _) -> totalPoints)
        |> List.map (fun (totalRank, (totalPoints, schoolClass, studentPerformances)) ->
            {
                TotalRank = totalRank
                Class = schoolClass
                Student = studentPerformances.Student
                Performances = studentPerformances.Performances
                TotalPoints = totalPoints
            }
        )

type Msg =
    | LoadAchtkampfResponse of Result<ClassPerformances list, FetchError>
    | SelectGroup of Group
    | AddStudentToComparison of CalculatedStudentPerformances
    | RemoveStudentFromComparison of CalculatedStudentPerformances
    | ResetComparison

module Group =
    let toString = function
        | All -> "Gesamtwertung"
        | Discipline discipline -> discipline.Name

type LoadedModel = {
    Data: CalculatedStudentPerformances list
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
        Loaded {
            Data = CalculatedStudentPerformances.fromData classPerformances
            SelectedGroup = All
            StudentsToCompare = []
        }
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
    | Loaded model, ResetComparison ->
        Loaded { model with StudentsToCompare = [] }
    | _, ResetComparison _ -> model

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
        let disciplines =
            model.Data
            |> List.collect (fun studentPerformances -> studentPerformances.Performances)
            |> List.map (fun performance -> performance.Discipline)
            |> List.distinct
        let groups =
            [
                yield Group.All
                yield! List.map Discipline disciplines
            ]
        let performanceCells performance =
            [
                td [] [ str (performance.MeasurementValue |> Option.map (sprintf "%g") |> Option.defaultValue "") ]
                td [] [ str (performance.Points |> Option.map (sprintf "%d") |> Option.defaultValue "") ]
            ]
        let studentPerformanceRow studentPerformances =
            [
                yield td [] [ str (sprintf "%d" studentPerformances.TotalPoints) ]
                for performance in studentPerformances.Performances do
                    yield! performanceCells performance
            ]
        let scrollOnOverflow elem =
            div [ Style [ OverflowX "auto" ] ] [ elem ]
        let fullTableHeader =
            [
                tr [] [
                    yield th [ ColSpan 4 ] []
                    for discipline in disciplines ->
                        th [ ColSpan 2 ] [ str discipline.Name ]
                ]
                tr [] [
                    yield th [ Title "Rang" ] [ Fa.i [ Fa.Solid.Medal ] [] ]
                    yield th [ Title "Klasse" ] [ Fa.i [ Fa.Solid.Users ] [] ]
                    yield th [ Title "Name" ] [ Fa.i [ Fa.Solid.User ] [] ]
                    yield th [ Title "Gesamtpunkte" ] [ Fa.i [ Fa.Solid.Poll ] []; sub [] [ str "Σ" ] ]
                    for discipline in disciplines do
                        yield th [] [ str discipline.Measurement ]
                        yield th [ Title "Punkte" ] [ Fa.i [ Fa.Solid.Poll ] [] ]
                ]
            ]

        let comparisonTable =
            if List.length model.StudentsToCompare > 1 then
                Table.table [ Table.IsBordered; Table.IsFullWidth ] [
                    thead [] fullTableHeader
                    tbody [] [
                        let studentRow studentPerformances =
                            tr [] [
                                yield td [] [ str (sprintf "%d" studentPerformances.TotalRank.Value) ]
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

                        for (i, (s1, s2)) in model.StudentsToCompare |> List.pairwise |> List.indexed do
                            if i = 0 then
                                yield studentRow s1
                            yield diffRow s1 s2
                            yield studentRow s2
                    ]
                ]
                |> scrollOnOverflow
                |> Some
            else None

        let dataTable =
            match model.SelectedGroup with
            | Group.All ->
                Table.table [ Table.IsHoverable; Table.IsBordered; Table.IsStriped; Table.IsFullWidth ] [
                    thead [] fullTableHeader
                    tfoot [] (List.rev fullTableHeader)
                    tbody [] [
                            let data =
                                model.Data
                                |> List.takeWhile (fun studentPerformances -> studentPerformances.TotalRank.Value <= 40 && studentPerformances.TotalPoints > 0)

                            for studentPerformances in data ->
                                tr
                                    [
                                        if List.contains studentPerformances model.StudentsToCompare then
                                            yield OnClick (fun _ev -> dispatch (RemoveStudentFromComparison studentPerformances))
                                            yield ClassName "is-selected"
                                        else
                                            yield OnClick (fun _ev -> dispatch (AddStudentToComparison studentPerformances))
                                    ]
                                    [
                                        yield td [] [ str (Rank.toString studentPerformances.TotalRank) ]
                                        yield td [] [ str (Class.toString studentPerformances.Class) ]
                                        yield td [] [ str (Student.fullName studentPerformances.Student) ]
                                        yield! studentPerformanceRow studentPerformances
                                    ]
                    ]
                ]
                |> scrollOnOverflow
            | Discipline discipline ->
                let tableHeader =
                    tr [] [
                        yield th [ Title "Rang" ] [ Fa.i [ Fa.Solid.Medal ] [] ]
                        yield th [ Title "Gesamtrang" ] [ Fa.i [ Fa.Solid.Medal ] [ ]; sub [] [ str "Σ" ] ]
                        yield th [ Title "Klasse" ] [ Fa.i [ Fa.Solid.Users ] [] ]
                        yield th [ Title "Name" ] [ Fa.i [ Fa.Solid.User ] [] ]
                        yield th [] [ str discipline.Measurement ]
                        yield th [ Title "Punkte" ] [ Fa.i [ Fa.Solid.Poll ] [] ]
                    ]
                Table.table [ Table.IsHoverable; Table.IsBordered; Table.IsStriped ] [
                    thead [] [ tableHeader ]
                    tfoot [] [ tableHeader ]
                    tbody [] [
                        let data =
                            model.Data
                            |> List.choose (fun studentPerformances ->
                                let disciplinePerformance =
                                    studentPerformances.Performances
                                    |> List.tryFind (fun p -> p.Discipline = discipline)
                                match disciplinePerformance with
                                | Some ({ Points = Some points } as performance) when points > 0 ->
                                    Some (performance, studentPerformances)
                                | _ -> None
                            )
                            |> sortAndAddRanks (fun (disciplinePerformance, _) -> disciplinePerformance.Points)
                            |> List.map (fun (disciplineRank, (disciplinePerformance, studentPerformances)) ->
                                (disciplineRank, disciplinePerformance, studentPerformances)
                            )
                            |> List.takeWhile (fun (disciplineRank, _, _) -> disciplineRank.Value <= 40)
                        for (disciplineRank, disciplinePerformance, studentPerformances) in data ->
                            tr
                                [
                                    if List.contains studentPerformances model.StudentsToCompare then
                                        yield OnClick (fun _ev -> dispatch (RemoveStudentFromComparison studentPerformances))
                                        yield ClassName "is-selected"
                                    else
                                        yield OnClick (fun _ev -> dispatch (AddStudentToComparison studentPerformances))
                                ]
                                [
                                    yield td [] [ str (Rank.toString disciplineRank) ]
                                    yield td [] [ str (sprintf "%d" studentPerformances.TotalRank.Value) ]
                                    yield td [] [ str (Class.toString studentPerformances.Class) ]
                                    yield td [] [ str (Student.fullName studentPerformances.Student) ]
                                    yield! performanceCells disciplinePerformance
                                ]
                    ]
                ]
                |> scrollOnOverflow

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

            match comparisonTable with
            | Some comparisonTable ->
                yield Section.section [] [
                    Container.container [] [
                        Heading.h2 [] [
                            str "Vergleich"
                            Delete.delete
                                [
                                    Delete.Props [ Title "Vergleich entfernen"; Style [ MarginLeft "10px"; VerticalAlign "middle" ] ]
                                    Delete.OnClick (fun _ev -> dispatch ResetComparison)
                                ]
                                []
                        ]
                        comparisonTable
                    ]
                ]
            | None -> ()

            yield Section.section [] [
                Container.container [] [
                    Heading.h2 [] [ str (sprintf "Achtkampf － %s" (Group.toString model.SelectedGroup)) ]
                    Notification.notification [] [
                        Fa.i [ Fa.Solid.InfoCircle ] []
                        span [ Style [ MarginLeft "10px" ] ] [ str "Zum Vergleichen zwei oder mehrere Personen auswählen." ]
                    ]
                    dataTable
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
            let! response = Fetch.tryFetchAs ("api/achtkampf/data.json", Decode.list ClassPerformances.decoder)
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
