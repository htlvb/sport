module HtlWarrior

open Elmish.Streams
open Fable.FontAwesome
open FSharp.Control
open Fulma
open HtlWarriorData
open Thoth.Fetch
open Thoth.Json

type FetchError =
    | HttpError of exn
    | DecodeError of string

type Group =
    | StudentGroup
    | ClassGroup

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

type GroupItem =
    | StudentGroupItem of Student * Class
    | ClassGroupItem of Class

type CalculatedPerformance = {
    TotalRank: Rank
    GroupItem: GroupItem
    Performance: Performance
    TotalTime: System.TimeSpan
}

type CalculatedClassPerformances = {
    TotalRank: Rank
    Class: Class
    Student: Student
    Performances: Performance list
    TotalTime: System.TimeSpan
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

module CalculatedStudentPerformance =
    let fromData (data: ClassPerformances list) =
        data
        |> List.collect (fun classPerformances ->
            classPerformances.Performances
            |> List.map (fun studentPerformance ->
                let totalTime = studentPerformance.Performance.TotalTime
                (totalTime, classPerformances.Class, studentPerformance)
            )
        )
        |> sortAndAddRanks (fun (totalTime, _, _) -> -totalTime.Ticks)
        |> List.map (fun (totalRank, (totalTime, schoolClass, studentPerformance)) ->
            {
                TotalRank = totalRank
                Class = schoolClass
                Student = studentPerformance.Student
                Performance = studentPerformance.Performance
                TotalTime = totalTime
            }
        )

type Msg =
    | LoadDataResponse of Result<ClassPerformances list, FetchError>
    | SelectGroup of Group
    | AddStudentToComparison of CalculatedStudentPerformance
    | RemoveStudentFromComparison of CalculatedStudentPerformance
    | AddClassToComparison of CalculatedClassPerformances
    | RemoveClassFromComparison of CalculatedClassPerformances
    | ResetComparison

module Group =
    let toString = function
        | Student -> "Einzelwertung"
        | Class -> "Klassenwertung"

type LoadedModel = {
    Data: CalculatedStudentPerformance list
    SelectedGroup: Group
    StudentsToCompare: CalculatedStudentPerformance list
    ClassesToCompare: CalculatedClassPerformances list
}

type Model =
    | NotLoaded
    | Loaded of LoadedModel
    | LoadError of FetchError

let init = NotLoaded

let update msg model =
    match model, msg with
    | _, LoadDataResponse (Ok classPerformances) ->
        Loaded {
            Data = CalculatedStudentPerformance.fromData classPerformances
            SelectedGroup = Student
            StudentsToCompare = []
            ClassesToCompare = []
        }
    | _, LoadDataResponse (Error error) ->
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
    | Loaded model, AddClassToComparison ``class`` ->
        Loaded { model with ClassesToCompare = model.ClassesToCompare @ [ ``class`` ] }
    | _, AddClassToComparison _ -> model
    | Loaded model, RemoveClassFromComparison ``class`` ->
        Loaded { model with ClassesToCompare = model.ClassesToCompare |> List.except [ ``class`` ] }
    | _, RemoveClassFromComparison _ -> model
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
        let groups =
            [
                Student
                Group.Class
            ]
        let performanceCells performance =
            [
                td [] [ str (performance.RunTime.ToString("mm:ss.FF")) ]
                td [] [ str (string performance.NumberOfFails) ]
            ]
        let studentPerformanceRow (studentPerformance: CalculatedStudentPerformance) =
            [
                yield! performanceCells studentPerformance.Performance
                td [] [ str (studentPerformance.TotalTime.ToString("mm:ss.FF")) ]
            ]
        let scrollOnOverflow elem =
            div [ Style [ OverflowX "auto" ] ] [ elem ]
        let fullTableHeader =
            [
                tr [] [
                    th [ Title "Rang" ] [ Fa.i [ Fa.Solid.Medal ] [] ]
                    th [ Title "Klasse" ] [ Fa.i [ Fa.Solid.Users ] [] ]
                    th [ Title "Name" ] [ Fa.i [ Fa.Solid.User ] [] ]
                    th [ Title "Laufzeit" ] [ Fa.i [ Fa.Solid.Stopwatch ] [] ]
                    th [ Title "Anzahl Fails" ] [ Fa.i [ Fa.Solid.Times ] [] ]
                    th [ Title "Gesamtzeit" ] [ Fa.i [ Fa.Solid.Stopwatch ] []; sub [] [ str "Σ" ] ]
                ]
            ]

        let comparisonTable =
            if List.length model.StudentsToCompare > 1 then
                Table.table [ Table.IsBordered; Table.IsFullWidth ] [
                    thead [] fullTableHeader
                    tbody [] [
                        let studentRow (studentPerformances: CalculatedStudentPerformance) =
                            tr [] [
                                td [] [ str (sprintf "%d" studentPerformances.TotalRank.Value) ]
                                td [] [ str (Class.toString studentPerformances.Class) ]
                                td [] [
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
                        let diffColTime v1 v2 =
                            let className =
                                if v1 < v2 then "has-background-success"
                                elif v1 = v2 then "has-background-warning"
                                else "has-background-danger"
                            let (sign, diff) =
                                let r = p1.RunTime - p2.RunTime
                                if r < System.TimeSpan.Zero then ("", r)
                                else ("+", r)
                            td [ Class className ] [ str (sprintf "%s%s" sign (diff.ToString("mm:ss.FF"))) ]

                        let diffColsPerformance p1 p2 =
                            [
                                diffColTime p1.RunTime p2.RunTime
                                diffColPoints p1Points p2Points
                            ]

                        let diffRow s1 s2 =
                            tr [] [
                                td [ ColSpan 3 ] []
                                yield!
                                    List.zip s1.Performances s2.Performances
                                    |> List.collect (uncurry diffColsPerformance)
                                diffColTime s1.TotalTime s2.TotalTime
                            ]

                        for (i, (s1, s2)) in model.StudentsToCompare |> List.pairwise |> List.indexed do
                            if i = 0 then
                                studentRow s1
                            diffRow s1 s2
                            studentRow s2
                    ]
                ]
                |> scrollOnOverflow
                |> Some
            else None

        let dataTable =
            match model.SelectedGroup with
            | Student ->
                Table.table [ Table.IsHoverable; Table.IsBordered; Table.IsStriped; Table.IsFullWidth ] [
                    thead [] fullTableHeader
                    tfoot [] (List.rev fullTableHeader)
                    tbody [] [
                            let data =
                                model.Data
                                |> List.takeWhile (fun studentPerformances -> studentPerformances.TotalRank.Value <= 40 && studentPerformances.TotalTime > TimeSpan.Zero)

                            for studentPerformances in data ->
                                tr
                                    [
                                        if List.contains studentPerformances model.StudentsToCompare then
                                            OnClick (fun _ev -> dispatch (RemoveStudentFromComparison studentPerformances))
                                            ClassName "is-selected"
                                        else
                                            OnClick (fun _ev -> dispatch (AddStudentToComparison studentPerformances))
                                    ]
                                    [
                                        td [] [ str (Rank.toString studentPerformances.TotalRank) ]
                                        td [] [ str (Class.toString studentPerformances.Class) ]
                                        td [] [ str (Student.fullName studentPerformances.Student) ]
                                        yield! studentPerformanceRow studentPerformances
                                    ]
                    ]
                ]
                |> scrollOnOverflow
            | Discipline discipline ->
                let tableHeader =
                    tr [] [
                        th [ Title "Rang" ] [ Fa.i [ Fa.Solid.Medal ] [] ]
                        th [ Title "Gesamtrang" ] [ Fa.i [ Fa.Solid.Medal ] [ ]; sub [] [ str "Σ" ] ]
                        th [ Title "Klasse" ] [ Fa.i [ Fa.Solid.Users ] [] ]
                        th [ Title "Name" ] [ Fa.i [ Fa.Solid.User ] [] ]
                        th [] [ str discipline.Measurement ]
                        th [ Title "Punkte" ] [ Fa.i [ Fa.Solid.Poll ] [] ]
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
