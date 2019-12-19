module App

open Elmish
open Elmish.Navigation
open Elmish.Streams
open Elmish.UrlParser
open Fable.Elmish.Nile
open Fable.Core.JsInterop
open FSharp.Control
open Fulma

importAll "../sass/main.sass"

type Page =
    | Achtkampf
    | HtlWarrior

let toHash page =
    match page with
    | Achtkampf -> "#achtkampf"
    | HtlWarrior -> "#htl-warrior"

let pageParser =
    oneOf [
        map Achtkampf (s "achtkampf")
        map HtlWarrior (s "htl-warrior")
    ]

type Msg =
    | AchtkampfMsg of Achtkampf.Msg
    | HtlWarriorMsg of HtlWarrior.Msg

type NavigationError =
    | InvalidUrl

type Model = {
    CurrentPage: Result<Page, NavigationError>
    Achtkampf: Achtkampf.Model
    HtlWarrior: HtlWarrior.Model
}

let urlUpdate (result : Page option) model =
    match result with
    | None ->
        { model with CurrentPage = Error InvalidUrl }
    | Some page ->
        { model with CurrentPage = Ok page }

let init result =
    {
        CurrentPage = Ok Achtkampf
        Achtkampf = Achtkampf.init
        HtlWarrior = HtlWarrior.init
    }
    |> urlUpdate result

let update msg model =
    match msg with
    | AchtkampfMsg msg -> { model with Achtkampf = Achtkampf.update msg model.Achtkampf }
    | HtlWarriorMsg msg -> { model with HtlWarrior = HtlWarrior.update msg model.HtlWarrior }

open Fable.React
open Fable.React.Props

let view model dispatch =
    let header =
        Navbar.navbar [ Navbar.Color IsPrimary ] [
            Navbar.Brand.div [] [
                Navbar.Item.a [ Navbar.Item.Props [ Href "#" ] ]
                    [ img [ Style [ Width "5em" ]
                            Src "img/logo_with_bg.svg" ] ]
            ]
            Navbar.Start.div [] [
                Navbar.Item.a [ Navbar.Item.IsTab; Navbar.Item.IsActive (model.CurrentPage = Ok Achtkampf) ] [ str "Achtkampf 💪" ]
                Navbar.Item.a [ Navbar.Item.IsTab; Navbar.Item.IsActive (model.CurrentPage = Ok HtlWarrior) ] [ str "HTL Warrior 🐱‍👤" ]
            ]
        ]

    div [] [
        yield header
        yield! Achtkampf.view model.Achtkampf (AchtkampfMsg >> dispatch)
    ]

let stream states msgs =
    let navigationErrors =
        states
        |> AsyncRx.map (snd >> fun state -> state.CurrentPage)
        |> AsyncRx.distinctUntilChanged
        |> AsyncRx.choose (function
            | Error InvalidUrl -> Some ()
            | _ -> None
        )
    let navigationPages =
        states
        |> AsyncRx.map (snd >> fun state -> state.CurrentPage)
        |> AsyncRx.distinctUntilChanged
        |> AsyncRx.choose (function
            | Ok page -> Some page
            | _ -> None
        )
        |> AsyncRx.startWith [ Achtkampf ]
    let modifyUrl url =
        AsyncRx.flatMapLatest (fun e ->
            AsyncRx.create (fun observer -> async {
                Navigation.modifyUrl url
                |> List.iter (fun sub -> sub (observer.OnNextAsync >> Async.StartImmediate))
                return AsyncDisposable.Empty
            })
        )

    [
        navigationPages
        |> AsyncRx.flatMapLatest (fun page ->
            navigationErrors
            |> modifyUrl (toHash page)
        )

        (
            states
            |> AsyncRx.choose (fun (msg, state) ->
                match msg with
                | None -> Some (None, state.Achtkampf)
                | Some (AchtkampfMsg msg) -> Some (Some msg, state.Achtkampf)
                | Some _ -> None
            ),
            msgs |> AsyncRx.choose (function | AchtkampfMsg msg -> Some msg | _ -> None)
        )
        ||> Achtkampf.stream
        |> AsyncRx.map AchtkampfMsg

        (
            states
            |> AsyncRx.choose (fun (msg, state) ->
                match msg with
                | None -> Some (None, state.HtlWarrior)
                | Some (HtlWarriorMsg msg) -> Some (Some msg, state.HtlWarrior)
                | Some _ -> None
            ),
            msgs |> AsyncRx.choose (function | HtlWarriorMsg msg -> Some msg | _ -> None)
        )
        ||> HtlWarrior.stream
        |> AsyncRx.map HtlWarriorMsg
    ]
    |> AsyncRx.mergeSeq

open Elmish.React
open Elmish.Debug
open Elmish.HMR

// App
Program.mkSimple init update view
|> Program.withStream stream
|> Program.toNavigable (parseHash pageParser) (fun p m -> urlUpdate p m, Cmd.none)
#if DEBUG
|> Program.withDebugger
|> Program.withConsoleTrace
#endif
|> Program.withReactBatched "elmish-app"
|> Program.run
