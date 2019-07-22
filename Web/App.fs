module App

open Elmish
open Elmish.Navigation
open Elmish.Streams
open Elmish.UrlParser
open Fable.Elmish.Nile
open Fable.Core.JsInterop
open FSharp.Control
open Fulma
open ParseWorksheet
open Thoth.Fetch
open Thoth.Json

importAll "../sass/main.sass"

type Page =
    | Achtkampf

let toHash page =
    match page with
    | Achtkampf -> "#achtkampf"

let pageParser =
    oneOf [
        map Achtkampf (s "achtkampf")
    ]

type Msg =
    | AchtkampfMsg of Achtkampf.Msg

type NavigationError =
    | InvalidUrl

type Model = {
    CurrentPage: Result<Page, NavigationError>
    Achtkampf: Achtkampf.Model
}

let urlUpdate (result : Page option) model =
    match result with
    | None ->
        { model with CurrentPage = Error InvalidUrl }
    | Some page ->
        { model with CurrentPage = Ok page }

let init result =
    { CurrentPage = Ok Achtkampf; Achtkampf = Achtkampf.init }
    |> urlUpdate result

let update msg model =
    match msg with
    | AchtkampfMsg msg -> { model with Achtkampf = Achtkampf.update msg model.Achtkampf }

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
                Navbar.Item.a [ Navbar.Item.IsTab; Navbar.Item.IsActive true ] [ str "Achtkampf 💪" ]
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
            ),
            msgs |> AsyncRx.choose (function | AchtkampfMsg msg -> Some msg)
        )
        ||> Achtkampf.stream
        |> AsyncRx.map AchtkampfMsg
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
