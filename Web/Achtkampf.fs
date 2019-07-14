module Achtkampf

open Elmish.Streams
open FSharp.Control
open ParseWorksheet
open Thoth.Fetch
open Thoth.Json

type FetchError =
    | HttpError of exn
    | DecodeError of string

type Msg =
    | LoadAchtkampfResponse of Result<ClassPerformances list, FetchError>

type Model =
    | NotLoaded
    | Loading
    | Loaded of ClassPerformances list
    | LoadError of FetchError

let init = NotLoaded

let update msg model =
    match msg with
    | LoadAchtkampfResponse (Ok classPerformances) ->
        Loaded classPerformances
    | LoadAchtkampfResponse (Error error) ->
        LoadError error

open Fable.React

let mainView = str "Achtkampf table"

let footerView = str "Group tabs"

let stream states msgs =
    let loadAchtkampfData =
        AsyncRx.ofPromise (promise {
            let! response = Fetch.tryFetchAs ("/api/achtkampf/data.json", Decode.list ClassPerformances.decoder)
            return response |> Result.mapError DecodeError
        })
        |> AsyncRx.catch (HttpError >> Error >> AsyncRx.single)

    [
        states
        |> AsyncRx.flatMapLatest (snd >> function | NotLoaded -> loadAchtkampfData | _ -> AsyncRx.empty ())
        |> AsyncRx.map LoadAchtkampfResponse
    ]
    |> AsyncRx.mergeSeq
