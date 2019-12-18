open Microsoft.Graph
open Microsoft.Identity.Client
open System
open System.Net
open System.Net.Http.Headers
open System.Threading.Tasks
open Thoth.Json.Net

let toSecureString (text: string) =
    // see https://stackoverflow.com/a/43084626/1293659
    NetworkCredential("", text).SecurePassword

let environVarOrFail key =
    let value = Environment.GetEnvironmentVariable key
    if String.IsNullOrEmpty value then failwithf "Environment variable \"%s\" not set" key
    else value

let rec readAll initialRequest executeRequest getNextRequest =
    let rec fn request acc = async {
        let! items = executeRequest request
        let acc' = acc @ Seq.toList items
        match getNextRequest items with
        | Some request' -> return! fn request' acc'
        | None -> return acc'
    }
    fn initialRequest []

let getFiles (request: IDriveItemChildrenCollectionRequest) =
    readAll request (fun r -> r.GetAsync() |> Async.AwaitTask) (fun r -> Option.ofObj r.NextPageRequest)

let getWorksheets (request: IWorkbookWorksheetsCollectionRequest) =
    readAll request (fun r -> r.GetAsync() |> Async.AwaitTask) (fun r -> Option.ofObj r.NextPageRequest)

let loadAchtkampfData (graphServiceClient: GraphServiceClient) =
    let folder =
        graphServiceClient.Me.Drive.Items.[environVarOrFail "ACHTKAMPF_FOLDER_ID"].Children.Request().OrderBy("name desc").Top(1).GetAsync()
        |> Async.AwaitTask
        |> Async.RunSynchronously
        |> Seq.tryHead
    match folder with
    | None -> failwith "No folders found"
    | Some folder ->
        let files = getFiles (graphServiceClient.Me.Drive.Items.[folder.Id].Children.Request()) |> Async.RunSynchronously

        let readFile (item: DriveItem) = async {
            let worksheetsRequestBuilder = graphServiceClient.Me.Drive.Items.[item.Id].Workbook.Worksheets
            let! worksheets = getWorksheets (worksheetsRequestBuilder.Request())
            return!
                worksheets
                |> List.choose (fun worksheet -> Class.tryParse worksheet.Name |> Option.map (fun c -> worksheet, c))
                |> List.map (fun (worksheet, schoolClass) -> async {
                    let! values = worksheetsRequestBuilder.[worksheet.Id].UsedRange().Request().GetAsync() |> Async.AwaitTask
                    let data = AchtkampfData.Worksheet.tryParse values.Values |> Result.mapError List.singleton
                    return
                        Ok AchtkampfData.ClassPerformances.create
                        |> Result.apply (Ok schoolClass)
                        |> Result.apply data
                })
                |> Async.Parallel
        }

        let data =
            files
            |> List.map readFile
            |> Async.Parallel
            |> Async.RunSynchronously
            |> Seq.collect id
            |> Seq.toList
            |> List.sequenceResultApplicative
        match data with
        | Ok performances ->
            printfn "%d worksheets successfully parsed" (List.length performances)
            performances
            |> List.map AchtkampfData.ClassPerformances.encode
            |> Encode.list
            |> Encode.toString 2
            |> File.writeAllText ("." |> Path.combine [ "public"; "api"; "achtkampf"; "data.json" ])
        | Error errors ->
            failwithf "%d worksheet(s) couldn't be parsed: %A" (List.length errors) errors

let loadHtlWarriorData (graphServiceClient: GraphServiceClient) =
    let folder =
        graphServiceClient.Me.Drive.Items.[environVarOrFail "HTL_WARRIOR_FOLDER_ID"].Children.Request().OrderBy("name desc").Top(1).GetAsync()
        |> Async.AwaitTask
        |> Async.RunSynchronously
        |> Seq.tryHead
    match folder with
    | None -> failwith "No folders found"
    | Some folder ->
        let files = getFiles (graphServiceClient.Me.Drive.Items.[folder.Id].Children.Request()) |> Async.RunSynchronously

        let readFile (item: DriveItem) = async {
            let worksheetsRequestBuilder = graphServiceClient.Me.Drive.Items.[item.Id].Workbook.Worksheets
            let! worksheets = getWorksheets (worksheetsRequestBuilder.Request())
            return!
                worksheets
                |> List.choose (fun worksheet -> Class.tryParse worksheet.Name |> Option.map (fun c -> worksheet, c))
                |> List.map (fun (worksheet, schoolClass) -> async {
                    let! values = worksheetsRequestBuilder.[worksheet.Id].UsedRange().Request().GetAsync() |> Async.AwaitTask
                    let data = HtlWarriorData.Worksheet.tryParse values.Values |> Result.mapError List.singleton
                    return
                        Ok HtlWarriorData.ClassPerformances.create
                        |> Result.apply (Ok schoolClass)
                        |> Result.apply data
                })
                |> Async.Parallel
        }

        let data =
            files
            |> List.map readFile
            |> Async.Parallel
            |> Async.RunSynchronously
            |> Seq.collect id
            |> Seq.toList
            |> List.sequenceResultApplicative
        match data with
        | Ok performances ->
            printfn "%d worksheets successfully parsed" (List.length performances)
            performances
            |> List.map HtlWarriorData.ClassPerformances.encode
            |> Encode.list
            |> Encode.toString 2
            |> File.writeAllText ("." |> Path.combine [ "public"; "api"; "htl-warrior"; "data.json" ])
        | Error errors ->
            failwithf "%d worksheet(s) couldn't be parsed: %A" (List.length errors) errors

let loadData() =
    let clientApp =
        PublicClientApplicationBuilder.Create(environVarOrFail "APP_ID")
            .WithAuthority(AzureCloudInstance.AzurePublic, environVarOrFail "TENANT_ID")
            .Build()

    let authResult =
        clientApp
            .AcquireTokenByUsernamePassword([| "Files.ReadWrite" |], environVarOrFail "AAD_USERNAME", environVarOrFail "AAD_PASSWORD" |> toSecureString )
            .ExecuteAsync()
        |> Async.AwaitTask
        |> Async.RunSynchronously

    let authenticationProvider = DelegateAuthenticationProvider(fun request ->
        request.Headers.Authorization <- AuthenticationHeaderValue("Bearer", authResult.AccessToken)
        Task.CompletedTask
    )
    let graphServiceClient = GraphServiceClient(authenticationProvider)

    loadAchtkampfData graphServiceClient
    loadHtlWarriorData graphServiceClient

[<EntryPoint>]
let main argv =
    loadData ()
    0
