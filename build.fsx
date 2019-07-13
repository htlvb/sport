#load "Common/Utils.fs"
#load "Common/String.fs"
#load "Common/Result.fs"
#load "Common/List.fs"
#load "Common/ParseWorksheet.fs"
#load ".fake/build.fsx/intellisense.fsx"

open Fake.Core
open Fake.DotNet
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.IO.Globbing.Operators
open Fake.Core.TargetOperators
open Microsoft.Graph
open Microsoft.Identity.Client
open ParseWorksheet
open System.Net
open System.Net.Http.Headers
open System.Text.RegularExpressions
open System.Threading.Tasks

Target.initEnvironment ()

let toSecureString (text: string) =
    // see https://stackoverflow.com/a/43084626/1293659
    NetworkCredential("", text).SecurePassword

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

Target.create "LoadData" (fun _ ->
    let clientApp =
        PublicClientApplicationBuilder.Create(Environment.environVarOrFail "APP_ID")
            .WithAuthority(AzureCloudInstance.AzurePublic, Environment.environVarOrFail "TENANT_ID")
            .Build()

    let authResult =
        clientApp
            .AcquireTokenByUsernamePassword([| "Files.ReadWrite" |], Environment.environVarOrFail "AAD_USERNAME", Environment.environVarOrFail "AAD_PASSWORD" |> toSecureString )
            .ExecuteAsync()
        |> Async.AwaitTask
        |> Async.RunSynchronously

    let authenticationProvider = DelegateAuthenticationProvider(fun request ->
        request.Headers.Authorization <- new AuthenticationHeaderValue("Bearer", authResult.AccessToken)
        Task.CompletedTask
    )
    let graphServiceClient = GraphServiceClient(authenticationProvider)

    let folder =
        graphServiceClient.Me.Drive.Items.[Environment.environVarOrFail "FOLDER_ID"].Children.Request().OrderBy("name desc").Top(1).GetAsync()
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
                    return
                        match Worksheet.tryParse values.Values with
                        | Ok data -> Ok { Class = schoolClass; Performances = data }
                        | Error NotEnoughRows -> Result.Error (schoolClass, NotEnoughRows)
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
        | Error errors ->
            failwithf "%d worksheet(s) couldn't be parsed: %A" (List.length errors) errors
)

// Target.create "Clean" (fun _ ->
//     !! "src/**/bin"
//     ++ "src/**/obj"
//     |> Shell.cleanDirs 
// )

// Target.create "Build" (fun _ ->
//     !! "src/**/*.*proj"
//     |> Seq.iter (DotNet.build id)
// )

Target.create "All" ignore

// "Clean"
//   ==> "Build"
"LoadData"
   ==> "All"

Target.runOrDefault "All"
