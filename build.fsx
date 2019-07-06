#load ".fake/build.fsx/intellisense.fsx"

open Fake.Core
open Fake.DotNet
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.IO.Globbing.Operators
open Fake.Core.TargetOperators
open Microsoft.Graph
open Microsoft.Identity.Client
open System.Net
open System.Net.Http.Headers
open System.Threading.Tasks

Target.initEnvironment ()

let toSecureString (text: string) =
    // see https://stackoverflow.com/a/43084626/1293659
    NetworkCredential("", text).SecurePassword

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
    printfn "Auth result: %s" (authResult.CreateAuthorizationHeader())
    let authenticationProvider = DelegateAuthenticationProvider(fun request ->
        request.Headers.Authorization <- new AuthenticationHeaderValue("bearer", authResult.AccessToken)
        Task.CompletedTask
    )
    let graphServiceClient = GraphServiceClient(authenticationProvider)
    let workbookRange = graphServiceClient.Me.Drive.Items.[Environment.environVarOrFail "FILE_ID"].Workbook.Worksheets.[Environment.environVarOrFail "WORKSHEET_ID"].UsedRange().Request().GetAsync() |> Async.AwaitTask |> Async.RunSynchronously
    printfn "Workbook range: %O" workbookRange.Values
    ()
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
