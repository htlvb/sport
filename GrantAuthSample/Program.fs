open Microsoft.Identity.Client
open System

[<EntryPoint>]
let main argv =
    let clientApp =
        PublicClientApplicationBuilder.Create(Environment.GetEnvironmentVariable "APP_ID")
            .WithAuthority(AzureCloudInstance.AzurePublic, Environment.GetEnvironmentVariable "TENANT_ID")
            .Build()
    let authResult =
        clientApp
            .AcquireTokenInteractive([| "Files.ReadWrite" |])
            .ExecuteAsync()
        |> Async.AwaitTask
        |> Async.RunSynchronously

    printfn "Auth token: %s" authResult.AccessToken
    0
