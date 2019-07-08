open Microsoft.Graph
open Microsoft.Identity.Client
open System
open System.Net
open System.Net.Http.Headers
open System.Threading.Tasks

let authenticateInteractively (clientApp: IPublicClientApplication) =
    clientApp
        .AcquireTokenInteractive([| "Files.ReadWrite" |])
        .ExecuteAsync()
    |> Async.AwaitTask

let toSecureString (text: string) =
    // see https://stackoverflow.com/a/43084626/1293659
    NetworkCredential("", text).SecurePassword

let authenticateByUsernameAndPassword (clientApp: IPublicClientApplication) =
    clientApp
        .AcquireTokenByUsernamePassword([| "Files.ReadWrite" |], Environment.GetEnvironmentVariable "AAD_USERNAME", Environment.GetEnvironmentVariable "AAD_PASSWORD" |> toSecureString )
        .ExecuteAsync()
    |> Async.AwaitTask

let createSubscription (graphServiceClient: IGraphServiceClient) =
    // Specific drive item not supported (see https://docs.microsoft.com/en-us/graph/webhooks?toc=.%2Fref%2Ftoc.json&view=graph-rest-1.0#supported-resources)
    // let resourceUrl =
    //     graphServiceClient.Me.Drive.Items.[Environment.GetEnvironmentVariable "FILE_ID"].Request().RequestUrl
    //     |> Uri
    // let resource =
    //     resourceUrl.Segments
    //     |> Array.skip 2
    //     |> Array.append resourceUrl.Segments.[0..0]
    //     |> String.concat ""
    let resource = "/me/drive/root"
    let subscription =
        Subscription(
            ChangeType = "updated",
            NotificationUrl = Environment.GetEnvironmentVariable "TRIGGER_BUILD_URL",
            Resource = resource,
            ExpirationDateTime = Nullable<_> (DateTimeOffset.Now.AddMinutes 43200.),
            ClientState = Environment.GetEnvironmentVariable "CLIENT_STATE"
        )
        |> graphServiceClient.Subscriptions.Request().AddAsync
        |> Async.AwaitTask
        |> Async.RunSynchronously

    subscription

let getSubscriptions (graphServiceClient: IGraphServiceClient) =
    let rec fn (request: IGraphServiceSubscriptionsCollectionRequest) acc = async {
        let! subscriptions =
            request.GetAsync()
            |> Async.AwaitTask
        let acc' = acc @ Seq.toList subscriptions
        match Option.ofObj subscriptions.NextPageRequest with
        | Some nextPageRequest ->
            return! fn nextPageRequest acc'
        | None -> return acc'
    }
    fn (graphServiceClient.Subscriptions.Request()) []

let printSubscription (subscription: Subscription) =
    [
        subscription.Id
        subscription.Resource
        subscription.ChangeType
        subscription.ExpirationDateTime.ToString()
        subscription.NotificationUrl
    ]
    |> String.concat " - "

[<EntryPoint>]
let main argv =
    let clientApp =
        PublicClientApplicationBuilder.Create(Environment.GetEnvironmentVariable "APP_ID")
            .WithAuthority(AzureCloudInstance.AzurePublic, Environment.GetEnvironmentVariable "TENANT_ID")
            .WithRedirectUri("http://localhost")
            .Build()
    // let authResult = authenticateInteractively clientApp |> Async.RunSynchronously
    let authResult = authenticateByUsernameAndPassword clientApp |> Async.RunSynchronously

    let authProvider =
        DelegateAuthenticationProvider(fun request ->
            request.Headers.Authorization <- AuthenticationHeaderValue("Bearer", authResult.AccessToken)
            Task.CompletedTask
        )
    let graphServiceClient = GraphServiceClient(authProvider)

    // let newSubscription = createSubscription graphServiceClient
    // printfn "New subscription: %s" (printSubscription newSubscription)

    let subscriptions = getSubscriptions graphServiceClient |> Async.RunSynchronously

    subscriptions
    |> List.map printSubscription
    |> List.iter (printfn "* %s")

    // subscriptions
    // |> List.map (fun subscription -> graphServiceClient.Subscriptions.[subscription.Id].Request().DeleteAsync() |> Async.AwaitTask)
    // |> Async.Parallel
    // |> Async.Ignore
    // |> Async.RunSynchronously

    0
