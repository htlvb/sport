namespace Sport

open Microsoft.AspNetCore.Mvc
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Logging
open System.IO

module BuildTrigger =
    let run(req: HttpRequest, log: ILogger) =
        match req.Query.TryGetValue "validationToken" with
        | (true, validationToken) ->
            ContentResult(Content = String.concat "" validationToken, ContentType = "text/plain")
        | _ ->
            use reader = new StreamReader(req.Body)
            log.LogInformation(sprintf "Request body: %s" (reader.ReadToEnd()))
            ContentResult(Content = "Hello World", ContentType = "text/html") 