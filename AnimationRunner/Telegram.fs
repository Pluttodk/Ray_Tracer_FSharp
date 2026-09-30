/// Delivers finished videos to a Telegram chat through the Bot API.
module AnimationRunner.Telegram

open System
open System.IO
open System.Net.Http
open System.Text.Json

/// Credentials come from the environment so they never appear in the repository or in shell history.
let tokenVariable = "TELEGRAM_BOT_TOKEN"
let chatVariable = "TELEGRAM_CHAT_ID"
/// Optional override of https://api.telegram.org, e.g. for a local Bot API server or tests.
let apiVariable = "TELEGRAM_API_BASE"

/// Bots may upload at most 50 MB through the public Bot API.
let maxUploadBytes = 50L * 1024L * 1024L

type Target = { Token: string; ChatId: string; ApiBase: string }

/// Reads the target from the environment; fails fast (before rendering) when anything is missing.
let fromEnvironment () =
    let get name = Environment.GetEnvironmentVariable name |> Option.ofObj |> Option.filter (String.IsNullOrWhiteSpace >> not)
    match get tokenVariable, get chatVariable with
    | Some token, Some chat ->
        { Token = token.Trim(); ChatId = chat.Trim(); ApiBase = (defaultArg (get apiVariable) "https://api.telegram.org").TrimEnd('/') }
    | token, chat ->
        let missing = [ if token.IsNone then tokenVariable; if chat.IsNone then chatVariable ]
        invalidArg "telegram" $"""--telegram needs {String.Join(" and ", missing)} set in the environment (create a bot with @BotFather; the chat ID is your user or group ID)."""

/// Never let the token leak into logs through an exception message.
let private redact (target: Target) (text: string) = text.Replace(target.Token, "<token>")

/// Uploads `path` as a streamable video with `caption`. Returns an error message instead of throwing, so a
/// failed delivery never costs the rendered frames or video.
let sendVideo (target: Target) (path: string) (caption: string) =
    let size = FileInfo(path).Length
    if size > maxUploadBytes then
        Error $"{Path.GetFileName path} is {float size / 1048576.:F1} MB; the Telegram Bot API accepts at most 50 MB. Lower --res or --spp, or send it manually."
    else
        try
            use client = new HttpClient(Timeout = TimeSpan.FromMinutes 10.)
            use form = new MultipartFormDataContent()
            form.Add(new StringContent(target.ChatId), "chat_id")
            form.Add(new StringContent(caption), "caption")
            form.Add(new StringContent("true"), "supports_streaming")
            use stream = File.OpenRead path
            let file = new StreamContent(stream)
            file.Headers.ContentType <- Headers.MediaTypeHeaderValue("video/mp4")
            form.Add(file, "video", Path.GetFileName path)
            use response = client.PostAsync($"{target.ApiBase}/bot{target.Token}/sendVideo", form).GetAwaiter().GetResult()
            let body = response.Content.ReadAsStringAsync().GetAwaiter().GetResult()
            let description () =
                try
                    use json = JsonDocument.Parse body
                    match json.RootElement.TryGetProperty "description" with
                    | true, d -> d.GetString()
                    | _ -> body
                with _ -> body
            if response.IsSuccessStatusCode then Ok ()
            else Error (redact target $"Telegram refused the upload ({int response.StatusCode}): {description ()}")
        with error -> Error (redact target $"Telegram upload failed: {error.Message}")
