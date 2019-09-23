namespace FSharp.JsonApi

open System.Text
open System.Threading.Tasks
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Primitives
open Microsoft.Net.Http.Headers


[<AutoOpen>]
module HttpContextExtensions =

  type HttpContext with

    /// Returns a parser for the query params in this HttpContext. Same as
    /// QueryParser.FromHttpContext.
    member this.QueryParser =
      QueryParser.FromHttpContext this

    /// Sets the HTTP "Location" header to the value of the main resource's
    /// "self" URL, if present.
    member this.SetLocationHeaderFromMainSelfUrl (doc: ResourceDocument) =
      match ResourceDocument.mainSelfUrl doc with
      | None -> ()
      | Some url -> this.Response.Headers.Add("Location", url |> string |> StringValues)

    /// Serializes the document and writes the output to the body of the HTTP
    /// response. Also sets the HTTP `Content-Type` header to
    /// `application/vnd.api+json` and sets the `Content-Length` header
    /// accordingly.
    member this.WriteJsonApiAsync (dataObj: #IJsonApiDocument, jsonApiCtx: JsonApiContext<'ResourceDiscriminator>) =
      let bytes = jsonApiCtx.Serialize dataObj |> Encoding.UTF8.GetBytes
      this.Response.Headers.[HeaderNames.ContentType] <- MediaTypes.jsonApi |> StringValues
      this.Response.Headers.[HeaderNames.ContentLength] <- bytes.Length |> string |> StringValues
      if this.Request.Method <> HttpMethods.Head then
        this.Response.Body.WriteAsync(bytes, 0, bytes.Length)
      else Task.CompletedTask


    /// Validates the JSON-API request (not including the request body). The
    /// customQueryNameRegexes parameter can be used to whitelist custom query
    /// parameter names.
    member this.ValidateJsonApiRequest (customQueryNameRegexes: string list) =
      let errs =
        [
          if not <| Validation.acceptsJsonApi this then yield RequestError.InvalidAccept
          if Validation.hasNonJsonApiContent this then yield RequestError.InvalidContentType
          if Validation.allJsonApiAcceptsHaveParams this then yield RequestError.InvalidAcceptParams
          if Validation.jsonApiContentTypeHasParams this then yield RequestError.InvalidContentTypeParams

          match Validation.getIllegalQueryStringParams customQueryNameRegexes this with
          | [] -> ()
          | names -> yield! names |> List.map RequestError.IllegalQueryParamName
        ]
      match errs with
      | [] -> Ok ()
      | errs -> Error errs

    /// Validates the JSON-API request (not including the request body).
    member this.ValidateJsonApiRequest () =
      this.ValidateJsonApiRequest([])
