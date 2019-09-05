[<AutoOpen>]
module FSharp.JsonApi.HttpHandlers

open Microsoft.AspNetCore.Http
open Giraffe
open FSharp.Control.Tasks.V2.ContextInsensitive
  
  
/// Serializes the document and writes the output to the body of the HTTP
/// response. Also sets the HTTP `Content-Type` header to
/// `application/vnd.api+json` and sets the `Content-Length` header accordingly.
/// This is simply an HttpHandler version of HttpContext.WriteJsonApiAsync.
let jsonApi (jsonApiCtx: JsonApiContext<'ResourceDiscriminator>) (doc: #IJsonApiDocument) : HttpHandler =
  fun (next : HttpFunc) (ctx : HttpContext) ->
    task {
      do! ctx.WriteJsonApiAsync(doc, jsonApiCtx)
      return Some ctx
    }
  

/// Sets the HTTP "Location" header to the value of the main resource's "self"
/// URL, if present. This is simply an HttpHandler version of
/// HttpContext.SetLocationHeaderFromMainSelfUrl.
let setLocationHeaderFromMainSelfUrl (doc: ResourceDocument) : HttpHandler =
  fun next ctx ->
    ctx.SetLocationHeaderFromMainSelfUrl doc
    next ctx


/// Validates the JSON-API request (not including the request body). This is
/// simply a HttpHandler version of HttpContext.ValidateJsonApiRequest. Use that
/// method if you need to allow custom query parameter names.
let validateJsonApiRequest (errorHandler: RequestError list -> HttpHandler) : HttpHandler =
  fun (next : HttpFunc) (ctx : HttpContext) ->
    match ctx.ValidateJsonApiRequest () with
    | Ok () -> next ctx
    | Error errs -> errorHandler errs next ctx
