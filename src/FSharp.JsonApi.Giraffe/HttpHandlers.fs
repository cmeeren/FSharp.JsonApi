[<AutoOpen>]
module FSharp.JsonApi.HttpHandlers

open System
open System.Security.Cryptography
open Microsoft.AspNetCore.Http
open Microsoft.Net.Http.Headers
open Giraffe
open FSharp.Control.Tasks.V2.ContextInsensitive
  
  
/// Serializes the document and writes the output to the body of the HTTP
/// response (unless the request method is HEAD). Also sets the HTTP
/// `Content-Type` header to `application/vnd.api+json` and sets the
/// `Content-Length` header accordingly. This is simply an HttpHandler version
/// of HttpContext.WriteJsonApiAsync.
let jsonApi (jsonApiCtx: JsonApiContext<'ResourceDiscriminator>) (doc: #IJsonApiDocument) : HttpHandler =
  fun (next : HttpFunc) (ctx : HttpContext) ->
    task {
      do! ctx.WriteJsonApiAsync(doc, jsonApiCtx)
      return Some ctx
    }


/// Serializes the document, calculates a hash of the response bytes using the
/// supplied hash function, and sets the response's ETag to a string
/// representation of this hash. If the request contains an `If-None-Match`
/// header matching the ETag, returns 304 Not Modified. Otherwise, writes the
/// output to the body of the HTTP response (unless the request method is HEAD),
/// sets the HTTP `Content-Type` header to `application/vnd.api+json`, and sets
/// the `Content-Length` header accordingly.
let jsonApiETagWith (computeHash: byte [] -> byte []) (jsonApiCtx: JsonApiContext<'ResourceDiscriminator>) (doc: #IJsonApiDocument) : HttpHandler =
  fun (next : HttpFunc) (ctx : HttpContext) ->
    task {
      let bytes = jsonApiCtx.SerializeAndGetBytes doc
      let eTag =
        bytes
        |> computeHash
        |> Convert.ToBase64String
        |> fun s -> s.TrimEnd('=')
        |> EntityTagHeaderValue.FromString false

      match ctx.ValidatePreconditions (Some eTag) None with
      | ResourceNotModified -> return ctx.NotModifiedResponse ()
      | _ ->
          do! ctx.WriteJsonApiAsync bytes
          return Some ctx
    }


/// Serializes the document, calculates a hash of the response bytes, and sets
/// the ETag to this hash. If the request contains an `If-None-Match` header
/// matching the ETag, returns 304 Not Modified. Otherwise, writes the output to
/// the body of the HTTP response (unless the request method is HEAD), sets the
/// HTTP `Content-Type` header to `application/vnd.api+json`, and sets the
/// `Content-Length` header accordingly.
let jsonApiETag (jsonApiCtx: JsonApiContext<'ResourceDiscriminator>) (doc: #IJsonApiDocument) : HttpHandler =
  fun (next : HttpFunc) (ctx : HttpContext) ->
    task {
      use sha1 = SHA1.Create()
      return! jsonApiETagWith sha1.ComputeHash jsonApiCtx doc next ctx
    }


/// Writes the specified bytes to the body of the HTTP response (unless the
/// request method is HEAD). Also sets the HTTP `Content-Type` header to
/// `application/vnd.api+json` and sets the `Content-Length` header accordingly.
/// This is simply an HttpHandler version of HttpContext.WriteJsonApiAsync.
let jsonApiBytes (bytes: byte []) : HttpHandler =
  fun (next : HttpFunc) (ctx : HttpContext) ->
    task {
      do! ctx.WriteJsonApiAsync bytes
      return Some ctx
    }


/// Sets the HTTP "Location" header to the value of the main resource's "self"
/// URL, if present. This is simply an HttpHandler version of
/// HttpContext.SetLocationHeaderFromMainSelfUrl.
let setLocationHeaderFromMainSelfUrl (doc: ResourceDocument) : HttpHandler =
  fun next ctx ->
    ctx.SetLocationHeaderFromMainSelfUrl doc
    next ctx


/// Validates the JSON:API request (not including the request body). This is
/// simply a HttpHandler version of HttpContext.ValidateJsonApiRequest. Use that
/// method if you need to allow custom query parameter names.
let validateJsonApiRequest (errorHandler: RequestError list -> HttpHandler) : HttpHandler =
  fun (next : HttpFunc) (ctx : HttpContext) ->
    match ctx.ValidateJsonApiRequest () with
    | Ok () -> next ctx
    | Error errs -> errorHandler errs next ctx
