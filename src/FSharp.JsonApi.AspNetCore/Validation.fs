namespace FSharp.JsonApi

open Microsoft.AspNetCore.Http


module internal Validation =


  /// Indicates if the client accepts the JSON-API media type (if Accept contains
  /// application/vnd.api+json or */*).
  let acceptsJsonApi (ctx: HttpContext) =
    ctx.Request.GetTypedHeaders().Accept
    |> Option.ofObj
    |> Option.map List.ofSeq
    |> Option.defaultValue []
    |> Seq.exists (fun x ->
        x.MediaType.Value = MediaTypes.jsonApi
        || x.MediaType.Value = "*/*")


  /// Returns true if Content-Type is present and not equal to application/vnd.api+json.
  let hasNonJsonApiContent (ctx: HttpContext) =
    let cType = ctx.Request.GetTypedHeaders().ContentType
    not <| isNull cType && cType.MediaType.Value <> MediaTypes.jsonApi


  /// Returns true if Content-Type is application/vnd.api+json and it is modified
  /// with media type parameters. According to the JSON-API specification, the server
  /// then MUST return 415 Unsupported Media Type.
  let jsonApiContentTypeHasParams (ctx: HttpContext) =
    let headers = ctx.Request.GetTypedHeaders()
    not <| isNull headers.ContentType
    && headers.ContentType.MediaType.Value = MediaTypes.jsonApi
    && headers.ContentType.Parameters.Count > 0


  /// Returns true if all JSON-API media types in the Accept header are modified
  /// with media type parameters. According to the JSON-API specification, the
  /// server then MUST return 406 Not Acceptable.
  let allJsonApiAcceptsHaveParams (ctx: HttpContext) =
    let headers = ctx.Request.GetTypedHeaders()
    let jsonApiAccepts =
      headers.Accept
      |> Option.ofObj
      |> Option.map List.ofSeq
      |> Option.defaultValue []
      |> List.filter (fun x -> x.MediaType.Value = MediaTypes.jsonApi)
    not jsonApiAccepts.IsEmpty
    && jsonApiAccepts |> List.forall (fun x -> x.Parameters.Count > 0)


  /// Returns a list of all query string parameter names present in the request
  /// that are illegal according to the JSON-API specification. If the returned
  /// list is not empty, then according to the JSON-API specification, the server
  /// MUST respond with 400 Bad Request. A custom list of regex patterns can be
  /// supplied in order to whitelist custom parameter names, but note that this is
  /// in violation of the JSON-API specification.
  let getIllegalQueryStringParams customWhitelist (ctx: HttpContext) =
    ctx.Request.Query
    |> Seq.map (fun kvp -> kvp.Key)
    |> Seq.filter (fun n -> Query.IsIllegalName(n, customWhitelist))
    |> Seq.toList
