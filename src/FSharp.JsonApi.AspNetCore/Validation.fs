namespace FSharp.JsonApi

open System.Text.RegularExpressions
open Microsoft.AspNetCore.Http


/// Represents errors with a JSON-API request (not including the request body).
[<RequireQualifiedAccess>]
type JsonApiRequestError =
  /// Indicates that the client does not accept the JSON-API media type (the
  /// `Accept` header does not contain `application/vnd.api+json` or `*/*`). A
  /// suitable response is 406 Not Acceptable.
  | InvalidAccept
  /// Indicates that all JSON-API media types in the `Accept` header are
  /// modified with media type parameters. According to the JSON-API
  /// specification, the server then MUST return 406 Not Acceptable.
  | InvalidAcceptParams
  /// Indicates that the `Content-Type` header  is present and not equal to
  /// `application/vnd.api+json`. A suitable response is 415 Unsupported Media
  /// Type.
  | InvalidContentType
  /// Indicates that the `Content-Type` header is `application/vnd.api+json` and
  /// it is modified with media type parameters. According to the JSON-API
  /// specification, the server then MUST return 415 Unsupported Media Type.
  | InvalidContentTypeParams
  /// Indicates that the request contained a query parameter name that is
  /// illegal according to the JSON-API specification.
  | IllegalQueryParamName of paramName: string


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


  /// Indicates if a query parameter name is illegal according to the JSON-API
  /// specification. A custom list of regex patterns can be supplied in order to
  /// whitelist custom parameter names, but note that this is in violation of
  /// the JSON-API specification.
  let isIllegalQueryParamName customWitelist (param: string) =

    // TODO: This is not a complete check. See http://jsonapi.org/format/#query-parameters

    let whitelist =
      [ "sort"
        "include"
        "^page\[.*?\]$"
        "^filter\[.*?\]$"
        "^fields\[.*?\]$" ]
      @ customWitelist

    Regex.IsMatch(param, "^[a-z]+$")
    && whitelist |> List.forall (fun pattern -> not <| Regex.IsMatch(param, pattern))


  /// Returns a list of all query string parameter names present in the request
  /// that are illegal according to the JSON-API specification. If the returned
  /// list is not empty, then according to the JSON-API specification, the server
  /// MUST respond with 400 Bad Request. A custom list of regex patterns can be
  /// supplied in order to whitelist custom parameter names, but note that this is
  /// in violation of the JSON-API specification.
  let getIllegalQueryStringParams customWhitelist (ctx: HttpContext) =
    ctx.Request.Query
    |> Seq.map (fun kvp -> kvp.Key)
    |> Seq.filter (isIllegalQueryParamName customWhitelist)
    |> Seq.toList
