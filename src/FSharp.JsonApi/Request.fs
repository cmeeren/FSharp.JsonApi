namespace FSharp.JsonApi

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
