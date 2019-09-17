module ErrorHandling

(*
This opinionated form of DU-based error handling makes it very easy to return
helpful error objects for any conceivable error condition, and plays very well
with monadic/applicative error handling using e.g. FsToolkit.ErrorHandling (as
demonstrated in HttpHandlers.fs).

This module defines an ApiError union with cases for each error that can be
returned by the API. Since the domain in this sample is "boring", there are no
domain-specific errors, and almost all of the cases below correspond to an error
that can be returned by functions and methods in FSharp.JsonApi.

After defining the error cases, we define a few functions that simply map from
FSharp.JsonApi errors to the corresponding ApiError case. Then we define the
getStatusAndError function that creates a JSON-API error object for each error
case, and returns it along with the HTTP status code to be used.

Whenever I create a new API, I basically just copy this entire file.
*)

open FSharp.JsonApi


type ApiError =
  // Basic JSON-API validation
  | InvalidAccept
  | InvalidAcceptParams
  | InvalidContentType
  | InvalidContentTypeParams
  | IllegalQueryParamName of paramName: string
  // Query parameter parsing errors
  | QueryInvalidEnum of paramName: string * invalidValue: string * allowedValues: string list
  | QueryInvalidParsed of paramName: string * invalidValue: string * errMsg: string option
  | QueryTooLarge of paramName: string * invalidValue: int * max: int
  | QueryTooSmall of paramName: string * invalidValue: int * min: int
  | QueryMissing of paramName: string
  | QueryNotSingular of paramName: string * numProvidedValues: int
  // JSON-API document validation errors
  | Malformed of ex: exn * json: string
  | InvalidNull of pointer: string * isOverride: bool
  | InvalidNullOrMissing of pointer: string
  | FieldReadOnly of pointer: string * isOverride: bool
  | UnexpectedType of pointer: string * actual: string * expected: string list
  | ResourceIdNotAllowedForPost of pointer: string
  | ResourceIdIncorrectForPatch of pointer: string * actual: string option * expected: string
  | RequiredFieldMissing of pointer: string
  | AttributeInvalidParsed of pointer: string * errMsg: string option
  | AttributeInvalidEnum of pointer: string * invalidValue: string * allowedValues: string list
  | RelationshipResourceNotFound of pointer: string * resType: string * resId: string

  // Custom errors

  /// General "resource not found" error
  | ResourceNotFound

  /// General 404 Not Found error (for paths that don't exist)
  | NotFound

  /// For invalid authorization (not actually used in this sample)
  | Unauthorized

  /// General 500 error
  | UnknownError


// Below are mappers from FSharp.JsonApi errors to ApiError. Note that all
// FSharp.JsonApi errors are documented; use Intellisense to see the
// descriptions. Some FSharp.JsonApi errors map to specific JSON-API error
// conditions that require the server to respond with specific status codes.


let requestError = function
  | RequestError.InvalidAccept -> InvalidAccept
  | RequestError.InvalidAcceptParams -> InvalidAcceptParams
  | RequestError.InvalidContentType -> InvalidContentType
  | RequestError.InvalidContentTypeParams -> InvalidContentTypeParams
  | RequestError.IllegalQueryParamName name -> IllegalQueryParamName name


let queryError = function
  | QueryError.InvalidEnum (name, inv, alw) -> QueryInvalidEnum (name, inv, alw)
  | QueryError.InvalidParsed (name, inv, msg) -> QueryInvalidParsed (name, inv, msg)
  | QueryError.TooLarge (name, inv, max) -> QueryTooLarge (name, inv, max)
  | QueryError.TooSmall (name, inv, min) -> QueryTooSmall (name, inv, min)
  | QueryError.Missing name -> QueryMissing name
  | QueryError.NotSingular (name, n) -> QueryNotSingular (name, n)


let docError = function
  | RequestDocumentError.Malformed (ex, json) -> Malformed (ex, json)
  | RequestDocumentError.InvalidNull (ptr, ovr) -> InvalidNull (ptr, ovr)
  | RequestDocumentError.InvalidNullOrMissing ptr -> InvalidNullOrMissing ptr
  | RequestDocumentError.FieldReadOnly (ptr, ovr) -> FieldReadOnly (ptr, ovr)
  | RequestDocumentError.UnexpectedMainResourceType (ptr, act, exp) -> UnexpectedType (ptr, act, exp)
  | RequestDocumentError.UnexpectedRelationshipType (ptr, act, exp) -> UnexpectedType (ptr, act, exp)
  | RequestDocumentError.ResourceIdNotAllowedForPost ptr -> ResourceIdNotAllowedForPost ptr
  | RequestDocumentError.ResourceIdIncorrectForPatch (ptr, act, exp) -> ResourceIdIncorrectForPatch (ptr, act, exp)
  | RequestDocumentError.RequiredFieldMissing ptr -> RequiredFieldMissing ptr
  | RequestDocumentError.AttributeInvalidParsed (ptr, msg) -> AttributeInvalidParsed (ptr, msg)
  | RequestDocumentError.AttributeInvalidEnum (ptr, inv, alw) -> AttributeInvalidEnum (ptr, inv, alw)
  | RequestDocumentError.RelationshipResourceNotFound (ptr, tp, id) -> RelationshipResourceNotFound (ptr, tp, id)


// Below is the function that maps from ApiError to a JSON-API error object.
// Note that we don't need to set the error object's Status property - that will
// be set to the returned numeric code later on in the error handler.


let getStatusAndError = function

  | InvalidAccept ->
      406,
      Error.createId "NegotiationFailure"
      |> Error.setTitle "Invalid Accept header"
      |> Error.setDetailf "The client must accept the JSON-API media type (%s)" MediaTypes.jsonApi

  | InvalidAcceptParams ->
      406,  // MUST return 406
      Error.createId "NegotiationFailure"
      |> Error.setTitle "Invalid JSON-API Accept params"
      |> Error.setDetail "The JSON-API media type in the Accept header must not be modified with media type parameters"

  | InvalidContentType ->
      415,
      Error.createId "NegotiationFailure"
      |> Error.setTitle "Invalid content type"
      |> Error.setDetailf "Request content must be sent with Content-Type set to the JSON-API media type (%s)" MediaTypes.jsonApi

  | InvalidContentTypeParams ->
      415,  // MUST return 415
      Error.createId "NegotiationFailure"
      |> Error.setTitle "Invalid JSON-API Content-Type params"
      |> Error.setDetail "The JSON-API media type in the Content-Type header must not be modified with media type parameters"

  | IllegalQueryParamName paramName ->
      400,  // MUST return 400
      Error.createId "QueryParamError"
      |> Error.setTitle "Invalid query parameter name"
      |> Error.setDetailf "'%s' is not an allowed name for a query parameter according to the JSON-API specification" paramName
      |> Error.setSourceParam paramName

  | QueryInvalidEnum (name, invalidValue, allowedValues) ->
      400,
      Error.createId "QueryParamError"
      |> Error.setTitle "Invalid query parameter value"
      |> Error.setDetailf
          "Query parameter '%s' got invalid value '%s', expected %s"
          name
          invalidValue
          (allowedValues |> List.map (sprintf "'%s'") |> String.concat ", ")
      |> Error.setSourceParam name
      |> Error.addMeta "allowedValues" allowedValues

  | QueryInvalidParsed (name, invalidValue, errMsg) ->
      400,
      Error.createId "QueryParamError"
      |> Error.setTitle "Invalid query parameter value"
      |> match errMsg with
         | Some msg -> Error.setDetailf "Query parameter '%s' got invalid value '%s'. Error message: %s" name invalidValue msg
         | None -> Error.setDetailf "Query parameter '%s' got invalid value '%s'" name invalidValue
      |> Error.setSourceAttr name

  | QueryTooLarge (name, invalidValue, max) ->
      400,
      Error.createId "QueryParamError"
      |> Error.setTitle "Query parameter value too large"
      |> Error.setDetailf "Query parameter '%s' got value %i, but must not be larger than %i" name invalidValue max
      |> Error.setSourceParam name

  | QueryTooSmall (name, invalidValue, min) ->
      400,
      Error.createId "QueryParamError"
      |> Error.setTitle "Query parameter value too small"
      |> Error.setDetailf "Query parameter '%s' got value %i, but must not be smaller than %i" name invalidValue min
      |> Error.setSourceParam name

  | QueryMissing name ->
      400,
      Error.createId "QueryParamError"
      |> Error.setTitle "Required query parameter missing"
      |> Error.setDetailf "Required query parameter '%s' was missing" name
      |> Error.setSourceParam name

  | QueryNotSingular (name, numProvidedValues) ->
      400,
      Error.createId "QueryParamError"
      |> Error.setTitle "Multiple values specified for singular query parameter"
      |> Error.setDetailf "Query parameter '%s' only supports a single value, but got %i values" name numProvidedValues
      |> Error.setSourceParam name

  | Malformed (ex, _) ->
      400,
      Error.createId "RequestDocumentError"
      |> Error.setTitle "Body malformed"
      |> Error.setDetail "The request body could not be parsed. Possible causes for this error include an empty body, incorrect syntax, invalid values for enumeration properties, or incompatible property data types (including 'null' for non-nullable numbers)."
      #if DEBUG
      |> Error.addMeta "exceptionMessage" ex.Message
      #endif

  | InvalidNull (pointer, isOverride) ->
      400,
      Error.createId "RequestDocumentError"
      |> Error.setTitle "Invalid null value"
      |> Error.setDetail ("This property may not be null" + if isOverride then " for this operation" else "")
      |> Error.setSourcePointer pointer

  | InvalidNullOrMissing pointer ->
      400,
      Error.createId "RequestDocumentError"
      |> Error.setTitle "Required property missing or null"
      |> Error.setDetail ("This property is required and may not be null")
      |> Error.setSourcePointer pointer

  | FieldReadOnly (pointer, isOverride) ->
      400,
      Error.createId "RequestDocumentError"
      |> Error.setTitle "Field read-only"
      |> Error.setDetail ("The field is read-only" + if isOverride then " for this operation" else "")
      |> Error.setSourcePointer pointer

  | UnexpectedType (pointer, actual, expected) ->
      409,  // MUST return 409
      Error.createId "RequestDocumentError"
      |> Error.setTitle "Incorrect resource type"
      |> Error.setDetailf
          "Unexpected type '%s', expected %s"
            actual
            (expected |> List.map (sprintf "'%s'") |> String.concat ", ")
      |> Error.setSourcePointer pointer
      |> Error.addMeta "allowedTypes" expected

  | ResourceIdNotAllowedForPost pointer ->
      403,  // MUST return 403
      Error.createId "RequestDocumentError"
      |> Error.setTitle "Resource ID not allowed for POST"
      |> Error.setDetail "This endpoint does not support creating resources with client-generated IDs"
      |> Error.setSourcePointer pointer

  | ResourceIdIncorrectForPatch (pointer, Some actual, expected) ->
      409,  // MUST return 409
      Error.createId "RequestDocumentError"
      |> Error.setTitle "Incorrect resource ID"
      |> Error.setDetailf "Expected resource ID '%s', but got '%s'" expected actual
      |> Error.setSourcePointer pointer

  | ResourceIdIncorrectForPatch (pointer, None, expected) ->
      409,  // MUST return 409
      Error.createId "RequestDocumentError"
      |> Error.setTitle "Missing resource ID"
      |> Error.setDetailf "Resource ID was missing, expected '%s'" expected
      |> Error.setSourcePointer pointer

  | RequiredFieldMissing pointer ->
      400,
      Error.createId "RequestDocumentError"
      |> Error.setTitle "Missing field"
      |> Error.setDetailf "A required field was missing"
      |> Error.setSourcePointer pointer

  | AttributeInvalidParsed (pointer, errMsg) ->
      400,
      Error.createId "RequestDocumentError"
      |> Error.setTitle "Invalid attribute value"
      |> match errMsg with
         | Some msg -> Error.setDetailf "Got invalid attribute value. Error message: %s" msg
         | None -> Error.setDetail "Got invalid attribute value"
      |> Error.setSourcePointer pointer

  | AttributeInvalidEnum (pointer, invalidValue, allowedValues) ->
      400,
      Error.createId "RequestDocumentError"
      |> Error.setTitle "Invalid attribute value"
      |> Error.setDetailf
          "Attribute got unexpected value '%s', expected %s"
          invalidValue
          (allowedValues |> List.map (sprintf "'%s'") |> String.concat ", ")
      |> Error.setSourcePointer pointer
      |> Error.addMeta "allowedValues" allowedValues

  | RelationshipResourceNotFound (pointer, resType, resId) ->
      404,  // MUST return 404
      Error.createId "ResourceNotFound"
      |> Error.setTitle "Related resource not found"
      |> Error.setDetailf "Relationship referred to non-existent resource with type '%s' and ID '%s'" resType resId
      |> Error.setSourcePointer pointer

  | ResourceNotFound ->
      404,  // MUST return 404
      Error.createId "ResourceNotFound"
      |> Error.setTitle "Resource not found"
      |> Error.setDetailf "No resource is accessible at this URL"

  | NotFound ->
      404,
      Error.createId "OperationNotFound"
      |> Error.setTitle "Operation not found"
      |> Error.setDetail "The requested operation (path + verb) does not exist"

  | Unauthorized ->
      401,
      Error.createId "Unauthorized"
      |> Error.setTitle "Unauthorized"
      |> Error.setDetail "The authorization was missing or invalid"

  | UnknownError ->
      500,
      Error.createId "UnknownError"
      |> Error.setTitle "Unknown error"
      |> Error.setDetail "An unknown error has occurred"
