﻿namespace FSharp.JsonApi

open System
open FSharp.JsonSkippable


/// Specifies the allowed resource type names for a relationship. Used when
/// validating JSON-API documents.
[<AttributeUsage(AttributeTargets.Property)>]
type AllowedTypesAttribute([<ParamArray>] typeNames: string[]) =
  inherit Attribute()
  member __.TypeNames = typeNames

/// Specifies that a to-one relationship's data can not be null. Used when
/// validating JSON-API documents.
[<AttributeUsage(AttributeTargets.Property)>]
type NotNullAttribute() =
  inherit Attribute()

/// Specifies that an attribute or relationship is read-only. Used when
/// validating JSON-API documents in requests.
[<AttributeUsage(AttributeTargets.Property)>]
type ReadOnlyAttribute() =
  inherit Attribute()

/// Specifies that an attribute or relationship is write-only. Used when
/// validating JSON-API documents in responses.
[<AttributeUsage(AttributeTargets.Property)>]
type WriteOnlyAttribute() =
  inherit Attribute()


[<RequireQualifiedAccess>]
type internal DocumentError =
  | InvalidNullPointer of jsonPointer: string * overridden: bool
  | InvalidRelationshipType of jsonPointer: string * invalidType: string * allowedTypes: string list
  | FieldReadOnly of jsonPointer: string * overridden: bool
  | FieldWriteOnly of jsonPointer: string * overridden: bool


/// Represents errors that can occur while validating a JSON-API request document.
[<RequireQualifiedAccess>]
type RequestDocumentError =
  /// A property was null where a null value is not allowed. If null is normally
  /// allowed but was overridden for this validation, overridden is true.
  | InvalidNullPointer of jsonPointer: string * overridden: bool
  /// An invalid resource type was encountered.
  | InvalidRelationshipType of jsonPointer: string * invalidType: string * allowedTypes: string list
  /// A resource attribute or relationship is read-only but was present in the
  /// request body. If this field is normally not read-only but was overridden
  /// for this validation, overridden is true.
  | FieldReadOnly of jsonPointer: string * overridden: bool

  static member internal OfDocumentError = function
    | DocumentError.InvalidNullPointer (ptr, ov) -> InvalidNullPointer (ptr, ov) |> Some
    | DocumentError.InvalidRelationshipType (ptr, inv, al) -> InvalidRelationshipType (ptr, inv, al) |> Some
    | DocumentError.FieldReadOnly (ptr, ov) -> FieldReadOnly (ptr, ov) |> Some
    | DocumentError.FieldWriteOnly _ -> None


/// Represents errors that can occur while validating a JSON-API response document.
[<RequireQualifiedAccess>]
type ResponseDocumentError =
  /// A property was null where a null value is not allowed. If null is normally
  /// allowed but was overridden for this validation, overridden is true.
  | InvalidNullPointer of jsonPointer: string * overridden: bool
  /// An invalid resource type was encountered.
  | InvalidRelationshipType of jsonPointer: string * invalidType: string * allowedTypes: string list
  /// A resource attribute or relationship is write-only but was present in the
  /// response body. If this field is normally not write-only but was overridden
  /// for this validation, overridden is true.
  | FieldWriteOnly of jsonPointer: string * overridden: bool

  static member internal OfDocumentError = function
    | DocumentError.InvalidNullPointer (ptr, ov) -> InvalidNullPointer (ptr, ov) |> Some
    | DocumentError.InvalidRelationshipType (ptr, inv, al) -> InvalidRelationshipType (ptr, inv, al) |> Some
    | DocumentError.FieldReadOnly _ -> None
    | DocumentError.FieldWriteOnly (ptr, ov) -> FieldWriteOnly (ptr, ov) |> Some


type internal ValidationType =
  | Request
  | Response

type internal FieldValidator = ValidationContext -> obj -> DocumentError list

and internal ValidationContext =
  { Pointer: string
    AllRegisteredTypes: Type list
    CurrentType: TypeName option
    CurrentField: FieldName option
    ReadOnly: Set<TypeName * FieldName>
    WriteOnly: Set<TypeName * FieldName>
    NotNull: Set<TypeName * FieldName>
    ReadOnlyIsOverridden: Set<TypeName * FieldName>
    WriteOnlyIsOverridden: Set<TypeName * FieldName>
    NotNullIsOverridden: Set<TypeName * FieldName>
    AllowedRelationshipTypes: Map<TypeName * FieldName, Set<TypeName>>
    AttributeValidators: Map<TypeName, FieldValidator list>
    RelationshipValidators: Map<TypeName, FieldValidator list>
    ValidationType: ValidationType }

  member internal this.CurrentFieldIsReadOnly =
    match this.CurrentType, this.CurrentField with
    | Some t, Some f -> this.ReadOnly.Contains (t, f)
    | _ -> false

  member internal this.CurrentFieldIsWriteOnly =
    match this.CurrentType, this.CurrentField with
    | Some t, Some f -> this.WriteOnly.Contains (t, f)
    | _ -> false

  member internal this.CurrentFieldIsNotNull =
    match this.CurrentType, this.CurrentField with
    | Some t, Some f -> this.NotNull.Contains (t, f)
    | _ -> false

  member internal this.CurrentFieldReadOnlyIsOverride =
    match this.CurrentType, this.CurrentField with
    | Some t, Some f -> this.ReadOnlyIsOverridden.Contains (t, f)
    | _ -> false

  member internal this.CurrentFieldWriteOnlyIsOverride =
    match this.CurrentType, this.CurrentField with
    | Some t, Some f -> this.WriteOnlyIsOverridden.Contains (t, f)
    | _ -> false

  member internal this.CurrentFieldNotNullIsOverride =
    match this.CurrentType, this.CurrentField with
    | Some t, Some f -> this.NotNullIsOverridden.Contains (t, f)
    | _ -> false

  member internal this.CurrentRelAllowedTypes =
    match this.CurrentType, this.CurrentField with
    | Some t, Some f -> this.AllowedRelationshipTypes.TryFind (t, f)
    | _ -> None

  member internal this.CurrentAttributeValidators =
    this.CurrentType |> Option.bind this.AttributeValidators.TryFind |> Option.defaultValue []

  member internal this.CurrentRelationshipValidators =
    this.CurrentType |> Option.bind this.RelationshipValidators.TryFind |> Option.defaultValue []


module internal ValidationContext =

  /// Specifies that the specified attribute or relationship on the specified
  /// type should or should not be read-only, overriding any ReadOnlyAttribute
  /// on that field.
  let overrideReadOnly typeName fieldName isReadOnly ctx =
    { ctx with
        ReadOnly =
          if isReadOnly then ctx.ReadOnly.Add(typeName, fieldName)
          else ctx.ReadOnly.Remove(typeName, fieldName)
        ReadOnlyIsOverridden = ctx.ReadOnlyIsOverridden.Add(typeName, fieldName) }

  /// Specifies that the specified attribute or relationship on the specified
  /// type should or should not be write-only, overriding any WriteOnlyAttribute
  /// on that field.
  let overrideWriteOnly typeName fieldName isWriteOnly ctx =
    { ctx with
        WriteOnly =
          if isWriteOnly then ctx.WriteOnly.Add(typeName, fieldName)
          else ctx.WriteOnly.Remove(typeName, fieldName)
        WriteOnlyIsOverridden = ctx.WriteOnlyIsOverridden.Add(typeName, fieldName) }

  /// Specifies that the specified relationship on the specified type may or
  /// may not be null, overriding any NotNullAttribute on that relationship.
  let overrideNotNull typeName relName isNotNull ctx =
    { ctx with
        NotNull =
          if isNotNull then ctx.NotNull.Add(typeName, relName)
          else ctx.NotNull.Remove(typeName, relName)
        NotNullIsOverridden = ctx.NotNullIsOverridden.Add(typeName, relName) }



[<RequireQualifiedAccess>]
module internal Validate =


  /// Returns validation errors for a JSON-API object.
  let jsonApi ctx (jsonApi: JsonApi) =
    [
      if isNull jsonApi.Version then
        yield DocumentError.InvalidNullPointer (ctx.Pointer + "/version", false)
      if isIncludedNull jsonApi.Meta then
        yield DocumentError.InvalidNullPointer (ctx.Pointer + "/meta", false)
    ]


  /// Returns validation errors for a link object.
  let link ctx (link: Link) =
    [
      if isIncludedNull link.Meta then
        yield DocumentError.InvalidNullPointer (ctx.Pointer + "/meta", false)
    ]


  /// Returns validation errors for a link collection.
  let links ctx (Links links) =
    links
    |> Map.toList
    |> List.collect (fun (name, l) ->
        l
        |> link { ctx with Pointer = sprintf "%s/%s" ctx.Pointer name}
    )


  /// Returns validation errors for an error source object.
  let errorSource ctx (es: ErrorSource) =
    [
      if isIncludedNull es.Pointer then
        yield DocumentError.InvalidNullPointer (ctx.Pointer + "/pointer", false)
      if isIncludedNull es.Parameter then
        yield DocumentError.InvalidNullPointer (ctx.Pointer + "/parameter", false)
    ]


  /// Returns validation errors for an error object.
  let error ctx (err: Error) =
    [
      if isIncludedNull err.Id then yield DocumentError.InvalidNullPointer (ctx.Pointer + "/id", false)
      if isIncludedNull err.Links then yield DocumentError.InvalidNullPointer (ctx.Pointer + "/links", false)
      if isIncludedNull err.Status then yield DocumentError.InvalidNullPointer (ctx.Pointer + "/status", false)
      if isIncludedNull err.Code then yield DocumentError.InvalidNullPointer (ctx.Pointer + "/code", false)
      if isIncludedNull err.Title then yield DocumentError.InvalidNullPointer (ctx.Pointer + "/title", false)
      if isIncludedNull err.Detail then yield DocumentError.InvalidNullPointer (ctx.Pointer + "/detail", false)
      if isIncludedNull err.Source then yield DocumentError.InvalidNullPointer (ctx.Pointer + "/source", false)
      if isIncludedNull err.Meta then yield DocumentError.InvalidNullPointer (ctx.Pointer + "/meta", false)
    ]


  /// Returns validation errors for a resource identifier.
  let resourceIdentifier ctx (id: ResourceIdentifier) =
    [
      if isNull id.Type then
        yield DocumentError.InvalidNullPointer (ctx.Pointer + "/type", false)
      else
        match ctx.CurrentRelAllowedTypes with
        | None -> ()
        | Some types when types.Contains id.Type -> ()
        | Some types -> yield DocumentError.InvalidRelationshipType (ctx.Pointer + "/type", id.Type, types |> Set.toList)
      if isNull id.Id then yield DocumentError.InvalidNullPointer (ctx.Pointer + "/id", false)
    ]


  /// Returns validation errors for a to-one relationship object.
  let toOne ctx (rel: ToOne) =
      [
        match rel.Links with
        | Skip -> ()
        | Include BoxedNull -> yield DocumentError.InvalidNullPointer (ctx.Pointer + "/links", false)
        | Include ls -> yield! ls |> links { ctx with Pointer = ctx.Pointer + "/links" }

        match rel.Data with
        | Skip -> ()
        | Include None when ctx.CurrentFieldIsNotNull ->
            yield DocumentError.InvalidNullPointer (ctx.Pointer + "/data", false)
        | Include None -> ()
        | Include (Some resId) ->
          yield! resId |> resourceIdentifier { ctx with Pointer = ctx.Pointer + "/data" }

        if isIncludedNull rel.Meta then yield DocumentError.InvalidNullPointer (ctx.Pointer + "/meta", false)
      ]


  /// Returns validation errors for a to-many relationship object.
  let toMany ctx (rel: ToMany) =
    [
      match rel.Links with
      | Skip -> ()
      | Include BoxedNull -> yield DocumentError.InvalidNullPointer (ctx.Pointer + "/links", false)
      | Include ls -> yield! ls |> links { ctx with Pointer = ctx.Pointer + "/links" }

      match rel.Data with
      | Skip -> ()
      | Include resIds ->
          yield!
            resIds
            |> List.mapi (fun i resId ->
                  if isBoxedNull resId then [DocumentError.InvalidNullPointer (sprintf "%s/data/%i" ctx.Pointer i, false)]
                  else resId |> resourceIdentifier { ctx with Pointer = sprintf "%s/data/%i" ctx.Pointer i }
            )
            |> List.collect id

      if isIncludedNull rel.Meta then yield DocumentError.InvalidNullPointer (ctx.Pointer + "/meta", false)
    ]


  /// Returns validation errors for a resource object.
  let resource ctx (res: Resource<'attrs, 'rels>) =
    [
      if isIncludedNull res.Type then yield DocumentError.InvalidNullPointer (ctx.Pointer + "/type", false)
      if isIncludedNull res.Id then yield DocumentError.InvalidNullPointer (ctx.Pointer + "/id", false)

      match res.Attributes with
      | Include BoxedNull -> yield DocumentError.InvalidNullPointer (ctx.Pointer + "/attributes", false)
      | Include attrs ->
          yield!
            ctx.CurrentAttributeValidators
            |> List.collect (fun validate ->
                validate
                  { ctx with Pointer = ctx.Pointer + "/attributes" }
                  attrs
            )
      | _ -> ()

      match res.Links with
      | Skip -> ()
      | Include BoxedNull -> yield DocumentError.InvalidNullPointer (ctx.Pointer + "/links", false)
      | Include ls -> yield! ls |> links { ctx with Pointer = ctx.Pointer + "/links" }

      match res.Relationships with
      | Include BoxedNull -> yield DocumentError.InvalidNullPointer (ctx.Pointer + "/relationships", false)
      | Include rels ->
          yield!
            ctx.CurrentRelationshipValidators
            |> List.collect (fun validate ->
                validate
                  { ctx with Pointer = ctx.Pointer + "/relationships" }
                  rels
            )
      | Skip -> ()

      if isIncludedNull res.Meta then yield DocumentError.InvalidNullPointer (ctx.Pointer + "/meta", false)
    ]


  /// Returns validation errors for a resource document.
  let resourceDocument ctx (doc: ResourceDocument) =
    [
      match doc.JsonApi with
      | Skip -> ()
      | Include BoxedNull -> yield DocumentError.InvalidNullPointer ("/jsonapi", false)
      | Include japi -> yield! jsonApi { ctx with Pointer = "/jsonapi" } japi

      match doc.Links with
      | Skip -> ()
      | Include BoxedNull -> yield DocumentError.InvalidNullPointer ("/links", false)
      | Include ls -> yield! links { ctx with Pointer = "/links" } ls

      if isIncludedNull doc.Meta then yield DocumentError.InvalidNullPointer ("/meta", false)

      match doc.Data with
      | None -> ()
      | Some res ->
          yield!
            resource
              { ctx with
                  Pointer = "/data"
                  CurrentType = res.Type |> Skippable.toOption }
              res

      match doc.Included with
      | Skip -> ()
      | Include resources ->
          yield!
            resources
            |> List.mapi (fun i res ->
                  if isBoxedNull res then [DocumentError.InvalidNullPointer (sprintf "/included/%i" i, false)]
                  else
                    resource
                      { ctx with
                          Pointer = sprintf "/included/%i" i
                          CurrentType = res.Type |> Skippable.toOption }
                      res
            )
            |> List.collect id
    ]


  /// Returns validation errors for a resource collection document.
  let resourceCollectionDocument ctx (doc: ResourceCollectionDocument) =
    [
      match doc.JsonApi with
      | Skip -> ()
      | Include BoxedNull -> yield DocumentError.InvalidNullPointer ("/jsonapi", false)
      | Include japi -> yield! jsonApi { ctx with Pointer = "/jsonapi" } japi

      match doc.Links with
      | Skip -> ()
      | Include BoxedNull -> yield DocumentError.InvalidNullPointer ("/links", false)
      | Include ls -> yield! links { ctx with Pointer = "/links" } ls

      if isIncludedNull doc.Meta then yield DocumentError.InvalidNullPointer ("/meta", false)

      match doc.Data with
      | BoxedNull -> yield DocumentError.InvalidNullPointer ("/data", false)
      | resources ->
          yield!
            resources
            |> List.mapi (fun i res ->
                  if isBoxedNull res then [DocumentError.InvalidNullPointer (sprintf "/data/%i" i, false)]
                  else
                    resource
                      { ctx with
                          Pointer = sprintf "/data/%i" i
                          CurrentType = res.Type |> Skippable.toOption }
                      res
            )
            |> List.collect id

      match doc.Included with
      | Skip -> ()
      | Include resources ->
          yield!
            resources
            |> List.mapi (fun i res ->
                  if isBoxedNull res then [DocumentError.InvalidNullPointer (sprintf "/included/%i" i, false)]
                  else
                    resource
                      { ctx with
                          Pointer = sprintf "/included/%i" i
                          CurrentType = res.Type |> Skippable.toOption }
                      res
            )
            |> List.collect id
    ]


  /// Returns validation errors for a resource identifier document.
  let resourceIdentifierDocument ctx (doc: ResourceIdentifierDocument) =
    [
      match doc.JsonApi with
      | Skip -> ()
      | Include BoxedNull -> yield DocumentError.InvalidNullPointer ("/jsonapi", false)
      | Include japi -> yield! jsonApi { ctx with Pointer = "/jsonapi" } japi

      match doc.Links with
      | Skip -> ()
      | Include BoxedNull -> yield DocumentError.InvalidNullPointer ("/links", false)
      | Include ls -> yield! links { ctx with Pointer = "/links" } ls

      if isIncludedNull doc.Meta then yield DocumentError.InvalidNullPointer ("/meta", false)

      match doc.Data with
      | None -> ()
      | Some resId ->
          yield! resourceIdentifier { ctx with Pointer = "/data" } resId
    ]


  /// Returns validation errors for a resource identifier collection document.
  let resourceIdentifierCollectionDocument ctx (doc: ResourceIdentifierCollectionDocument) =
    [
      match doc.JsonApi with
      | Skip -> ()
      | Include BoxedNull -> yield DocumentError.InvalidNullPointer ("/jsonapi", false)
      | Include japi -> yield! jsonApi { ctx with Pointer = "/jsonapi" } japi

      match doc.Links with
      | Skip -> ()
      | Include BoxedNull -> yield DocumentError.InvalidNullPointer ("/links", false)
      | Include ls -> yield! links { ctx with Pointer = "/links" } ls

      if isIncludedNull doc.Meta then yield DocumentError.InvalidNullPointer ("/meta", false)

      match doc.Data with
      | BoxedNull -> yield DocumentError.InvalidNullPointer ("/data", false)
      | resIds ->
          yield!
            resIds
            |> List.mapi (fun i resId ->
                  if isBoxedNull resId then [DocumentError.InvalidNullPointer (sprintf "/data/%i" i, false)]
                  else resourceIdentifier { ctx with Pointer = sprintf "/data/%i" i } resId
            )
            |> List.collect id
    ]


  /// Returns validation errors for an error document.
  let errorDocument ctx (doc: ErrorDocument) =
    [
      match doc.JsonApi with
      | Skip -> ()
      | Include BoxedNull -> yield DocumentError.InvalidNullPointer ("/jsonapi", false)
      | Include japi -> yield! jsonApi { ctx with Pointer = "/jsonapi" } japi

      match doc.Errors with
      | BoxedNull -> yield DocumentError.InvalidNullPointer ("/errors", false)
      | errors ->
          yield!
            errors
            |> List.mapi (fun i err ->
                  if isBoxedNull err then [DocumentError.InvalidNullPointer (sprintf "/errors/%i" i, false)]
                  else error { ctx with Pointer = sprintf "/errors/%i" i} err
            )
            |> List.collect id

      match doc.Links with
      | Skip -> ()
      | Include BoxedNull -> yield DocumentError.InvalidNullPointer ("/links", false)
      | Include ls -> yield! links { ctx with Pointer = "/links" } ls

      if isIncludedNull doc.Meta then yield DocumentError.InvalidNullPointer ("/meta", false)
    ]


  /// Returns validation errors for a JSON-API document.
  let document ctx (doc: #IJsonApiDocument) =
    match box doc with
    | :? ResourceDocument as d -> resourceDocument ctx d
    | :? ResourceCollectionDocument as d -> resourceCollectionDocument ctx d
    | :? ResourceIdentifierDocument as d -> resourceIdentifierDocument ctx d
    | :? ResourceIdentifierCollectionDocument as d -> resourceIdentifierCollectionDocument ctx d
    | :? ErrorDocument as d -> errorDocument ctx d
    | doc -> failwithf "Can only validate built-in JSON-API documents, but received unsupported document type '%s'" (doc.GetType().FullName)
