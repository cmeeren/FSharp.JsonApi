namespace FSharp.JsonApi

open FSharp.JsonSkippable


[<AutoOpen>]
module private RelationshipHelpers =

  let relPointer relName =
    sprintf "/data/relationships/%s" relName

  let dataPointer relName =
    sprintf "/data/relationships/%s/data" relName

  let getId (id: ResourceIdentifier) =
    id.Id


/// Helpers for parsing relationships of a single-resource document's main
/// resource.
type Relationship =

  /// Gets the resource identifier from a nullable to-one relationship.
  static member Get
      ( value: Skippable<ToOne>
      ) : Result<ResourceIdentifier option option, RequestDocumentError list> =
    match value with
    | Skip -> Ok None
    | Include r ->
        match r.Data with
        | Skip | Include None -> Ok (Some None)
        | Include (Some id) -> Ok (Some (Some id))

  /// Gets a nullable to-one relationship. If map returns None, it will be
  /// interpreted as the related resource not existing. The inner option is part
  /// of the actual relationship data, while the outer option signifies if the
  /// relationship was included or not.
  static member Get
      ( name: string,
        value: Skippable<ToOne>,
        map: ResourceIdentifier -> 'a option
      ) : Result<'a option option, RequestDocumentError list> =
    match Relationship.Get(value) with
    | Ok (Some (Some id)) ->
        match map id with
        | None -> Error [RequestDocumentError.RelationshipResourceNotFound (dataPointer name, id.Type, id.Id)]
        | Some x -> Ok (Some (Some x))
    | Ok (Some None) -> Ok (Some None)
    | Ok None -> Ok None
    | Error errs -> Error errs

  /// Gets a nullable to-one relationship. If mapId returns None, it will be
  /// interpreted as the related resource not existing. The inner option is part
  /// of the actual relationship data, while the outer option signifies if the
  /// relationship was included or not.
  static member Get
      ( name: string,
        value: Skippable<ToOne>,
        mapId: string -> 'a option
      ) : Result<'a option option, RequestDocumentError list> =
    Relationship.Get(name, value, getId >> mapId)

  /// Gets a nullable to-one relationship in two steps, the second being async.
  /// If either map or asyncMap returns None, it will be interpreted as the
  /// related resource not existing. The inner option is part of the actual
  /// relationship data, while the outer option signifies if the relationship
  /// was included or not.
  static member Get
      ( name: string,
        value: Skippable<ToOne>,
        map: ResourceIdentifier -> 'a option,
        asyncMap: 'a -> Async<'b option>
      ) : Async<Result<'b option option, RequestDocumentError list>> =
    async {
      match Relationship.Get(value) with
      | Ok (Some (Some id)) ->
          match map id with
          | None -> return Error [RequestDocumentError.RelationshipResourceNotFound (dataPointer name, id.Type, id.Id)]
          | Some a ->
              match! asyncMap a with
              | None -> return Error [RequestDocumentError.RelationshipResourceNotFound (dataPointer name, id.Type, id.Id)]
              | Some b -> return Ok (Some (Some b))
      | Ok (Some None) -> return Ok (Some None)
      | Ok None -> return Ok None
      | Error errs -> return Error errs
    }

  /// Gets a nullable to-one relationship in two steps, the second being async.
  /// If either mapId or asyncMap returns None, it will be interpreted as the
  /// related resource not existing. The inner option is part of the actual
  /// relationship data, while the outer option signifies if the relationship
  /// was included or not.
  static member Get
      ( name: string,
        value: Skippable<ToOne>,
        mapId: string -> 'a option,
        asyncMap: 'a -> Async<'b option>
      ) : Async<Result<'b option option, RequestDocumentError list>> =
    Relationship.Get(name, value, getId >> mapId, asyncMap)

  /// Gets the resource identifier from a non-nullable to-one relationship. If
  /// nullableByDefault is true, the returned InvalidNull error will have
  /// overridden = true.
  static member GetNonNull
      ( name: string,
        value: Skippable<ToOne>,
        ?nullableByDefault: bool
      ) : Result<ResourceIdentifier option, RequestDocumentError list> =
    match value with
    | Skip -> Ok None
    | Include r ->
        match r.Data with
        | Skip | Include None -> Error [RequestDocumentError.InvalidNull (dataPointer name, defaultArg nullableByDefault false)]
        | Include (Some id) -> Ok (Some id)

  /// Gets a non-nullable to-one relationship. If map returns None, it will be
  /// interpreted as the related resource not existing. If nullableByDefault is
  /// true, the returned InvalidNull error will have overridden = true.
  static member GetNonNull
      ( name: string,
        value: Skippable<ToOne>,
        map: ResourceIdentifier -> 'a option,
        ?nullableByDefault: bool
      ) : Result<'a option, RequestDocumentError list> =
    match value with
    | Skip -> Ok None
    | Include r ->
        match r.Data with
        | Skip | Include None -> Error [RequestDocumentError.InvalidNull (dataPointer name, defaultArg nullableByDefault false)]
        | Include (Some id) ->
            match map id with
            | None -> Error [RequestDocumentError.RelationshipResourceNotFound (dataPointer name, id.Type, id.Id)]
            | Some x -> Ok (Some x)

  /// Gets a non-nullable to-one relationship. If mapId returns None, it will be
  /// interpreted as the related resource not existing. If nullableByDefault is
  /// true, the returned InvalidNull error will have overridden = true.
  static member GetNonNull
      ( name: string,
        value: Skippable<ToOne>,
        mapId: string -> 'a option,
        ?nullableByDefault: bool
      ) : Result<'a option, RequestDocumentError list> =
    Relationship.GetNonNull(name, value, getId >> mapId, ?nullableByDefault = nullableByDefault)

  /// Gets a non-nullable to-one relationship in two steps, the second being
  /// async. If either map or asyncMap returns None, it will be interpreted as
  /// the related resource not existing. If nullableByDefault is true, the
  /// returned InvalidNull error will have overridden = true.
  static member GetNonNull
      ( name: string,
        value: Skippable<ToOne>,
        map: ResourceIdentifier -> 'a option,
        asyncMap: 'a -> Async<'b option>,
        ?nullableByDefault: bool
      ) : Async<Result<'b option, RequestDocumentError list>> =
    async {
      match Relationship.GetNonNull(name, value, ?nullableByDefault = nullableByDefault) with
      | Ok (Some id) ->
          match map id with
          | None -> return Error [RequestDocumentError.RelationshipResourceNotFound (dataPointer name, id.Type, id.Id)]
          | Some a ->
              match! asyncMap a with
              | None -> return Error [RequestDocumentError.RelationshipResourceNotFound (dataPointer name, id.Type, id.Id)]
              | Some b -> return Ok (Some b)
      | Ok None -> return Ok None
      | Error errs -> return Error errs
    }

  /// Gets a non-nullable to-one relationship in two steps, the second being
  /// async. If either mapId or asyncMap returns None, it will be interpreted as
  /// the related resource not existing. If nullableByDefault is true, the
  /// returned InvalidNull error will have overridden = true.
  static member GetNonNull
      ( name: string,
        value: Skippable<ToOne>,
        mapId: string -> 'a option,
        asyncMap: 'a -> Async<'b option>,
        ?nullableByDefault: bool
      ) : Async<Result<'b option, RequestDocumentError list>> =
    Relationship.GetNonNull(name, value, getId >> mapId, asyncMap, ?nullableByDefault = nullableByDefault)

  /// Gets the resource identifier from a required, nullable to-one
  /// relationship.
  static member Require
      ( name: string,
        value: Skippable<ToOne>
      ) : Result<ResourceIdentifier option, RequestDocumentError list> =
    match value with
    | Skip -> Error [RequestDocumentError.RequiredFieldMissing (relPointer name)]
    | Include r ->
        match r.Data with
        | Skip | Include None -> Ok None
        | Include (Some id) -> Ok (Some id)

  /// Gets a required, nullable to-one relationship. If map returns None, it
  /// will be interpreted as the related resource not existing.
  static member Require
      ( name: string,
        value: Skippable<ToOne>,
        map: ResourceIdentifier -> 'a option
      ) : Result<'a option, RequestDocumentError list> =
    match Relationship.Require(name, value) with
    | Ok (Some id) ->
        match map id with
        | None -> Error [RequestDocumentError.RelationshipResourceNotFound (dataPointer name, id.Type, id.Id)]
        | Some x -> Ok (Some x)
    | Ok None -> Ok None
    | Error errs -> Error errs

  /// Gets a required, nullable to-one relationship. If mapId returns None, it
  /// will be interpreted as the related resource not existing.
  static member Require
      ( name: string,
        value: Skippable<ToOne>,
        mapId: string -> 'a option
      ) : Result<'a option, RequestDocumentError list> =
    Relationship.Require(name, value, getId >> mapId)

  /// Gets a required, nullable to-one relationship in two steps, the second
  /// being async. If either map or asyncMap returns None, it will be
  /// interpreted as the related resource not existing.
  static member Require
      ( name: string,
        value: Skippable<ToOne>,
        map: ResourceIdentifier -> 'a option,
        asyncMap: 'a -> Async<'b option>
      ) : Async<Result<'b option, RequestDocumentError list>> =
    async {
      match Relationship.Require(name, value) with
      | Ok (Some id) ->
          match map id with
          | None -> return Error [RequestDocumentError.RelationshipResourceNotFound (dataPointer name, id.Type, id.Id)]
          | Some a ->
              match! asyncMap a with
              | None -> return Error [RequestDocumentError.RelationshipResourceNotFound (dataPointer name, id.Type, id.Id)]
              | Some b -> return Ok (Some b)
      | Ok None -> return Ok None
      | Error errs -> return Error errs
    }

  /// Gets a required, nullable to-one relationship in two steps, the second
  /// being async. If either mapId or asyncMap returns None, it will be
  /// interpreted as the related resource not existing.
  static member Require
      ( name: string,
        value: Skippable<ToOne>,
        mapId: string -> 'a option,
        asyncMap: 'a -> Async<'b option>
      ) : Async<Result<'b option, RequestDocumentError list>> =
    Relationship.Require(name, value, getId >> mapId, asyncMap)

  /// Gets the resource identifier from a required, non-nullable to-one
  /// relationship. If nullableByDefault is true, the returned InvalidNull error
  /// will have overridden = true.
  static member RequireNonNull
      ( name: string,
        value: Skippable<ToOne>,
        ?nullableByDefault: bool
      ) : Result<ResourceIdentifier, RequestDocumentError list> =
    match value with
    | Skip -> Error [RequestDocumentError.RequiredFieldMissing (relPointer name)]
    | Include r ->
        match r.Data with
        | Skip | Include None -> Error [RequestDocumentError.InvalidNull (dataPointer name, defaultArg nullableByDefault false)]
        | Include (Some id) -> Ok id

  /// Gets a required, non-nullable to-one relationship. If map returns None, it
  /// will be interpreted as the related resource not existing. If
  /// nullableByDefault is true, the returned InvalidNull error will have
  /// overridden = true.
  static member RequireNonNull
      ( name: string,
        value: Skippable<ToOne>,
        map: ResourceIdentifier -> 'a option,
        ?nullableByDefault: bool
      ) : Result<'a, RequestDocumentError list> =
    match Relationship.RequireNonNull(name, value, ?nullableByDefault = nullableByDefault) with
    | Ok id ->
        match map id with
        | None -> Error [RequestDocumentError.RelationshipResourceNotFound (dataPointer name, id.Type, id.Id)]
        | Some x -> Ok x
    | Error errs -> Error errs

  /// Gets a required, non-nullable to-one relationship. If mapId returns None,
  /// it will be interpreted as the related resource not existing. If
  /// nullableByDefault is true, the returned InvalidNull error will have
  /// overridden = true.
  static member RequireNonNull
      ( name: string,
        value: Skippable<ToOne>,
        mapId: string -> 'a option,
        ?nullableByDefault: bool
      ) : Result<'a, RequestDocumentError list> =
    Relationship.RequireNonNull(name, value, getId >> mapId, ?nullableByDefault = nullableByDefault)

  /// Gets a required, non-nullable to-one relationship in two steps, the second
  /// being async. If either map or asyncMap returns None, it will be
  /// interpreted as the related resource not existing. If nullableByDefault is
  /// true, the returned InvalidNull error will have overridden = true.
  static member RequireNonNull
      ( name: string,
        value: Skippable<ToOne>,
        map: ResourceIdentifier -> 'a option,
        asyncMap: 'a -> Async<'b option>,
        ?nullableByDefault: bool
      ) : Async<Result<'b, RequestDocumentError list>> =
    async {
      match Relationship.RequireNonNull(name, value, ?nullableByDefault = nullableByDefault) with
      | Ok id ->
          match map id with
          | None -> return Error [RequestDocumentError.RelationshipResourceNotFound (dataPointer name, id.Type, id.Id)]
          | Some a ->
              match! asyncMap a with
              | None -> return Error [RequestDocumentError.RelationshipResourceNotFound (dataPointer name, id.Type, id.Id)]
              | Some b -> return Ok b
      | Error errs -> return Error errs
    }

  /// Gets a required, non-nullable to-one relationship in two steps, the second
  /// being async. If either mapId or asyncMap returns None, it will be
  /// interpreted as the related resource not existing. If nullableByDefault is
  /// true, the returned InvalidNull error will have overridden = true.
  static member RequireNonNull
      ( name: string,
        value: Skippable<ToOne>,
        mapId: string -> 'a option,
        asyncMap: 'a -> Async<'b option>,
        ?nullableByDefault: bool
      ) : Async<Result<'b, RequestDocumentError list>> =
    Relationship.RequireNonNull(name, value, getId >> mapId, asyncMap, ?nullableByDefault = nullableByDefault)
