namespace FSharp.JsonApi

open FSharp.JsonSkippable


[<AutoOpen>]
module private RelationshipHelpers =

  let relPointer relName =
    sprintf "/data/relationships/%s" relName

  let dataPointer relName =
    sprintf "/data/relationships/%s/data" relName


/// Helpers for parsing relationships of a single-resource document's main
/// resource.
type Relationship =

  /// Parses a nullable to-one relationship. If parseId returns None, it will be
  /// interpreted as the related resource not existing. The inner option is part
  /// of the actual relationship data, while the outer option signifies if the
  /// relationship was included or not.
  static member Get
      ( name: string,
        value: Skippable<ToOne>,
        parseId: string -> 'a option
      ) : Result<'a option option, RequestDocumentError list> =
    match value with
    | Skip -> Ok None
    | Include r ->
        match r.Data with
        | Skip | Include None -> Ok (Some None)
        | Include (Some d) ->
            match parseId d.Id with
            | None -> Error [RequestDocumentError.RelationshipResourceNotFound (dataPointer name, name, d.Type, d.Id)]
            | Some x -> Ok (Some (Some x))

  /// Parses a nullable to-one relationship in two steps, the second being
  /// async. If either parseId or asyncMap returns None, it will be interpreted
  /// as the related resource not existing. The inner option is part of the
  /// actual relationship data, while the outer option signifies if the
  /// relationship was included or not.
  static member Get
      ( name: string,
        value: Skippable<ToOne>,
        parseId: string -> 'a option,
        asyncMap: 'a -> Async<'b option>
      ) : Async<Result<'b option option, RequestDocumentError list>> =
    async {
      match value with
      | Skip -> return Ok None
      | Include r ->
          match r.Data with
          | Skip | Include None -> return Ok (Some None)
          | Include (Some d) ->
              match parseId d.Id with
              | None -> return Error [RequestDocumentError.RelationshipResourceNotFound (dataPointer name, name, d.Type, d.Id)]
              | Some x ->
                  match! asyncMap x with
                  | None -> return Error [RequestDocumentError.RelationshipResourceNotFound (dataPointer name, name, d.Type, d.Id)]
                  | Some x -> return Ok (Some (Some x))
    }

  /// Parses a non-nullable to-one relationship. If parseId returns None, it
  /// will be interpreted as the related resource not existing. If
  /// nullableByDefault is true, the returned InvalidNullPointer error will have
  /// overridden = true.
  static member GetNonNull
      ( name: string,
        value: Skippable<ToOne>,
        parseId: string -> 'a option,
        ?nullableByDefault: bool
      ) : Result<'a option, RequestDocumentError list> =
    match value with
    | Skip -> Ok None
    | Include r ->
        match r.Data with
        | Skip | Include None -> Error [RequestDocumentError.InvalidNullPointer (dataPointer name, defaultArg nullableByDefault false)]
        | Include (Some d) ->
            match parseId d.Id with
            | None -> Error [RequestDocumentError.RelationshipResourceNotFound (dataPointer name, name, d.Type, d.Id)]
            | Some x -> Ok (Some x)

  /// Parses a non-nullable to-one relationship in two steps, the second being
  /// async. If either parseId or asyncMap returns None, it will be interpreted
  /// as the related resource not existing. If nullableByDefault is true, the
  /// returned InvalidNullPointer error will have overridden = true.
  static member GetNonNull
      ( name: string,
        value: Skippable<ToOne>,
        parseId: string -> 'a option,
        asyncMap: 'a -> Async<'b option>,
        ?nullableByDefault: bool
      ) : Async<Result<'b option, RequestDocumentError list>> =
    async {
      match value with
      | Skip -> return Ok None
      | Include r ->
          match r.Data with
          | Skip | Include None -> return Error [RequestDocumentError.InvalidNullPointer (dataPointer name, defaultArg nullableByDefault false)]
          | Include (Some d) ->
              match parseId d.Id with
              | None -> return Error [RequestDocumentError.RelationshipResourceNotFound (dataPointer name, name, d.Type, d.Id)]
              | Some x ->
                  match! asyncMap x with
                  | None -> return Error [RequestDocumentError.RelationshipResourceNotFound (dataPointer name, name, d.Type, d.Id)]
                  | Some x -> return Ok (Some x)
    }

  /// Parses a required, nullable to-one relationship. If parseId returns None,
  /// it will be interpreted as the related resource not existing.
  static member Require
      ( name: string,
        value: Skippable<ToOne>,
        parseId: string -> 'a option
      ) : Result<'a option, RequestDocumentError list> =
    match value with
    | Skip -> Error [RequestDocumentError.RequiredFieldMissing (relPointer name, name)]
    | Include r ->
        match r.Data with
        | Skip | Include None -> Ok None
        | Include (Some d) ->
            match parseId d.Id with
            | None -> Error [RequestDocumentError.RelationshipResourceNotFound (dataPointer name, name, d.Type, d.Id)]
            | Some x -> Ok (Some x)

  /// Parses a required, nullable to-one relationship in two steps, the second
  /// being async. If either parseId or asyncMap returns None, it will be
  /// interpreted as the related resource not existing. The inner option is part
  /// of the actual relationship data, while the outer option signifies if the
  /// relationship was included or not.
  static member Require
      ( name: string,
        value: Skippable<ToOne>,
        parseId: string -> 'a option,
        asyncMap: 'a -> Async<'b option>
      ) : Async<Result<'b option, RequestDocumentError list>> =
    async {
      match value with
      | Skip -> return Error [RequestDocumentError.RequiredFieldMissing (relPointer name, name)]
      | Include r ->
          match r.Data with
          | Skip | Include None -> return Ok None
          | Include (Some d) ->
              match parseId d.Id with
              | None -> return Error [RequestDocumentError.RelationshipResourceNotFound (dataPointer name, name, d.Type, d.Id)]
              | Some x ->
                  match! asyncMap x with
                  | None -> return Error [RequestDocumentError.RelationshipResourceNotFound (dataPointer name, name, d.Type, d.Id)]
                  | Some x -> return Ok (Some x)
    }

  /// Parses a required non-nullable relationship. If parseId returns None, it
  /// will be interpreted as the related resource not existing. If
  /// nullableByDefault is true, the returned InvalidNullPointer error will have
  /// overridden = true.
  static member RequireNonNull
      ( name: string,
        value: Skippable<ToOne>,
        parseId: string -> 'a option,
        ?nullableByDefault: bool
      ) : Result<'a, RequestDocumentError list> =
    match value with
    | Skip -> Error [RequestDocumentError.RequiredFieldMissing (relPointer name, name)]
    | Include r ->
        match r.Data with
        | Skip | Include None -> Error [RequestDocumentError.InvalidNullPointer (dataPointer name, defaultArg nullableByDefault false)]
        | Include (Some d) ->
            match parseId d.Id with
            | None -> Error [RequestDocumentError.RelationshipResourceNotFound (dataPointer name, name, d.Type, d.Id)]
            | Some x -> Ok x

  /// Parses a required non-nullable relationship in two steps, the second being
  /// async. If either parseId or asyncMap returns None, it will be interpreted
  /// as the related resource not existing. If nullableByDefault is true, the
  /// returned InvalidNullPointer error will have overridden = true.
  static member RequireNonNull
      ( name: string,
        value: Skippable<ToOne>,
        parseId: string -> 'a option,
        asyncMap: 'a -> Async<'b option>,
        ?nullableByDefault: bool
      ) : Async<Result<'b, RequestDocumentError list>> =
    async {
      match value with
      | Skip -> return Error [RequestDocumentError.RequiredFieldMissing (relPointer name, name)]
      | Include r ->
          match r.Data with
          | Skip | Include None -> return Error [RequestDocumentError.InvalidNullPointer (dataPointer name, defaultArg nullableByDefault false)]
          | Include (Some d) ->
              match parseId d.Id with
              | None -> return Error [RequestDocumentError.RelationshipResourceNotFound (dataPointer name, name, d.Type, d.Id)]
              | Some x ->
                  match! asyncMap x with
                  | None -> return Error [RequestDocumentError.RelationshipResourceNotFound (dataPointer name, name, d.Type, d.Id)]
                  | Some x -> return Ok x
    }
