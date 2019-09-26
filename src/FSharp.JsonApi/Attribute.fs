namespace FSharp.JsonApi

open FSharp.JsonSkippable


[<AutoOpen>]
module private AttributeHelpers =

  let pointer attrName =
    sprintf "/data/attributes/%s" attrName


/// Helpers for parsing attributes of a single-resource document's main
/// resource.
type Attribute =

  /// Parses a non-nullable string resource attribute according to the specified
  /// map. Values that do not exist as keys in the map will give
  /// AttributeError.InvalidEnum where allowedValues are the map keys.
  static member Get
      ( name: string,
        value: Skippable<string>,
        valueMap: Map<string, 'b>
      ) : Result<'b option, RequestDocumentError list> =
    match value with
    | Skip -> Ok None
    | Include x ->
        match valueMap |> Map.tryFind x with
        | Some x -> Ok (Some x)
        | None ->
            let allowedValues = valueMap |> Map.toList |> List.map fst
            Error [RequestDocumentError.AttributeInvalidEnum (pointer name, x, allowedValues)]

  /// Parses a non-nullable enum resource attribute according to the specified
  /// map. Values that do not exist as keys in the map will give
  /// AttributeError.InvalidEnum where allowedValues are the string values of
  /// the map keys.
  static member Get<'enum, 'b, 'c when 'enum : enum<'c> and 'enum : comparison>
      ( name: string,
        value: Skippable<'enum>,
        valueMap: Map<'enum, 'b>
      ) : Result<'b option, RequestDocumentError list> =
    match value with
    | Skip -> Ok None
    | Include x ->
        match valueMap |> Map.tryFind x with
        | Some x -> Ok (Some x)
        | None ->
            let allowedValues = valueMap |> Map.toList |> List.map (fst >> box >> string)
            Error [RequestDocumentError.AttributeInvalidEnum (pointer name, x |> box |> string, allowedValues)]

  /// Parses a non-nullable resource attribute. Returns errors if it is included
  /// and invalid, and Ok None if it is skipped.
  static member Get
      ( name: string,
        value: Skippable<'a>,
        tryParse: 'a -> Result<'b, string>
      ) : Result<'b option, RequestDocumentError list> =
    match value with
    | Skip -> Ok None
    | Include x ->
        tryParse x
        |> Result.mapError (fun errMsg ->
            [RequestDocumentError.AttributeInvalidParsed (pointer name, Some errMsg)])
        |> Result.map Some

  /// Parses a non-nullable resource attribute. Returns errors if it is included
  /// and invalid, and Ok None if it is skipped.
  static member Get
      ( name: string,
        value: Skippable<'a>,
        tryParse: 'a -> 'b option
      ) : Result<'b option, RequestDocumentError list> =
    match value with
    | Skip -> Ok None
    | Include x ->
        tryParse x
        |> Result.requireSome [RequestDocumentError.AttributeInvalidParsed (pointer name, None)]
        |> Result.map Some

  /// Gets a resource attribute (whether nullable or not) as-is without any
  /// transformation. Never returns an error.
  static member Get
      ( value: Skippable<'a>
      ) : Result<'a option, RequestDocumentError list> =
    value |> Skippable.toOption |> Ok

  /// Parses a nullable string resource attribute according to the specified
  /// map. Values that do not exist as keys in the map will give
  /// AttributeError.InvalidEnum where allowedValues are the map keys. The inner
  /// option is part of the actual attribute value, while the outer option
  /// signifies if it was included or not.
  static member Get
      ( name: string,
        value: Skippable<string option>,
        valueMap: Map<string, 'b>
      ) : Result<'b option option, RequestDocumentError list> =
    match value with
    | Skip -> Ok None
    | Include None -> Ok (Some None)
    | Include (Some x) ->
        match valueMap |> Map.tryFind x with
        | Some x -> Ok (Some (Some x))
        | None ->
            let allowedValues = valueMap |> Map.toList |> List.map fst
            Error [RequestDocumentError.AttributeInvalidEnum (pointer name, x, allowedValues)]

  /// Parses a nullable enum resource attribute according to the specified map.
  /// Values that do not exist as keys in the map will give
  /// AttributeError.InvalidEnum where allowedValues are the string values of
  /// the map keys. The inner option is part of the actual attribute value,
  /// while the outer option signifies if it was included or not.
  static member Get<'enum, 'b, 'c when 'enum : enum<'c> and 'enum : comparison>
      ( name: string,
        value: Skippable<'enum option>,
        valueMap: Map<'enum, 'b>
      ) : Result<'b option option, RequestDocumentError list> =
    match value with
    | Skip -> Ok None
    | Include None -> Ok (Some None)
    | Include (Some x) ->
        match valueMap |> Map.tryFind x with
        | Some x -> Ok (Some (Some x))
        | None ->
            let allowedValues = valueMap |> Map.toList |> List.map (fst >> box >> string)
            Error [RequestDocumentError.AttributeInvalidEnum (pointer name, x |> box |> string, allowedValues)]

  /// Parses a nullable resource attribute. Returns errors if it is included and
  /// invalid, and Ok None if it is skipped. The inner option is part of the
  /// actual attribute value, while the outer option signifies if it was
  /// included or not.
  static member Get
      ( name: string,
        value: Skippable<'a option>,
        tryParse: 'a -> Result<'b, string>
      ) : Result<'b option option, RequestDocumentError list> =
    match value with
    | Skip -> Ok None
    | Include x ->
        Option.traverseResult tryParse x
        |> Result.mapError (fun errMsg ->
            [RequestDocumentError.AttributeInvalidParsed (pointer name, Some errMsg)])
        |> Result.map Some

  /// Parses a nullable resource attribute. Returns errors if it is included and
  /// invalid, and Ok None if it is skipped. The inner option is part of the
  /// actual attribute value, while the outer option signifies if it was
  /// included or not.
  static member Get
      ( name: string,
        value: Skippable<'a option>,
        tryParse: 'a -> 'b option
      ) : Result<'b option option, RequestDocumentError list> =
    match value with
    | Skip -> Ok None
    | Include x ->
        x
        |> Option.traverseResult
            (tryParse >> Result.requireSome [RequestDocumentError.AttributeInvalidParsed (pointer name, None)])
        |> Result.map Some

  /// Gets a nullable resource attribute as-is without any transformation, but
  /// requires that it is not null. If it is, will give InvalidNull with
  /// overridden = true.
  static member GetNonNull
      ( name: string,
        value: Skippable<'a option>
      ) : Result<'a option, RequestDocumentError list> =
    value |> Skippable.toOption |> Option.traverseResult (Result.requireSome [RequestDocumentError.InvalidNull (pointer name, true)])

  /// Parses a normally nullable string resource attribute according to the
  /// specified map, but requires that it is not null. If it is, will give
  /// InvalidNull with overridden = true. Values that do not exist as keys in
  /// the map will give AttributeError.InvalidEnum where allowedValues are the
  /// map keys. 
  static member GetNonNull
      ( name: string,
        value: Skippable<string option>,
        valueMap: Map<string, 'b>
      ) : Result<'b option, RequestDocumentError list> =
    Attribute.Get(name, value, valueMap)
    |> Result.bind (Option.traverseResult (Result.requireSome [RequestDocumentError.InvalidNull (pointer name, true)]))


  /// Parses a nullable enum resource attribute according to the specified map,
  /// but requires that it is not null. If it is, will give InvalidNull with
  /// overridden = true. Values that do not exist as keys in the map will give
  /// AttributeError.InvalidEnum where allowedValues are the string values of
  /// the map keys.
  static member GetNonNull<'enum, 'b, 'c when 'enum : enum<'c> and 'enum : comparison>
      ( name: string,
        value: Skippable<'enum option>,
        valueMap: Map<'enum, 'b>
      ) : Result<'b option, RequestDocumentError list> =
    Attribute.Get(name, value, valueMap)
    |> Result.bind (Option.traverseResult (Result.requireSome [RequestDocumentError.InvalidNull (pointer name, true)]))

  /// Parses a nullable resource attribute, but requires that it is not null. If
  /// it is, will give InvalidNull with overridden = true. Returns errors if it
  /// is included and invalid, and Ok None if it is skipped.
  static member GetNonNull
      ( name: string,
        value: Skippable<'a option>,
        tryParse: 'a -> Result<'b, string>
      ) : Result<'b option, RequestDocumentError list> =
    Attribute.Get(name, value, tryParse)
    |> Result.bind (Option.traverseResult (Result.requireSome [RequestDocumentError.InvalidNull (pointer name, true)]))

  /// Parses a nullable resource attribute, but requires that it is not null. If
  /// it is, will give InvalidNull with overridden = true. Returns errors if it
  /// is included and invalid, and Ok None if it is skipped.
  static member GetNonNull
      ( name: string,
        value: Skippable<'a option>,
        tryParse: 'a -> 'b option
      ) : Result<'b option, RequestDocumentError list> =
    Attribute.Get(name, value, tryParse)
    |> Result.bind (Option.traverseResult (Result.requireSome [RequestDocumentError.InvalidNull (pointer name, true)]))

  /// Parses a required, non-nullable resource attribute. Returns errors if it
  /// is skipped or if it is included and not present as a key in the map. In
  /// the latter situation, the error will be AttributeError.InvalidEnum where
  /// allowedValues are the map keys.
  static member Require
      ( name: string,
        value: Skippable<string>,
        valueMap: Map<string, 'b>
      ) : Result<'b, RequestDocumentError list> =
    Attribute.Get(name, value, valueMap)
    |> Result.bind (Result.requireSome [RequestDocumentError.RequiredFieldMissing (pointer name)])

  /// Parses a required, non-nullable resource attribute. Returns errors if it
  /// is skipped or if it is included and not present as a key in the map. In
  /// the latter situation, the error will be AttributeError.InvalidEnum where
  /// allowedValues are the map keys.
  static member Require
      ( name: string,
        value: Skippable<'enum>,
        valueMap: Map<'enum, 'b>
      ) : Result<'b, RequestDocumentError list> =
    Attribute.Get(name, value, valueMap)
    |> Result.bind (Result.requireSome [RequestDocumentError.RequiredFieldMissing (pointer name)])

  /// Parses a required, non-nullable resource attribute. Returns errors if it
  /// is included and invalid or if it is skipped.
  static member Require
      ( name: string,
        value: Skippable<'a>,
        tryParse: 'a -> Result<'b, string>
      ) : Result<'b, RequestDocumentError list> =
    Attribute.Get(name, value, tryParse)
    |> Result.bind (Result.requireSome [RequestDocumentError.RequiredFieldMissing (pointer name)])

  /// Parses a required, non-nullable resource attribute. Returns errors if it
  /// is included and invalid or if it is skipped.
  static member Require
      ( name: string,
        value: Skippable<'a>,
        tryParse: 'a -> 'b option
      ) : Result<'b, RequestDocumentError list> =
    Attribute.Get(name, value, tryParse)
    |> Result.bind (Result.requireSome [RequestDocumentError.RequiredFieldMissing (pointer name)])

  /// Gets a required resource attribute (whether nullable or not) as-is,
  /// without any transformation. Returns an error if it is skipped.
  static member Require
      ( name: string,
        value: Skippable<'a>
      ) : Result<'a, RequestDocumentError list> =
    Attribute.Get(value)
    |> Result.bind (Result.requireSome [RequestDocumentError.RequiredFieldMissing (pointer name)])

  /// Parses a required, nullable resource attribute. Returns errors if it is
  /// skipped or if it is included and Some and not present as a key in the map.
  /// In the latter situation, the error will be AttributeError.InvalidEnum
  /// where allowedValues are the map keys.
  static member Require
      ( name: string,
        value: Skippable<string option>,
        valueMap: Map<string, 'b>
      ) : Result<'b option, RequestDocumentError list> =
    Attribute.Get(name, value, valueMap)
    |> Result.bind (Result.requireSome [RequestDocumentError.RequiredFieldMissing (pointer name)])

  /// Parses a required, nullable resource attribute. Returns errors if it is
  /// skipped or if it is included and Some and not present as a key in the map.
  /// In the latter situation, the error will be AttributeError.InvalidEnum
  /// where allowedValues are the map keys.
  static member Require
      ( name: string,
        value: Skippable<'enum option>,
        valueMap: Map<'enum, 'b>
      ) : Result<'b option, RequestDocumentError list> =
    Attribute.Get(name, value, valueMap)
    |> Result.bind (Result.requireSome [RequestDocumentError.RequiredFieldMissing (pointer name)])

  /// Parses a required, nullable resource attribute. Returns errors if it is
  /// included and invalid or if it is skipped.
  static member Require
      ( name: string,
        value: Skippable<'a option>,
        tryParse: 'a -> Result<'b, string>
      ) : Result<'b option, RequestDocumentError list> =
    Attribute.Get(name, value, tryParse)
    |> Result.bind (Result.requireSome [RequestDocumentError.RequiredFieldMissing (pointer name)])

  /// Parses a required, nullable resource attribute. Returns errors if it is
  /// included and invalid or if it is skipped.
  static member Require
      ( name: string,
        value: Skippable<'a option>,
        tryParse: 'a -> 'b option
      ) : Result<'b option, RequestDocumentError list> =
    Attribute.Get(name, value, tryParse)
    |> Result.bind (Result.requireSome [RequestDocumentError.RequiredFieldMissing (pointer name)])

  /// Gets a required, nullable resource attribute as-is, without any
  /// transformation, but requires that it is not null. If it is, will give
  /// InvalidNull with overridden = true.
  static member RequireNonNull
      ( name: string,
        value: Skippable<'a option>
      ) : Result<'a, RequestDocumentError list> =
    Attribute.Require(name, value)
    |> Result.bind (Result.requireSome [RequestDocumentError.InvalidNull (pointer name, true)])

  /// Parses a required, nullable resource attribute, but requires that it is
  /// not null. If it is, will give InvalidNull with overridden = true. Returns
  /// errors if it is skipped or if it is included and Some and not present as a
  /// key in the map. In the latter situation, the error will be
  /// AttributeError.InvalidEnum where allowedValues are the map keys.
  static member RequireNonNull
      ( name: string,
        value: Skippable<string option>,
        valueMap: Map<string, 'b>
      ) : Result<'b, RequestDocumentError list> =
    Attribute.Require(name, value, valueMap)
    |> Result.bind (Result.requireSome [RequestDocumentError.InvalidNull (pointer name, true)])

  /// Parses a required, nullable resource attribute, but requires that it is
  /// not null. If it is, will give InvalidNull with overridden = true. Returns
  /// errors if it is skipped or if it is included and Some and not present as a
  /// key in the map. In the latter situation, the error will be
  /// AttributeError.InvalidEnum where allowedValues are the map keys.
  static member RequireNonNull
      ( name: string,
        value: Skippable<'enum option>,
        valueMap: Map<'enum, 'b>
      ) : Result<'b, RequestDocumentError list> =
    Attribute.Require(name, value, valueMap)
    |> Result.bind (Result.requireSome [RequestDocumentError.InvalidNull (pointer name, true)])

  /// Parses a required, nullable resource attribute, but requires that it is
  /// not null. If it is, will give InvalidNull with overridden = true. Returns
  /// errors if it is included and invalid or if it is skipped.
  static member RequireNonNull
      ( name: string,
        value: Skippable<'a option>,
        tryParse: 'a -> Result<'b, string>
      ) : Result<'b, RequestDocumentError list> =
    Attribute.Require(name, value, tryParse)
    |> Result.bind (Result.requireSome [RequestDocumentError.InvalidNull (pointer name, true)])

  /// Parses a required, nullable resource attribute, but requires that it is
  /// not null. If it is, will give InvalidNull with overridden = true. Returns
  /// errors if it is included and invalid or if it is skipped.
  static member RequireNonNull
      ( name: string,
        value: Skippable<'a option>,
        tryParse: 'a -> 'b option
      ) : Result<'b, RequestDocumentError list> =
    Attribute.Require(name, value, tryParse)
    |> Result.bind (Result.requireSome [RequestDocumentError.InvalidNull (pointer name, true)])


[<AutoOpen>]
module AttributeExtensions =

  type Attribute with

    /// Parses a nullable resource attribute. Returns errors if it is included
    /// and invalid or if it is skipped.
    static member Require
        ( name: string,
          value: Skippable<'a option>,
          parse: 'a -> 'b
        ) : Result<'b option, RequestDocumentError list> =
      Attribute.Require(name, value, parse >> Some)

    /// Parses a nullable resource attribute, but requires that it is not null.
    /// If it is, will give InvalidNull with overridden = true. Returns errors
    /// if it is included and invalid or if it is skipped.
    static member RequireNonNull
        ( name: string,
          value: Skippable<'a option>,
          parse: 'a -> 'b
        ) : Result<'b, RequestDocumentError list> =
      Attribute.Require(name, value, parse)
      |> Result.bind (Result.requireSome [RequestDocumentError.InvalidNull (pointer name, true)])

    /// Parses a nullable resource attribute. Does not return any errors. The
    /// inner option is part of the actual attribute value, while the outer
    /// option signifies if it was included or not.
    static member Get
        ( value: Skippable<'a option>,
          parse: 'a -> 'b
        ) : Result<'b option option, RequestDocumentError list> =
      Attribute.Get("NOT USED", value, parse >> Some)

    /// Parses a nullable resource attribute, but requires that it is not null.
    /// If it is, will give InvalidNull with overridden = true.
    static member Get
        ( name: string,
          value: Skippable<'a option>,
          parse: 'a -> 'b
        ) : Result<'b option, RequestDocumentError list> =
      Attribute.Get(value, parse)
      |> Result.bind (Result.requireSome [RequestDocumentError.InvalidNull (pointer name, true)])
