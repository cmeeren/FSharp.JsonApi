namespace FSharp.JsonApi

open FSharp.JsonSkippable


[<AutoOpen>]
module private AttributeHelpers =

  let pointer attrName =
    sprintf "/data/attributes/%s" attrName

  let invalidNull name =
    RequestDocumentError.InvalidNull (pointer name, true)

  let missing name =
    RequestDocumentError.RequiredFieldMissing (pointer name)

  let parseStringMap map name x =
    match map |> Map.tryFind x with
    | Some x -> Ok x
    | None ->
        let allowedValues = map |> Map.toList |> List.map fst
        Error [RequestDocumentError.AttributeInvalidEnum (pointer name, x, allowedValues)]

  let parseEnumMap<'enum, 'b, 'c when 'enum : enum<'c> and 'enum : comparison>
      (map: Map<'enum, 'b>) name x =
    match map |> Map.tryFind x with
    | Some x -> Ok x
    | None ->
        let allowedValues = map  |> Map.toList |> List.map (fst >> box >> string)
        Error [RequestDocumentError.AttributeInvalidEnum (pointer name, x |> box |> string, allowedValues)]

  let parseResultMsg parse name x =
    parse x
    |> Result.mapError (fun errMsg ->
        [RequestDocumentError.AttributeInvalidParsed (pointer name, Some errMsg)])

  let parseAsyncResultMsg parse name x =
    parse x
    |> AsyncResult.mapError (fun errMsg ->
        [RequestDocumentError.AttributeInvalidParsed (pointer name, Some errMsg)])

  let parseOption parse name x =
    parse x
    |> Result.requireSome [RequestDocumentError.AttributeInvalidParsed (pointer name, None)]

  let parseAsyncOption parse name x =
    parse x
    |> AsyncResult.requireSome [RequestDocumentError.AttributeInvalidParsed (pointer name, None)]


/// Helpers for parsing attributes of a single-resource document's main
/// resource.
type Attribute =

  /// Gets a resource attribute (whether nullable or not) as-is without any
  /// transformation. Never returns an error.
  static member Get
      ( value: Skippable<'a>
      ) : Result<'a option, RequestDocumentError list> =
    value
    |> Skippable.toOption
    |> Option.traverseResult Ok

  /// Parses a non-nullable string resource attribute according to the specified
  /// map. Values that do not exist as keys in the map will give
  /// AttributeError.InvalidEnum where allowedValues are the map keys.
  static member Get
      ( name: string,
        value: Skippable<string>,
        valueMap: Map<string, 'b>
      ) : Result<'b option, RequestDocumentError list> =
    value
    |> Skippable.toOption
    |> Option.traverseResult (parseStringMap valueMap name)

  /// Parses a non-nullable enum resource attribute according to the specified
  /// map. Values that do not exist as keys in the map will give
  /// AttributeError.InvalidEnum where allowedValues are the string values of
  /// the map keys.
  static member Get<'enum, 'b, 'c when 'enum : enum<'c> and 'enum : comparison>
      ( name: string,
        value: Skippable<'enum>,
        valueMap: Map<'enum, 'b>
      ) : Result<'b option, RequestDocumentError list> =
    value
    |> Skippable.toOption
    |> Option.traverseResult (parseEnumMap valueMap name)

  /// Parses a non-nullable resource attribute. Returns errors if it is included
  /// and invalid, and Ok None if it is skipped.
  static member Get
      ( name: string,
        value: Skippable<'a>,
        tryParse: 'a -> Result<'b, string>
      ) : Result<'b option, RequestDocumentError list> =
    value
    |> Skippable.toOption
    |> Option.traverseResult (parseResultMsg tryParse name)

  /// Parses a non-nullable resource attribute. Returns errors if it is included
  /// and invalid, and Ok None if it is skipped.
  static member Get
      ( name: string,
        value: Skippable<'a>,
        tryParse: 'a -> Async<Result<'b, string>>
      ) : Async<Result<'b option, RequestDocumentError list>> =
    value
    |> Skippable.toOption
    |> Option.traverseAsyncResult (parseAsyncResultMsg tryParse name)

  /// Parses a non-nullable resource attribute. Returns errors if it is included
  /// and invalid, and Ok None if it is skipped.
  static member Get
      ( name: string,
        value: Skippable<'a>,
        tryParse: 'a -> 'b option
      ) : Result<'b option, RequestDocumentError list> =
    value
    |> Skippable.toOption
    |> Option.traverseResult (parseOption tryParse name)

  /// Parses a non-nullable resource attribute. Returns errors if it is included
  /// and invalid, and Ok None if it is skipped.
  static member Get
      ( name: string,
        value: Skippable<'a>,
        tryParse: 'a -> Async<'b option>
      ) : Async<Result<'b option, RequestDocumentError list>> =
    value
    |> Skippable.toOption
    |> Option.traverseAsyncResult (parseAsyncOption tryParse name)

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
    value
    |> Skippable.toOption
    |> Option.traverseResult (Option.traverseResult (parseStringMap valueMap name))

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
    value
    |> Skippable.toOption
    |> Option.traverseResult (Option.traverseResult (parseEnumMap valueMap name))

  /// Parses a nullable resource attribute. Returns errors if it is included and
  /// invalid, and Ok None if it is skipped. The inner option is part of the
  /// actual attribute value, while the outer option signifies if it was
  /// included or not.
  static member Get
      ( name: string,
        value: Skippable<'a option>,
        tryParse: 'a -> Result<'b, string>
      ) : Result<'b option option, RequestDocumentError list> =
    value
    |> Skippable.toOption
    |> Option.traverseResult (Option.traverseResult (parseResultMsg tryParse name))

  /// Parses a nullable resource attribute. Returns errors if it is included and
  /// invalid, and Ok None if it is skipped. The inner option is part of the
  /// actual attribute value, while the outer option signifies if it was
  /// included or not.
  static member Get
      ( name: string,
        value: Skippable<'a option>,
        tryParse: 'a -> Async<Result<'b, string>>
      ) : Async<Result<'b option option, RequestDocumentError list>> =
    value
    |> Skippable.toOption
    |> Option.traverseAsyncResult (Option.traverseAsyncResult (parseAsyncResultMsg tryParse name))

  /// Parses a nullable resource attribute. Returns errors if it is included and
  /// invalid, and Ok None if it is skipped. The inner option is part of the
  /// actual attribute value, while the outer option signifies if it was
  /// included or not.
  static member Get
      ( name: string,
        value: Skippable<'a option>,
        tryParse: 'a -> 'b option
      ) : Result<'b option option, RequestDocumentError list> =
    value
    |> Skippable.toOption
    |> Option.traverseResult (Option.traverseResult (parseOption tryParse name))

  /// Parses a nullable resource attribute. Returns errors if it is included and
  /// invalid, and Ok None if it is skipped. The inner option is part of the
  /// actual attribute value, while the outer option signifies if it was
  /// included or not.
  static member Get
      ( name: string,
        value: Skippable<'a option>,
        tryParse: 'a -> Async<'b option>
      ) : Async<Result<'b option option, RequestDocumentError list>> =
    value
    |> Skippable.toOption
    |> Option.traverseAsyncResult (Option.traverseAsyncResult (parseAsyncOption tryParse name))

  /// Gets a nullable resource attribute as-is without any transformation, but
  /// requires that it is not null. If it is, will give InvalidNull with
  /// overridden = true.
  static member GetNonNull
      ( name: string,
        value: Skippable<'a option>
      ) : Result<'a option, RequestDocumentError list> =
    Attribute.Get(value)
    |> Result.bind (Option.traverseResult (Result.requireSome [invalidNull name]))

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
    |> Result.bind (Option.traverseResult (Result.requireSome [invalidNull name]))

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
    |> Result.bind (Option.traverseResult (Result.requireSome [invalidNull name]))

  /// Parses a nullable resource attribute, but requires that it is not null. If
  /// it is, will give InvalidNull with overridden = true. Returns errors if it
  /// is included and invalid, and Ok None if it is skipped.
  static member GetNonNull
      ( name: string,
        value: Skippable<'a option>,
        tryParse: 'a -> Result<'b, string>
      ) : Result<'b option, RequestDocumentError list> =
    Attribute.Get(name, value, tryParse)
    |> Result.bind (Option.traverseResult (Result.requireSome [invalidNull name]))

  /// Parses a nullable resource attribute, but requires that it is not null. If
  /// it is, will give InvalidNull with overridden = true. Returns errors if it
  /// is included and invalid, and Ok None if it is skipped.
  static member GetNonNull
      ( name: string,
        value: Skippable<'a option>,
        tryParse: 'a -> Async<Result<'b, string>>
      ) : Async<Result<'b option, RequestDocumentError list>> =
    Attribute.Get(name, value, tryParse)
    |> AsyncResult.bindResult (Option.traverseResult (Result.requireSome [invalidNull name]))

  /// Parses a nullable resource attribute, but requires that it is not null. If
  /// it is, will give InvalidNull with overridden = true. Returns errors if it
  /// is included and invalid, and Ok None if it is skipped.
  static member GetNonNull
      ( name: string,
        value: Skippable<'a option>,
        tryParse: 'a -> 'b option
      ) : Result<'b option, RequestDocumentError list> =
    Attribute.Get(name, value, tryParse)
    |> Result.bind (Option.traverseResult (Result.requireSome [invalidNull name]))

  /// Parses a nullable resource attribute, but requires that it is not null. If
  /// it is, will give InvalidNull with overridden = true. Returns errors if it
  /// is included and invalid, and Ok None if it is skipped.
  static member GetNonNull
      ( name: string,
        value: Skippable<'a option>,
        tryParse: 'a -> Async<'b option>
      ) : Async<Result<'b option, RequestDocumentError list>> =
    Attribute.Get(name, value, tryParse)
    |> AsyncResult.bindResult (Option.traverseResult (Result.requireSome [invalidNull name]))

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
    |> Result.bind (Result.requireSome [missing name])

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
    |> Result.bind (Result.requireSome [missing name])

  /// Parses a required, non-nullable resource attribute. Returns errors if it
  /// is included and invalid or if it is skipped.
  static member Require
      ( name: string,
        value: Skippable<'a>,
        tryParse: 'a -> Result<'b, string>
      ) : Result<'b, RequestDocumentError list> =
    Attribute.Get(name, value, tryParse)
    |> Result.bind (Result.requireSome [missing name])

  /// Parses a required, non-nullable resource attribute. Returns errors if it
  /// is included and invalid or if it is skipped.
  static member Require
      ( name: string,
        value: Skippable<'a>,
        tryParse: 'a -> Async<Result<'b, string>>
      ) : Async<Result<'b, RequestDocumentError list>> =
    Attribute.Get(name, value, tryParse)
    |> AsyncResult.bindResult (Result.requireSome [missing name])

  /// Parses a required, non-nullable resource attribute. Returns errors if it
  /// is included and invalid or if it is skipped.
  static member Require
      ( name: string,
        value: Skippable<'a>,
        tryParse: 'a -> 'b option
      ) : Result<'b, RequestDocumentError list> =
    Attribute.Get(name, value, tryParse)
    |> Result.bind (Result.requireSome [missing name])

  /// Parses a required, non-nullable resource attribute. Returns errors if it
  /// is included and invalid or if it is skipped.
  static member Require
      ( name: string,
        value: Skippable<'a>,
        tryParse: 'a -> Async<'b option>
      ) : Async<Result<'b, RequestDocumentError list>> =
    Attribute.Get(name, value, tryParse)
    |> AsyncResult.bindResult (Result.requireSome [missing name])

  /// Gets a required resource attribute (whether nullable or not) as-is,
  /// without any transformation. Returns an error if it is skipped.
  static member Require
      ( name: string,
        value: Skippable<'a>
      ) : Result<'a, RequestDocumentError list> =
    Attribute.Get(value)
    |> Result.bind (Result.requireSome [missing name])

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
    |> Result.bind (Result.requireSome [missing name])

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
    |> Result.bind (Result.requireSome [missing name])

  /// Parses a required, nullable resource attribute. Returns errors if it is
  /// included and invalid or if it is skipped.
  static member Require
      ( name: string,
        value: Skippable<'a option>,
        tryParse: 'a -> Result<'b, string>
      ) : Result<'b option, RequestDocumentError list> =
    Attribute.Get(name, value, tryParse)
    |> Result.bind (Result.requireSome [missing name])

  /// Parses a required, nullable resource attribute. Returns errors if it is
  /// included and invalid or if it is skipped.
  static member Require
      ( name: string,
        value: Skippable<'a option>,
        tryParse: 'a -> Async<Result<'b, string>>
      ) : Async<Result<'b option, RequestDocumentError list>> =
    Attribute.Get(name, value, tryParse)
    |> AsyncResult.bindResult (Result.requireSome [missing name])

  /// Parses a required, nullable resource attribute. Returns errors if it is
  /// included and invalid or if it is skipped.
  static member Require
      ( name: string,
        value: Skippable<'a option>,
        tryParse: 'a -> 'b option
      ) : Result<'b option, RequestDocumentError list> =
    Attribute.Get(name, value, tryParse)
    |> Result.bind (Result.requireSome [missing name])

  /// Parses a required, nullable resource attribute. Returns errors if it is
  /// included and invalid or if it is skipped.
  static member Require
      ( name: string,
        value: Skippable<'a option>,
        tryParse: 'a -> Async<'b option>
      ) : Async<Result<'b option, RequestDocumentError list>> =
    Attribute.Get(name, value, tryParse)
    |> AsyncResult.bindResult (Result.requireSome [missing name])

  /// Gets a required, nullable resource attribute as-is, without any
  /// transformation, but requires that it is not null. If it is, will give
  /// InvalidNull with overridden = true.
  static member RequireNonNull
      ( name: string,
        value: Skippable<'a option>
      ) : Result<'a, RequestDocumentError list> =
    Attribute.Require(name, value)
    |> Result.bind (Result.requireSome [invalidNull name])

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
    |> Result.bind (Result.requireSome [invalidNull name])

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
    |> Result.bind (Result.requireSome [invalidNull name])

  /// Parses a required, nullable resource attribute, but requires that it is
  /// not null. If it is, will give InvalidNull with overridden = true. Returns
  /// errors if it is included and invalid or if it is skipped.
  static member RequireNonNull
      ( name: string,
        value: Skippable<'a option>,
        tryParse: 'a -> Result<'b, string>
      ) : Result<'b, RequestDocumentError list> =
    Attribute.Require(name, value, tryParse)
    |> Result.bind (Result.requireSome [invalidNull name])

  /// Parses a required, nullable resource attribute, but requires that it is
  /// not null. If it is, will give InvalidNull with overridden = true. Returns
  /// errors if it is included and invalid or if it is skipped.
  static member RequireNonNull
      ( name: string,
        value: Skippable<'a option>,
        tryParse: 'a -> Async<Result<'b, string>>
      ) : Async<Result<'b, RequestDocumentError list>> =
    Attribute.Require(name, value, tryParse)
    |> AsyncResult.bindResult (Result.requireSome [invalidNull name])

  /// Parses a required, nullable resource attribute, but requires that it is
  /// not null. If it is, will give InvalidNull with overridden = true. Returns
  /// errors if it is included and invalid or if it is skipped.
  static member RequireNonNull
      ( name: string,
        value: Skippable<'a option>,
        tryParse: 'a -> 'b option
      ) : Result<'b, RequestDocumentError list> =
    Attribute.Require(name, value, tryParse)
    |> Result.bind (Result.requireSome [invalidNull name])

  /// Parses a required, nullable resource attribute, but requires that it is
  /// not null. If it is, will give InvalidNull with overridden = true. Returns
  /// errors if it is included and invalid or if it is skipped.
  static member RequireNonNull
      ( name: string,
        value: Skippable<'a option>,
        tryParse: 'a -> Async<'b option>
      ) : Async<Result<'b, RequestDocumentError list>> =
    Attribute.Require(name, value, tryParse)
    |> AsyncResult.bindResult (Result.requireSome [invalidNull name])


[<AutoOpen>]
module AttributeExtensions =

  type Attribute with

    /// Parses a nullable resource attribute. Does not return any errors. The
    /// inner option is part of the actual attribute value, while the outer
    /// option signifies if it was included or not.
    static member Get
        ( value: Skippable<'a option>,
          parse: 'a -> Async<'b>
        ) : Async<Result<'b option option, RequestDocumentError list>> =
      Attribute.Get("NOT USED", value, parse >> Async.map Some)

    /// Parses a nullable resource attribute, but requires that it is not null.
    /// If it is, will give InvalidNull with overridden = true.
    static member GetNonNull
        ( name: string,
          value: Skippable<'a option>,
          parse: 'a -> Async<'b>
        ) : Async<Result<'b option, RequestDocumentError list>> =
      Attribute.Get(value, parse)
      |> AsyncResult.bindResult (Option.traverseResult (Result.requireSome [invalidNull name]))

    /// Parses a nullable resource attribute. Returns errors if it is included
    /// and invalid or if it is skipped.
    static member Require
        ( name: string,
          value: Skippable<'a option>,
          parse: 'a -> Async<'b>
        ) : Async<Result<'b option, RequestDocumentError list>> =
      Attribute.Require(name, value, parse >> Async.map Some)

    /// Parses a nullable resource attribute, but requires that it is not null.
    /// If it is, will give InvalidNull with overridden = true. Returns errors
    /// if it is included and invalid or if it is skipped.
    static member RequireNonNull
        ( name: string,
          value: Skippable<'a option>,
          parse: 'a -> Async<'b>
        ) : Async<Result<'b, RequestDocumentError list>> =
      Attribute.Require(name, value, parse)
      |> AsyncResult.bindResult (Result.requireSome [invalidNull name])
