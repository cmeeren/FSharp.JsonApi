namespace FSharp.JsonApi

open FSharp.JsonSkippable


[<AutoOpen>]
module private AttributeHelpers =

  let pointer attrName =
    sprintf "/data/attributes/%s" attrName


/// Helpers for parsing attributes of a single-resource document's main
/// resource.
type Attribute =

  /// Parses a non-option-wrapped string resource attribute according to the
  /// specified map. Values that do not exist as keys in the map will give
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

  /// Parses a non-option-wrapped enum resource attribute according to the
  /// specified map. Values that do not exist as keys in the map will give
  /// AttributeError.InvalidEnum where allowedValues are the string values
  /// of the map keys.
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

  /// Parses a non-option-wrapped resource attribute. Returns errors if it is
  /// included and invalid, and Ok None if it is skipped.
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

  /// Parses a non-option-wrapped resource attribute. Returns errors if it is
  /// included and invalid, and Ok None if it is skipped.
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

  /// Gets a resource attribute (whether option-wrapped or not) as-is without
  /// any transformation. Never returns an error.
  static member Get
      ( value: Skippable<'a>
      ) : Result<'a option, RequestDocumentError list> =
    value |> Skippable.toOption |> Ok

  /// Parses an option-wrapped string resource attribute according to the
  /// specified map. Values that do not exist as keys in the map will give
  /// AttributeError.InvalidEnum where allowedValues are the map keys. The
  /// inner option is part of the actual attribute value, while the outer option
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

  /// Parses an option-wrapped enum resource attribute according to the
  /// specified map. Values that do not exist as keys in the map will give
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

  /// Parses an option-wrapped resource attribute. Returns errors if it is
  /// included and invalid, and Ok None if it is skipped. The inner option is
  /// part of the actual attribute value, while the outer option signifies if it
  /// was included or not.
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

  /// Parses an option-wrapped resource attribute. Returns errors if it is
  /// included and invalid, and Ok None if it is skipped. The inner option is
  /// part of the actual attribute value, while the outer option signifies if it
  /// was included or not.
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

  /// Parses a non-option-wrapped resource attribute. Returns errors if it is
  /// skipped or if it is included and not present as a key in the map. In the
  /// latter situation, the error will be AttributeError.InvalidEnum where
  /// allowedValues are the map keys.
  static member Require
      ( name: string,
        value: Skippable<string>,
        valueMap: Map<string, 'b>
      ) : Result<'b, RequestDocumentError list> =
    Attribute.Get(name, value, valueMap)
    |> Result.bind (Result.requireSome [RequestDocumentError.RequiredFieldMissing (pointer name)])

  /// Parses a non-option-wrapped resource attribute. Returns errors if it is
  /// skipped or if it is included and not present as a key in the map. In the
  /// latter situation, the error will be AttributeError.InvalidEnum where
  /// allowedValues are the map keys.
  static member Require
      ( name: string,
        value: Skippable<'enum>,
        valueMap: Map<'enum, 'b>
      ) : Result<'b, RequestDocumentError list> =
    Attribute.Get(name, value, valueMap)
    |> Result.bind (Result.requireSome [RequestDocumentError.RequiredFieldMissing (pointer name)])

  /// Parses a non-option-wrapped resource attribute. Returns errors if it is
  /// included and invalid or if it is skipped.
  static member Require
      ( name: string,
        value: Skippable<'a>,
        tryParse: 'a -> Result<'b, string>
      ) : Result<'b, RequestDocumentError list> =
    Attribute.Get(name, value, tryParse)
    |> Result.bind (Result.requireSome [RequestDocumentError.RequiredFieldMissing (pointer name)])

  /// Parses a non-option-wrapped resource attribute. Returns errors if it is
  /// included and invalid or if it is skipped.
  static member Require
      ( name: string,
        value: Skippable<'a>,
        tryParse: 'a -> 'b option
      ) : Result<'b, RequestDocumentError list> =
    Attribute.Get(name, value, tryParse)
    |> Result.bind (Result.requireSome [RequestDocumentError.RequiredFieldMissing (pointer name)])

  /// Gets a resource attribute (whether option-wrapped or not) as-is, without
  /// any transformation. Returns an error if it is skipped.
  static member Require
      ( name: string,
        value: Skippable<'a>
      ) : Result<'a, RequestDocumentError list> =
    Attribute.Get(value)
    |> Result.bind (Result.requireSome [RequestDocumentError.RequiredFieldMissing (pointer name)])

  /// Parses an option-wrapped resource attribute. Returns errors if it is
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

  /// Parses an option-wrapped resource attribute. Returns errors if it is
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

  /// Parses an option-wrapped resource attribute. Returns errors if it is
  /// included and invalid or if it is skipped.
  static member Require
      ( name: string,
        value: Skippable<'a option>,
        tryParse: 'a -> Result<'b, string>
      ) : Result<'b option, RequestDocumentError list> =
    Attribute.Get(name, value, tryParse)
    |> Result.bind (Result.requireSome [RequestDocumentError.RequiredFieldMissing (pointer name)])

  /// Parses an option-wrapped resource attribute. Returns errors if it is
  /// included and invalid or if it is skipped.
  static member Require
      ( name: string,
        value: Skippable<'a option>,
        tryParse: 'a -> 'b option
      ) : Result<'b option, RequestDocumentError list> =
    Attribute.Get(name, value, tryParse)
    |> Result.bind (Result.requireSome [RequestDocumentError.RequiredFieldMissing (pointer name)])


[<AutoOpen>]
module Extensions =

  type Attribute with

    /// Parses an option-wrapped resource attribute. Returns errors if it is
    /// included and invalid or if it is skipped.
    static member Require
        ( name: string,
          value: Skippable<'a option>,
          parse: 'a -> 'b
        ) : Result<'b option, RequestDocumentError list> =
      Attribute.Require(name, value, parse >> Some)

    /// Parses an option-wrapped resource attribute. Does not return any errors.
    /// The inner option is part of the actual attribute value, while the outer
    /// option signifies if it was included or not.
    static member Get
        ( value: Skippable<'a option>,
          parse: 'a -> 'b
        ) : Result<'b option option, RequestDocumentError list> =
      Attribute.Get("NOT USED", value, parse >> Some)

[<AutoOpen>]
module Extensions2 =

  type Attribute with

    /// Parses a non-option-wrapped resource attribute. Returns errors if it is
    /// skipped.
    static member Require
        ( name: string,
          value: Skippable<'a>,
          parse: 'a -> 'b
        ) : Result<'b, RequestDocumentError list> =
      Attribute.Require(name, value, parse >> Some)

    /// Parses a non-option-wrapped resource attribute. Does not return any
    /// errors.
    static member Get
        ( value: Skippable<'a>,
          parse: 'a -> 'b
        ) : Result<'b option, RequestDocumentError list> =
      Attribute.Get("NOT USED", value, parse >> Some)
