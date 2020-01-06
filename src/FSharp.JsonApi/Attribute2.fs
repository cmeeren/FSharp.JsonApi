namespace FSharp.JsonApi

open FSharp.JsonSkippable


[<AutoOpen>]
module AttributeExtensions2 =

  type Attribute with

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
