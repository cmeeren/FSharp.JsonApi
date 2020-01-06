namespace FSharp.JsonApi

open FSharp.JsonSkippable


[<AutoOpen>]
module AttributeExtensions4 =

  type Attribute with

    /// Parses a non-nullable resource attribute. Does not return any errors.
    static member Get
        ( value: Skippable<'a>,
          parse: 'a -> 'b
        ) : Result<'b option, RequestDocumentError list> =
      Attribute.Get("NOT USED", value, parse >> Some)

    /// Parses a nullable resource attribute, but requires that it is not null.
    /// If it is, will give InvalidNull with overridden = true.
    static member GetNonNull
        ( name: string,
          value: Skippable<'a option>,
          parse: 'a -> 'b
        ) : Result<'b option, RequestDocumentError list> =
      Attribute.Get(value, parse)
      |> Result.bind (Option.traverseResult (Result.requireSome [invalidNull name]))

    /// Parses a non-nullable resource attribute. Returns errors if it is
    /// skipped.
    static member Require
        ( name: string,
          value: Skippable<'a>,
          parse: 'a -> 'b
        ) : Result<'b, RequestDocumentError list> =
      Attribute.Require(name, value, parse >> Some)

    /// Parses a non-nullable resource attribute. Returns errors if it is
    /// skipped.
    static member RequireNonNull
        ( name: string,
          value: Skippable<'a option>,
          parse: 'a -> 'b
        ) : Result<'b, RequestDocumentError list> =
      Attribute.Require(name, value, parse)
      |> Result.bind (Result.requireSome [invalidNull name])
