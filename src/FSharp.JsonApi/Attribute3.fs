namespace FSharp.JsonApi

open FSharp.JsonSkippable


[<AutoOpen>]
module AttributeExtensions3 =

  type Attribute with

    /// Parses a nullable resource attribute. Does not return any errors. The
    /// inner option is part of the actual attribute value, while the outer
    /// option signifies if it was included or not.
    static member Get
        ( value: Skippable<'a option>,
          parse: 'a -> 'b
        ) : Result<'b option option, RequestDocumentError list> =
      Attribute.Get("NOT USED", value, parse >> Some)

    /// Parses a non-nullable resource attribute. Does not return any errors.
    static member Get
        ( value: Skippable<'a>,
          parse: 'a -> Async<'b>
        ) : Async<Result<'b option, RequestDocumentError list>> =
      Attribute.Get("NOT USED", value, parse >> Async.map Some)

    /// Parses a nullable resource attribute. Returns errors if it is skipped.
    static member Require
        ( name: string,
          value: Skippable<'a option>,
          parse: 'a -> 'b
        ) : Result<'b option, RequestDocumentError list> =
      Attribute.Require(name, value, parse >> Some)

    /// Parses a nullable resource attribute, but requires that it is not null.
    /// If it is, will give InvalidNull with overridden = true. Returns errors
    /// if it is skipped.
    static member Require
        ( name: string,
          value: Skippable<'a>,
          parse: 'a -> Async<'b>
        ) : Async<Result<'b, RequestDocumentError list>> =
      Attribute.Require(name, value, parse >> Async.map Some)
