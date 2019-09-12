namespace FSharp.JsonApi

open FSharp.JsonSkippable


[<AutoOpen>]
module AttributeExtensions2 =

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
