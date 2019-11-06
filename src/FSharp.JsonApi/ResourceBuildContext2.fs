namespace FSharp.JsonApi

open FSharp.JsonSkippable


[<AutoOpen>]
module ResourceBuildContextExtensions2 =

  type ResourceBuildContext with

    /// Returns the specified value (wrapped in Include) if the attribute is
    /// not excluded using sparse fieldsets. Otherwise returns Skip.
    member this.GetAttribute (attrName: AttributeName, value: 'a) : Skippable<'a> =
      this.GetAttribute (attrName, Include value)

    /// Returns the specified value (wrapped in Include) if the attribute is
    /// explicitly included using sparse fieldsets. Otherwise returns Skip.
    member this.GetExplicitAttribute (attrName: AttributeName, value: 'a) : Skippable<'a> =
      this.GetExplicitAttribute (attrName, Include value)
