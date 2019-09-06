namespace FSharp.JsonApi

open System
open FSharp.JsonSkippable


// TODO: shouldn't Type always be present? Also in base object model


/// Represents a simplified view of a JSON-API resource with only type name, ID,
/// attributes, and relationships.
type SimpleResource<'attrs, 'rels> =
  { Type: string
    Id: string Skippable
    Attributes: 'attrs
    Relationships: 'rels }


module SimpleResource =

  /// An empty SimpleResource ('type' is the empty string and 'id' is Skip, and
  /// all attribute and relationship members will be Skip).
  let empty<'attrs, 'rels> =
    { Type = ""
      Id = Skip
      Attributes = Activator.CreateInstance<'attrs>()
      Relationships = Activator.CreateInstance<'rels>() }

  /// Returns a simplified view of a resource. If the resource lacks the 'data'
  /// or 'relationships' properties, all members of 'attrs and 'rels will be Skip.
  let ofResource (res: Resource<'attrs, 'rels>) =
    { Type = res.Type
      Id = res.Id
      Attributes = res.Attributes |> Skippable.defaultWith Activator.CreateInstance
      Relationships = res.Relationships |> Skippable.defaultWith Activator.CreateInstance }

  /// Returns the SimpleResource's attributes.
  let attributes r =
    r.Attributes

  /// Returns the SimpleResource's relationships.
  let relationships r =
    r.Relationships
