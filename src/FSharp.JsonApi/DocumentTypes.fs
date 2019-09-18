namespace FSharp.JsonApi

open System
open FSharp.JsonSkippable
open Newtonsoft.Json


[<CLIMutable>]
type JsonApi =
  {
    [<JsonProperty("version")>]
    Version: string Skippable
    [<JsonProperty("meta")>]
    Meta: Map<string, obj> Skippable
  }


[<CLIMutable>]
type Link =
  {
    [<JsonProperty("href")>]
    Href: Uri option
    [<JsonProperty("meta")>]
    Meta: Map<string, obj> Skippable
  }


[<Struct>]
type Links = Links of Map<string, Link>


[<CLIMutable>]
type ErrorSource =
  {
    [<JsonProperty("pointer")>]
    Pointer: string Skippable
    [<JsonProperty("parameter")>]
    Parameter: string Skippable
  }


[<CLIMutable>]
type Error =
  {
    [<JsonProperty("id")>]
    Id: string Skippable
    [<JsonProperty("links")>]
    Links: Links Skippable
    [<JsonProperty("status")>]
    Status: string Skippable
    [<JsonProperty("code")>]
    Code: string Skippable
    [<JsonProperty("title")>]
    Title: string Skippable
    [<JsonProperty("detail")>]
    Detail: string Skippable
    [<JsonProperty("source")>]
    Source: ErrorSource Skippable
    [<JsonProperty("meta")>]
    Meta: Map<string, obj> Skippable
  }


[<CLIMutable>]
type ResourceIdentifier =
  {
    [<JsonProperty("type")>]
    Type: string
    [<JsonProperty("id")>]
    Id: string
  }


[<CLIMutable>]
type ToOne =
  {
    [<JsonProperty("links")>]
    Links: Links Skippable
    [<JsonProperty("data")>]
    Data: ResourceIdentifier option Skippable
    [<JsonProperty("meta")>]
    Meta: Map<string, obj> Skippable
  }


[<CLIMutable>]
type ToMany =
  {
    [<JsonProperty("links")>]
    Links: Links Skippable
    [<JsonProperty("data")>]
    Data: ResourceIdentifier list Skippable
    [<JsonProperty("meta")>]
    Meta: Map<string, obj> Skippable
  }


[<CLIMutable>]
type Resource<'attrs, 'rels> =
  {
    [<JsonProperty("type")>]
    Type: string
    [<JsonProperty("id")>]
    Id: string Skippable
    [<JsonProperty("attributes")>]
    Attributes: 'attrs Skippable
    [<JsonProperty("links")>]
    Links: Links Skippable
    [<JsonProperty("relationships")>]
    Relationships: 'rels Skippable
    [<JsonProperty("meta")>]
    Meta: Map<string, obj> Skippable
  }


type IJsonApiDocument = interface end


type ICompoundDocument =
  abstract Included : Resource<obj, obj> list Skippable


[<CLIMutable>]
type ResourceDocument =
  {
    [<JsonProperty("jsonapi")>]
    JsonApi: JsonApi Skippable
    [<JsonProperty("links")>]
    Links: Links Skippable
    [<JsonProperty("meta")>]
    Meta: Map<string, obj> Skippable
    [<JsonProperty("data")>]
    Data: Resource<obj, obj> option
    [<JsonProperty("included")>]
    Included: Resource<obj, obj> list Skippable
  }
  interface IJsonApiDocument
  interface ICompoundDocument with member this.Included = this.Included


[<CLIMutable>]
type ResourceCollectionDocument =
  {
    [<JsonProperty("jsonapi")>]
    JsonApi: JsonApi Skippable
    [<JsonProperty("links")>]
    Links: Links Skippable
    [<JsonProperty("meta")>]
    Meta: Map<string, obj> Skippable
    [<JsonProperty("data")>]
    Data: Resource<obj, obj> list
    [<JsonProperty("included")>]
    Included: Resource<obj, obj> list Skippable
  }
  interface IJsonApiDocument
  interface ICompoundDocument with member this.Included = this.Included


[<CLIMutable>]
type ResourceIdentifierDocument =
  {
    [<JsonProperty("jsonapi")>]
    JsonApi: JsonApi Skippable
    [<JsonProperty("links")>]
    Links: Links Skippable
    [<JsonProperty("meta")>]
    Meta: Map<string, obj> Skippable
    [<JsonProperty("data")>]
    Data: ResourceIdentifier option
  }
  interface IJsonApiDocument


[<CLIMutable>]
type ResourceIdentifierCollectionDocument =
  {
    [<JsonProperty("jsonapi")>]
    JsonApi: JsonApi Skippable
    [<JsonProperty("links")>]
    Links: Links Skippable
    [<JsonProperty("meta")>]
    Meta: Map<string, obj> Skippable
    [<JsonProperty("data")>]
    Data: ResourceIdentifier list
  }
  interface IJsonApiDocument


[<CLIMutable>]
type ErrorDocument =
  {
    [<JsonProperty("jsonapi")>]
    JsonApi: JsonApi Skippable
    [<JsonProperty("errors")>]
    Errors: Error list
    [<JsonProperty("links")>]
    Links: Links Skippable
    [<JsonProperty("meta")>]
    Meta: Map<string, obj> Skippable
  }
  interface IJsonApiDocument



module JsonApi =


  // A JSON-API object indicating version 1.0.
  let v1_0 = { Version = Include "1.0"; Meta = Skip }


  /// Adds the specified key-value pair to the JsonApi object's Meta object.
  let addMeta key value (jsonApi: JsonApi) =
    { jsonApi with
        Meta =
          jsonApi.Meta
          |> Skippable.map (Map.add key (box value))
          |> Skippable.orElse (Map.empty.Add(key, box value) |> Include)
    }


  /// Adds the specified key-value pair to the JsonApi object's Meta object if
  /// condition is true.
  let addMetaIf condition key value jsonApi =
    if condition then addMeta key value jsonApi else jsonApi


module Link =


  /// The name of the JSON-API standard "self" link.
  let [<Literal>] self = "self"

  /// The name of the JSON-API standard "related" link.
  let [<Literal>] related = "related"

  /// The name of the JSON-API standard "about" link.
  let [<Literal>] about = "about"

  /// The name of the JSON-API standard "first" link.
  let [<Literal>] first = "first"

  /// The name of the JSON-API standard "last" link.
  let [<Literal>] last = "last"

  /// The name of the JSON-API standard "prev" link.
  let [<Literal>] prev = "prev"

  /// The name of the JSON-API standard "next" link.
  let [<Literal>] next = "next"


  /// Adds the specified key-value pair to the link's Meta object.
  let addMeta key value (link: Link) =
    { link with
        Meta =
          link.Meta
          |> Skippable.map (Map.add key (box value))
          |> Skippable.orElse (Map.empty.Add(key, box value) |> Include)
    }


  /// Adds the specified key-value pair to the link's Meta object if condition
  /// is true.
  let addMetaIf condition key value link =
    if condition then addMeta key value link else link



module Links =


  let private toMeta (entries: #seq<string * 'a>) =
    entries
    |> Seq.map (fun (k, v) -> k, box v)
    |> Map.ofSeq
    |> Include
    |> Skippable.filter (not << Seq.isEmpty)


  /// Creates a new link collection containing a link with the given name and URI.
  let create name uri =
    Map.empty.Add(name, { Href = Some uri; Meta = Skip }) |> Links


  /// Creates a new link collection containing a link with the given name and meta and
  /// null href.
  let createWithMeta name metaEntries =
    Map.empty.Add(name, { Href = None; Meta = toMeta metaEntries }) |> Links


  /// Creates a new link collection containing a null link.
  let createNull name =
    Map.empty.Add(name, { Href = None; Meta = Skip }) |> Links


  /// Adds the specified link and meta to the link collection. The meta property
  /// is not included if there are no meta entries.
  let addOptWithMeta name (uri: Uri option) metaEntries (Links links) =
    links |> Map.add name { Href = uri; Meta = toMeta metaEntries } |> Links


  /// Adds the specified link and meta to the link collection. The meta property
  /// is not included if there are no meta entries.
  let addWithMeta name uri metaEntries links =
    addOptWithMeta name (Some uri) metaEntries links


  /// Adds the specified link to the link collection.
  let addOpt name uri links =
    addOptWithMeta name uri [] links


  /// Adds the specified link to the link collection.
  let add name uri links =
    addOptWithMeta name (Some uri) [] links


  /// Adds the specified link and meta to the link collection if the condition is true.
  /// The meta property is not included if there are no meta entries.
  let addOptIfWithMeta cond name uri metaEntries links =
    if cond then addOptWithMeta name uri metaEntries links else links


  /// Adds the specified link and meta to the link collection if the condition is true.
  /// The meta property is not included if there are no meta entries.
  let addIfWithMeta cond name uri metaEntries links =
    addOptIfWithMeta cond name (Some uri) metaEntries links


  /// Adds the specified link to the link collection if the condition is true.
  let addOptIf cond name uri links =
    addOptIfWithMeta cond name uri [] links


  /// Adds the specified link to the link collection if the condition is true.
  let addIf cond name uri links =
    addOptIfWithMeta cond name (Some uri) [] links



module Error =


  /// An empty Error object.
  let empty =
    { Id = Skip; Links = Skip; Status = Skip; Code = Skip;
      Title = Skip; Detail = Skip; Source = Skip; Meta = Skip }


  /// Creates an Error object with the given Code.
  let create code =
    { empty with Code = Include code }


  /// Creates an Error object with the given Code and with the Id property set
  /// to a new GUID.
  let createId code =
    { empty with
        Id = Guid.NewGuid() |> string |> Include
        Code = Include code }


  /// Transforms the error using the specified function on the inner value if opt is Some.
  let ifSome opt f (err: Error) =
    match opt with
    | None -> err
    | Some x -> err |> f x


  /// Sets the error's Detail property.
  let setDetail text err =
    { err with Detail = Include text }


  /// Sets the error's Detail property using a format string.
  let setDetailf format =
    Printf.ksprintf (fun s -> setDetail s) format


  /// Sets the error's Status property.
  let setStatus (statusCode: int) err =
    { err with Status = Include (string statusCode) }


  /// Sets the error's Title property.
  let setTitle text err =
    { err with Title = Include text }


  /// Sets the error's Source.Parameter property (and removes any Source.Pointer value).
  let setSourceParam queryParam err =
    { err with Source = Include { Parameter = Include queryParam; Pointer = Skip } }


  /// Sets the error's Source.Pointer property (and removes any Source.Parameter value).
  let setSourcePointer jsonPointer err =
    { err with Source = Include { Parameter = Skip; Pointer = Include jsonPointer } }


  /// Sets the error's Source.Pointer property to "/data/attributes/{attrName}"
  /// (and removes any Source.Parameter value). Use only for errors in a
  /// ResourceDocument; see setSourceAttrIdx for use with a ResourceCollectionDocument.
  let setSourceAttr attrName err =
    setSourcePointer (sprintf "/data/attributes/%s" attrName) err


  /// Sets the error's Source.Pointer property to "/data/{idx}/attributes/{attrName}"
  /// (and removes any Source.Parameter value). Use only for errors in a
  /// ResourceCollectionDocument; see setSourceAttr for use with a ResourceDocument.
  let setSourceAttrIdx idx attrName err =
    setSourcePointer (sprintf "/data/%i/attributes/%s" idx attrName) err


  /// Sets the error's Source.Pointer property to "/data/relationships/{relName}"
  /// (and removes any Source.Parameter value). Use only for errors in a
  /// ResourceDocument; see setSourceRelIdx for use with a ResourceCollectionDocument.
  let setSourceRel relName err =
    setSourcePointer (sprintf "/data/relationships/%s" relName) err


  /// Sets the error's Source.Pointer property to "/data/{idx}/relationships/{relName}"
  /// (and removes any Source.Parameter value). Use only for errors in a
  /// ResourceCollectionDocument; see setSourceRel for use with a ResourceDocument.
  let setSourceRelIdx idx relName err =
    setSourcePointer (sprintf "/data/%i/relationships/%s" idx relName) err


  /// Adds the specified key-value pair to the error's Meta object.
  let addMeta key value (err: Error) =
    { err with
        Meta =
          err.Meta
          |> Skippable.map (Map.add key (box value))
          |> Skippable.orElse (Map.empty.Add(key, box value) |> Include)
    }


  /// Adds the specified key-value pair to the error's Meta object if condition
  /// is true.
  let addMetaIf condition key value err =
    if condition then addMeta key value err else err


  /// Adds the specified link to the error's Links object.
  let addLink linkName url (err: Error) =
    { err with
        Links =
          err.Links
          |> Skippable.map (Links.add linkName url)
          |> Skippable.orElse (Include <| Links.create linkName url)
    }


  /// Adds the specified link to the error's Links object if condition is true.
  let addLinkIf condition linkName url err =
    if condition then addLink linkName url err else err


  /// Sets the Error object's "about" link to the specified URL.
  let setAboutLink url (err: Error) =
    { err with
        Links =
          err.Links
          |> Skippable.map (Links.add Link.about url)
          |> Skippable.orElse (Include <| Links.create Link.about url)
    }



module ResourceIdentifier =

  /// Creates a resource identifier object with the specified type and ID.
  let create resourceType resourceId =
    { Type = resourceType; Id = resourceId }


module ToOne =


  /// An empty to-one relationship.
  let empty : ToOne =
    { Links = Skip; Data = Skip; Meta = Skip }


  /// Adds the specified link to the relationship's Links object.
  let addLink linkName url (rel: ToOne) =
    { rel with
        Links =
          rel.Links
          |> Skippable.map (Links.add linkName url)
          |> Skippable.orElse (Include <| Links.create linkName url)
    }

  /// Adds the specified link to the relationship's Links object if condition
  /// is true.
  let addLinkIf condition linkName url rel =
    if condition then addLink linkName url rel else rel


  /// Sets the relationship's "self" link using the given relationship name.
  let setSelfLink (selfUrl: Uri) relName (rel: ToOne) =
    let url = selfUrl.AddSegments ["relationships"; relName]
    addLink Link.self url rel


  /// Sets the relationship's "related" link using the given relationship name.
  let setRelatedLink (selfUrl: Uri) relName (rel: ToOne) =
    let url = selfUrl.AddSegment relName
    addLink Link.related url rel


  /// Sets the relationship's data using the given resource identifier.
  let setData (data: ResourceIdentifier option) (rel: ToOne) =
    { rel with Data = Include data }


  /// Adds the specified key-value pair to the relationship's Meta object.
  let addMeta key value (rel: ToOne) =
    { rel with
        Meta =
          rel.Meta
          |> Skippable.map (Map.add key (box value))
          |> Skippable.orElse (Map.empty.Add(key, box value) |> Include)
    }


  /// Adds the specified key-value pair to the relationship's Meta object if
  /// condition is true.
  let addMetaIf condition key value err =
    if condition then addMeta key value err else err



module ToMany =


  /// An empty to-many relationship.
  let empty : ToMany =
    { Links = Skip; Data = Skip; Meta = Skip }


  /// Adds the specified link to the relationship's Links object.
  let addLink linkName url (rel: ToMany) =
    { rel with
        Links =
          rel.Links
          |> Skippable.map (Links.add linkName url)
          |> Skippable.orElse (Include <| Links.create linkName url)
    }


  /// Adds the specified link to the relationship's Links object if condition
  /// is true.
  let addLinkIf condition linkName url rel =
    if condition then addLink linkName url rel else rel


  /// Sets the relationship's "self" link using the given relationship name.
  let setSelfLink (selfUrl: Uri) relName (rel: ToMany) =
    let url = selfUrl.AddSegments ["relationships"; relName]
    addLink Link.self url rel


  /// Sets the relationship's "related" link using the given relationship name.
  let setRelatedLink (selfUrl: Uri) relName (rel: ToMany) =
    let url = selfUrl.AddSegment relName
    addLink Link.related url rel


  /// Sets the relationship's data using the given resource identifiers.
  let setData (data: ResourceIdentifier list) (rel: ToMany) =
    { rel with Data = Include data }


  /// Adds the specified key-value pair to the relationship's Meta object.
  let addMeta key value (rel: ToMany) =
    { rel with
        Meta =
          rel.Meta
          |> Skippable.map (Map.add key (box value))
          |> Skippable.orElse (Map.empty.Add(key, box value) |> Include)
    }


  /// Adds the specified key-value pair to the relationship's Meta object if
  /// condition is true.
  let addMetaIf condition key value err =
    if condition then addMeta key value err else err



module Resource =

  /// Converts a strongly typed resource to a weakly typed resource.
  let box (res: Resource<'attrs, 'rels>) : Resource<obj, obj> =
    {
      Type = res.Type
      Id = res.Id
      Attributes = res.Attributes |> Skippable.map box
      Links = res.Links
      Relationships = res.Relationships |> Skippable.map box
      Meta = res.Meta
    }


  /// Converts a weakly typed resource to a strongly typed resource. May fail,
  /// just like the built-in unbox function.
  let unbox<'attrs, 'rels> (res: Resource<obj, obj>) : Resource<'attrs, 'rels> =
    {
      Type = res.Type
      Id = res.Id
      Attributes = res.Attributes |> Skippable.map unbox
      Links = res.Links
      Relationships = res.Relationships |> Skippable.map unbox
      Meta = res.Meta
    }


  /// Converts a weakly typed resource to a strongly typed resource. Returns None
  /// if the resource was not of the desired type.
  let tryUnbox<'attrs, 'rels> (res: Resource<obj, obj>) : Resource<'attrs, 'rels> option =
    try unbox res |> Some
    with _ -> None


  /// Adds the specified key-value pair to the resource's Meta object.
  let addMeta key value (rel: Resource<_,_>) =
    { rel with
        Meta =
          rel.Meta
          |> Skippable.map (Map.add key (Operators.box value))
          |> Skippable.orElse (Map.empty.Add(key, Operators.box value) |> Include)
    }


  /// Adds the specified key-value pair to the resource's Meta object if condition
  /// is true.
  let addMetaIf condition key value err =
    if condition then addMeta key value err else err


  /// Adds the specified link to the resource's Links object.
  let addLink linkName url (res: Resource<_,_>) =
    { res with
        Links =
          res.Links
          |> Skippable.map (Links.add linkName url)
          |> Skippable.orElse (Include <| Links.create linkName url)
    }


  /// Adds the specified link to the resource's Links object if condition is true.
  let addLinkIf condition linkName url res =
    if condition then addLink linkName url res else res


  /// If the resource is Some and the attributes are included, returns the
  /// attributes. Otherwise returns a default attributes instance (where all
  /// attributes are Skip).
  let attributesOrDefault (res: Resource<'attrs, 'rels> option) : 'attrs =
    res
    |> Option.bind (fun r -> r.Attributes |> Skippable.toOption)
    |> Option.defaultWith Activator.CreateInstance<'attrs>


  /// If the resource is Some and the relationships are included, returns the
  /// relationships. Otherwise returns a default relationships instance (where
  /// all relationships are Skip).
  let relationshipsOrDefault (res: Resource<'attrs, 'rels> option) : 'rels =
    res
    |> Option.bind (fun r -> r.Relationships |> Skippable.toOption)
    |> Option.defaultWith Activator.CreateInstance<'rels>


  /// If the resource is Some and the meta is included, returns the meta.
  /// Otherwise returns an empty map.
  let metaOrDefault (res: Resource<'attrs, 'rels> option) : Map<string, obj> =
    res
    |> Option.bind (fun r -> r.Meta |> Skippable.toOption)
    |> Option.defaultValue Map.empty



module ResourceDocument =


  /// Returns a single-resource document with the specified resource and
  /// included resources.
  let ofResourceAndIncluded
        (resource: Resource<'attrs, 'rels> option)
        (included: Resource<obj, obj> list)
        : ResourceDocument =
    {
      JsonApi = Skip
      Links = Skip
      Meta = Skip
      Data = resource |> Option.map Resource.box
      Included = included |> Include |> Skippable.filter (not << Seq.isEmpty)
    }


  /// Returns a single-resource document with the specified resource.
  let ofResource (resource: Resource<'attrs, 'rels> option) =
    ofResourceAndIncluded resource []


  /// A single-resource document with no data.
  let noResource : ResourceDocument =
    { JsonApi = Skip
      Links = Skip
      Meta = Skip
      Data = None
      Included = Skip }


  /// Extracts a strongly typed resource from a single-resource document.
  /// Returns None if the document has no resource of if the resource type does
  /// not match the inferred type.
  let resourceOfType (doc: ResourceDocument) : Resource<'attrs, 'rels> option =
    doc.Data |> Option.bind Resource.tryUnbox


  /// Adds the specified key-value pair to the document's Meta object.
  let addMeta key value (doc: ResourceDocument) =
    { doc with
        Meta =
          doc.Meta
          |> Skippable.map (Map.add key (box value))
          |> Skippable.orElse (Map.empty.Add(key, box value) |> Include)
    }


  /// Adds the specified key-value pair to the document's Meta object if condition
  /// is true.
  let addMetaIf condition key value err =
    if condition then addMeta key value err else err


  /// Adds the specified link to the document's Links object.
  let addLink linkName url (doc: ResourceDocument) =
    { doc with
        Links =
          doc.Links
          |> Skippable.map (Links.add linkName url)
          |> Skippable.orElse (Include <| Links.create linkName url)
    }


  /// Adds the specified link to the document's Links object if condition is true.
  let addLinkIf condition linkName url doc =
    if condition then addLink linkName url doc else doc


  /// Returns the main resource's self URL, if present.
  let mainSelfUrl (doc: ResourceDocument) =
    match doc with
    | { Data = Some { Links = Include (Links map) } } ->
        match map.TryFind "self" with
        | Some { Href = Some url } -> Some url
        | _ -> None
    | _ -> None



module ResourceCollectionDocument =


  /// Returns a resource collection document with the specified resources and
  /// included resources.
  let ofResourcesAndIncluded
        (resources: Resource<'attrs, 'rels> list)
        (included: Resource<obj, obj> list)
        : ResourceCollectionDocument =
    {
      JsonApi = Skip
      Links = Skip
      Meta = Skip
      Data = resources |> List.map Resource.box
      Included = included |> Include |> Skippable.filter (not << Seq.isEmpty)
    }

  /// Returns a resource collection document with the specified resources.
  let ofResource (resources: Resource<'attrs, 'rels> list) =
    ofResourcesAndIncluded resources []

  /// Adds the specified key-value pair to the document's Meta object.
  let addMeta key value (doc: ResourceCollectionDocument) =
    { doc with
        Meta =
          doc.Meta
          |> Skippable.map (Map.add key (box value))
          |> Skippable.orElse (Map.empty.Add(key, box value) |> Include)
    }


  /// Adds the specified key-value pair to the document's Meta object if condition
  /// is true.
  let addMetaIf condition key value err =
    if condition then addMeta key value err else err


  /// Adds the specified link to the document's Links object.
  let addLink linkName url (doc: ResourceCollectionDocument) =
    { doc with
        Links =
          doc.Links
          |> Skippable.map (Links.add linkName url)
          |> Skippable.orElse (Include <| Links.create linkName url)
    }


  /// Adds the specified link to the document's Links object if condition is true.
  let addLinkIf condition linkName url doc =
    if condition then addLink linkName url doc else doc



module ResourceIdentifierDocument =


  /// Returns a resource identifier document with the specified resource
  /// identifier as data.
  let ofId (resId: ResourceIdentifier) : ResourceIdentifierDocument =
    { JsonApi = Skip
      Links = Skip
      Meta = Skip
      Data = Some resId }


  /// A resource identifier document with no data.
  let noId : ResourceIdentifierDocument =
    { JsonApi = Skip
      Links = Skip
      Meta = Skip
      Data = None }


  /// Returns a resource identifier document with the specified resource
  /// identifier as data.
  let ofOptId (resId: ResourceIdentifier option) : ResourceIdentifierDocument =
    resId |> Option.map ofId |> Option.defaultValue noId


  /// Adds the specified key-value pair to the document's Meta object.
  let addMeta key value (doc: ResourceIdentifierDocument) =
    { doc with
        Meta =
          doc.Meta
          |> Skippable.map (Map.add key (box value))
          |> Skippable.orElse (Map.empty.Add(key, box value) |> Include)
    }


  /// Adds the specified key-value pair to the document's Meta object if condition
  /// is true.
  let addMetaIf condition key value err =
    if condition then addMeta key value err else err


  /// Adds the specified link to the document's Links object.
  let addLink linkName url (doc: ResourceIdentifierDocument) =
    { doc with
        Links =
          doc.Links
          |> Skippable.map (Links.add linkName url)
          |> Skippable.orElse (Include <| Links.create linkName url)
    }


  /// Adds the specified link to the document's Links object if condition is true.
  let addLinkIf condition linkName url doc =
    if condition then addLink linkName url doc else doc



module ResourceIdentifierCollectionDocument =


  /// Returns a resource identifier collection document with the given resource
  /// identifiers as data.
  let ofIds (resIds: ResourceIdentifier list) =
    { JsonApi = Skip
      Links = Skip
      Meta = Skip
      Data = resIds }


  /// Adds the specified key-value pair to the document's Meta object.
  let addMeta key value (doc: ResourceIdentifierCollectionDocument) =
    { doc with
        Meta =
          doc.Meta
          |> Skippable.map (Map.add key (box value))
          |> Skippable.orElse (Map.empty.Add(key, box value) |> Include)
    }


  /// Adds the specified key-value pair to the document's Meta object if condition
  /// is true.
  let addMetaIf condition key value err =
    if condition then addMeta key value err else err


  /// Adds the specified link to the document's Links object.
  let addLink linkName url (doc: ResourceIdentifierCollectionDocument) =
    { doc with
        Links =
          doc.Links
          |> Skippable.map (Links.add linkName url)
          |> Skippable.orElse (Include <| Links.create linkName url)
    }


  /// Adds the specified link to the document's Links object if condition is true.
  let addLinkIf condition linkName url doc =
    if condition then addLink linkName url doc else doc



module ErrorDocument =


  /// Creates an ErrorDocument with the specified errors.
  let ofErrors errors =
    { JsonApi = Skip
      Errors = errors
      Links = Skip
      Meta = Skip }


  /// Creates an ErrorDocument with the specified error.
  let ofError error =
    ofErrors [error]


  /// Adds the specified key-value pair to the document's Meta object.
  let addMeta key value (doc: ErrorDocument) =
    { doc with
        Meta =
          doc.Meta
          |> Skippable.map (Map.add key (box value))
          |> Skippable.orElse (Map.empty.Add(key, box value) |> Include)
    }


  /// Adds the specified key-value pair to the document's Meta object if condition
  /// is true.
  let addMetaIf condition key value err =
    if condition then addMeta key value err else err


  /// Adds the specified link to the document's Links object.
  let addLink linkName url (doc: ErrorDocument) =
    { doc with
        Links =
          doc.Links
          |> Skippable.map (Links.add linkName url)
          |> Skippable.orElse (Include <| Links.create linkName url)
    }


  /// Adds the specified link to the document's Links object if condition is true.
  let addLinkIf condition linkName url doc =
    if condition then addLink linkName url doc else doc
