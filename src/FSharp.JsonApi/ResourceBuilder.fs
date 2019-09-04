namespace FSharp.JsonApi

open System
open System.Collections.Concurrent
open FSharp.JsonSkippable


type ResourceBuilder<'attrs, 'rels> = internal {
  Identifier: ResourceIdentifier
  SelfUrl: Uri option
  Attributes: unit -> Async<'attrs Skippable>
  Relationships: unit -> Async<'rels Skippable * ResourceBuilder<obj, obj> list>
  Links: unit -> Async<Links Skippable>
  Meta: unit -> Async<Map<string, obj> Skippable>
} with

  static member Create identifier : ResourceBuilder<'attrs, 'rels> = {
    Identifier = identifier
    SelfUrl = None
    Attributes = fun () -> async.Return Skip
    Relationships = fun () -> async.Return (Skip, [])
    Links = fun () -> async.Return Skip
    Meta = fun () -> async.Return Skip
  }

  member this.WithSelfUrl (selfUrl: Uri option) =
    { this with SelfUrl = selfUrl }

  member this.WithSelfUrl (selfUrl: Uri) =
    { this with SelfUrl = Some selfUrl }

  member this.WithAttributes (attributes: Async<'attrs Skippable>) =
    { this with Attributes = fun () -> attributes }

  member this.WithAttributes (attributes: unit -> 'attrs Skippable) =
    { this with Attributes = attributes >> async.Return }

  member this.WithRelationships (relationships: Async<'rels Skippable * ResourceBuilder<obj, obj> list>) =
    { this with Relationships = fun () -> relationships }

  member this.WithRelationships (relationships: unit -> ('rels Skippable * ResourceBuilder<obj, obj> list)) =
    { this with Relationships = relationships >> async.Return }

  member this.WithLinks (links: Async<Links Skippable>) =
    { this with Links = fun () -> links }

  member this.WithLinks (links: Async<Links>) =
    { this with Links = fun () -> links |> Async.map Include }

  member this.WithLinks (links: unit -> Links Skippable) =
    { this with Links = links >> async.Return }

  member this.WithLinks (links: unit -> Links) =
    { this with Links = links >> Include >> async.Return }

  member this.WithMeta (meta: Async<Map<string, 'a> Skippable>) =
    { this with Meta = fun () -> meta |> Async.map (Skippable.map Map.box) }

  member this.WithMeta (meta: Async<Map<string, 'a>>) =
    { this with Meta = fun () -> meta |> Async.map (Map.box >> Include) }

  member this.WithMeta (meta: unit -> Map<string, 'a> Skippable) =
    { this with Meta = meta >> Skippable.map Map.box >> async.Return }

  member this.WithMeta (meta: unit -> Map<string, 'a>) =
    { this with Meta = meta >> Map.box >> Include >> async.Return }


[<AutoOpen>]
module ResourceBuilderExtensions =

  type ResourceBuilder<'attrs, 'rels> with

    member this.WithAttributes (attributes: Async<'attrs>) =
      { this with Attributes = fun () -> attributes |> Async.map Include }

    member this.WithAttributes (attributes: unit -> 'attrs) =
      { this with Attributes = attributes >> Include >> async.Return }

    member this.WithRelationships (relationships: Async<'rels * ResourceBuilder<obj, obj> list>) =
      { this with Relationships = fun () -> relationships |> Async.map (Tuple2.map Include id) }

    member this.WithRelationships (relationships: unit -> ('rels * ResourceBuilder<obj, obj> list)) =
      { this with Relationships = relationships >> Tuple2.map Include id >> async.Return }


module ResourceBuilder =

  /// Converts a strongly typed ResourceBuilder to a weakly typed ResourceBuilder.
  let box (builder: ResourceBuilder<'attrs, 'rels>) : ResourceBuilder<obj, obj> = {
    Identifier = builder.Identifier
    SelfUrl = builder.SelfUrl
    Attributes = builder.Attributes >> Async.map (Skippable.map box)
    Relationships = builder.Relationships >> Async.map (fun (rels, bs) -> rels |> Skippable.map box, bs)
    Links = builder.Links
    Meta = builder.Meta
  }


  let private getGettersAndSetters : Type -> ((obj -> obj) * (obj -> obj -> unit)) [] =
    memoize (fun (t: Type) ->
      t.GetProperties()
      |> Array.map (fun pi -> buildUntypedGetter pi, buildUntypedSetter pi)
    )


  /// Merges the relationships of two relationships collections. This is needed
  /// because a resource may have several resource builders with different
  /// relationships.
  ///
  /// For example, GET /posts/1?include=author.posts.comments will produce one
  /// builder that includes post.author and N builders that include
  /// post.comments.
  ///
  /// If a relationship is present in both relationship collections, the links
  /// and meta will be merged and the data (if included in both) will be taken
  /// from rels1.
  let internal mergeRelationships (rels1: Skippable<'rels>) (rels2: Skippable<'rels>) =
    match rels1, rels2 with
    | Skip, Skip ->
        // The relationship collections are skipped entirely
        Skip
    | Include rels, Skip | Skip, Include rels -> 
        // Only one relationship collection is present, so use that
        Include rels
    | Include rel1, Include rel2 ->
        // Both relationship collections are present. For each relationship in
        // the collection (i.e., for each member of the object), use the new
        // relationship data if present.
        for get, set in rel1.GetType() |> getGettersAndSetters do
          match get rel1, get rel2 with
          | (:? Skippable<ToOne> as sr1), (:? Skippable<ToOne> as sr2) ->
              let newRel =
                match sr1, sr2 with
                | Include r1, Include r2 ->
                    Include
                      { ToOne.Links =
                          match r1.Links, r2.Links with
                          | Include (Links l1), Include (Links l2) -> Map.merge l1 l2 |> Links |> Include
                          | _ -> r1.Links |> Skippable.orElse r2.Links
                        Data = r1.Data |> Skippable.orElse r2.Data
                        Meta =
                          match r1.Meta, r2.Meta with
                          | Include m1, Include m2 -> ExpandoObject.merge m1 m2 |> Include
                          | _ -> r1.Meta |> Skippable.orElse r2.Meta
                      }
                | _ -> sr1 |> Skippable.orElse sr2
              set rel1 newRel
          | (:? Skippable<ToMany> as sr1), (:? Skippable<ToMany> as sr2) ->
              let newRel =
                match sr1, sr2 with
                | Include r1, Include r2 ->
                    Include
                      { ToMany.Links =
                          match r1.Links, r2.Links with
                          | Include (Links l1), Include (Links l2) -> Map.merge l1 l2 |> Links |> Include
                          | _ -> r1.Links |> Skippable.orElse r2.Links
                        Data = r1.Data |> Skippable.orElse r2.Data
                        Meta =
                          match r1.Meta, r2.Meta with
                          | Include m1, Include m2 -> ExpandoObject.merge m1 m2 |> Include
                          | _ -> r1.Meta |> Skippable.orElse r2.Meta
                      }
                | _ -> sr1 |> Skippable.orElse sr2
              set rel1 newRel
          | t1, t2 ->
              // Should never happen since we check the types when constructing the JsonApiContext
              failwithf "Invalid relationship type when merging relationships: %s" (t1.GetType().FullName)
        Include rel1


  /// Sets empty relationships in the relationship collection to Skip.
  let internal cleanRelationships (res: Resource<'attrs, 'rels>) =
    match res.Relationships with
    | Skip -> ()
    | Include rels ->
        for get, set in rels.GetType() |> getGettersAndSetters do
          match get rels with
          | (:? Skippable<ToOne> as sr) ->
              match sr with
              | Include r when r = ToOne.empty -> set rels (Skip: Skippable<ToOne>)
              | _ -> ()
          | (:? Skippable<ToMany> as sr) ->
              match sr with
              | Include r when r = ToMany.empty -> set rels (Skip: Skippable<ToMany>)
              | _ -> ()
          | _ -> ()


  /// Gets the resource's built relationship collections, merges them, and
  /// returns a copy of the resource containing its relationship collection.
  let addRelationships 
      getRelationships (res: Resource<'attrs, 'rels>)
      : Resource<'attrs, 'rels> =
    match res.Id, res.Type with
    | Include id, Include tp ->
        match getRelationships { Id = id; Type = tp } with
        | [] -> res
        | rels -> { res with Relationships = rels |> List.reduce mergeRelationships }
    | _ -> res


  /// Builds a single resource, calls addRelationship with the identifier and
  /// relationship collection, and returns the resource along with builders for
  /// the included resources.
  let internal buildAndGetRelated
      addRelationship (builder: ResourceBuilder<'attrs, 'rels>)
      : Async<Resource<'attrs, 'rels> * ResourceBuilder<obj, obj> list> =
    async {
      let! attrsComp = builder.Attributes () |> Async.StartChild
      let! relsAndIncludedComp = builder.Relationships () |> Async.StartChild
      let! linksComp = builder.Links () |> Async.StartChild
      let! metaComp = builder.Meta () |> Async.StartChild

      let! attrs = attrsComp
      let! rels, included = relsAndIncludedComp
      let! links = linksComp
      let! meta = metaComp

      addRelationship builder.Identifier rels

      let resource = {
        Type = Include builder.Identifier.Type
        Id = Include builder.Identifier.Id
        Attributes = attrs
        Links = links
        Relationships = Skip  // We add all relationships at the end of the process
        Meta = meta |> Skippable.map ExpandoObject.ofMap
      }

      return resource, included
    }

  /// Calls addRelationship with the identifier and relationship collection, and
  /// returns the builders for the included resources.
  let internal getRelated 
      addRelationship (builder: ResourceBuilder<'attrs, 'rels>)
      : Async<ResourceBuilder<obj, obj> list> =
    async {
      let! rels, included = builder.Relationships ()
      addRelationship builder.Identifier rels
      return included
    }

  /// Builds a single resource and all included resources. If isMain is true,
  /// calls addMainResource with the built resource, otherwise calls
  /// addIncludedResource with the built resource (unless it's already built).
  let rec internal buildRecursive 
      initRelationships addRelationship addMainResource addIncludedResource
      isMain (builder: ResourceBuilder<obj, obj>)
      : Async<unit> =
    async {
      let recurse = buildRecursive initRelationships addRelationship addMainResource addIncludedResource false
      let wasInitiated = initRelationships builder.Identifier
      if isMain then
        // We are building a main resource
        let! mainResource, relatedBuilders = buildAndGetRelated addRelationship builder
        addMainResource mainResource
        do! relatedBuilders |> List.map recurse |> Async.Parallel |> Async.map ignoreUnitArray
      elif wasInitiated then
        // We are building an included resource that has not been built yet
        let! includedResource, relatedBuilders = buildAndGetRelated addRelationship builder
        addIncludedResource includedResource
        do! relatedBuilders |> List.map recurse |> Async.Parallel |> Async.map ignoreUnitArray
      else
        // We are building a resource that has already been built
        let! relatedBuilders = getRelated addRelationship builder
        do! relatedBuilders |> List.map recurse |> Async.Parallel |> Async.map ignoreUnitArray
    }

  /// Builds the specified main resources and returns the built resources along
  /// with any included resources.
  let internal build (mainBuilders: ResourceBuilder<'attrs, 'rels> list)
      : Async<Resource<'attrs, 'rels> list * Resource<obj, obj> list> =

    /// We only build each resource once, but different builders for the same
    /// resource may have different relationships included, so we add all
    /// relationships to this dictionary when building, and merge them and add
    /// them to the resources later.
    ///
    /// A ResourceIdentifier being present in this dict indicates that it is
    /// already being built and should not be built again (except the
    /// relationships and included resources).
    let relationships = ConcurrentDictionary<ResourceIdentifier, Skippable<obj> list>()

    /// A container for all the main resources
    let mainResources : Resource<obj, obj> [] = Array.zeroCreate(mainBuilders.Length)

    /// A container for all the included resources
    let includedResources = ConcurrentBag()


    // Helper functions

    let initRelationships identifier = 
      relationships.TryAdd (identifier, [])

    let addRelationship identifier rels =
      relationships.AddOrUpdate(
        identifier,
        (fun _ -> failwith "Library bug: Resource identifier not found in relationships dict during update, should never happen"),
        fun _ existing -> rels :: existing)
      |> ignore

    let addMainResource idx res =
      mainResources.[idx] <- res

    let addIncludedResource res =
      includedResources.Add res

    let getRelationships identifier =
      match relationships.TryGetValue identifier with
      | false, _ -> []
      | true, rels -> rels

    // Do the actual work

    async {
      do!
        mainBuilders
        |> List.mapi (fun i b -> b |> box |> buildRecursive initRelationships addRelationship (addMainResource i) addIncludedResource true)
        |> Async.Parallel
        |> Async.map ignoreUnitArray

      return
        mainResources
        |> Array.map (addRelationships getRelationships >> Resource.unbox)
        |> Array.tee cleanRelationships
        |> Array.toList,

        includedResources.ToArray()
        |> Array.map (addRelationships getRelationships)
        |> Array.tee cleanRelationships
        |> Array.toList
    }

  /// Builds the specified main resource and returns the built resource along
  /// with any included resources.
  let internal buildOne (mainBuilder: ResourceBuilder<'attrs, 'rels>)
      : Async<Resource<'attrs, 'rels> * Resource<obj, obj> list> =
    async {
      let! main, included = build [mainBuilder]
      return main.Head, included
    }
