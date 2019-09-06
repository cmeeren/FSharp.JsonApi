module FSharp.JsonApi.Tests.ResourceBuilderTests

open System
open System.Dynamic
open Xunit
open Hedgehog
open Swensen.Unquote
open FSharp.JsonSkippable
open FSharp.JsonApi


[<CLIMutable>]
type Attrs = {
  X: int
}

[<CLIMutable>]
type Rels = {
  ToOne: ToOne Skippable
  ToMany: ToMany Skippable
}


type ResourceDiscriminator =
  | Obj of Resource<obj, obj>
  | Attrs of Resource<Attrs, obj>
  | Rels of Resource<obj, Rels>
  | AttrsRels of Resource<Attrs, Rels>


let getIdentifier (res: Resource<'attrs, 'rels>) =
  { Id = res.Id |> Skippable.defaultWith (fun () -> failwith "Missing Id")
    Type = res.Type }

module build =

  let getBuilder identifier included =
    ResourceBuilder
      .Create(Obj, identifier)
      .WithRelationships(async.Return (Skip, included))

  [<Fact>]
  let ``main resource IDs exactly match the top-level builder IDs`` () =
    let builders = [
      getBuilder { Type = "A"; Id = "1" } [
        getBuilder { Type = "A"; Id = "2" } []
        getBuilder { Type = "A"; Id = "3" } [
          getBuilder { Type = "A"; Id = "5" } []
        ]
        getBuilder { Type = "A"; Id = "4" } [
          getBuilder { Type = "A"; Id = "5" } []
        ]
      ]
      getBuilder { Type = "A"; Id = "2" } []
      getBuilder { Type = "A"; Id = "3" } []
    ]

    let main, _ = builders |> ResourceBuilder.build |> Async.RunSynchronously
      
    let builderIds = 
      [("A", "1"); ("A", "2"); ("A", "3")]
      |> List.map (fun (t, i) -> { Type = t; Id = i })
    let mainIds = main |> List.map getIdentifier
    test <@ mainIds = builderIds @>

  [<Fact>]
  let ``included resource IDs match the non-top-level builder IDs, disregarding order`` () =
    let builders = [
      getBuilder { Type = "A"; Id = "1" } [
        getBuilder { Type = "A"; Id = "2" } []
        getBuilder { Type = "A"; Id = "3" } [
          getBuilder { Type = "A"; Id = "5" } []
        ]
        getBuilder { Type = "A"; Id = "4" } [
          getBuilder { Type = "A"; Id = "5" } []
        ]
      ]
      getBuilder { Type = "A"; Id = "2" } []
      getBuilder { Type = "A"; Id = "3" } []
    ]

    let _, included = builders |> ResourceBuilder.build |> Async.RunSynchronously
      
    let subBuilderIds = 
      [("A", "4"); ("A", "5")]
      |> List.map (fun (t, i) -> { Type = t; Id = i })
      |> Set.ofList
    let includedIds = included |> List.map getIdentifier |> Set.ofList
    test <@ includedIds = subBuilderIds @>

  [<Fact>]
  let ``there are no duplicate resources`` () =
    let builders = [
      getBuilder { Type = "A"; Id = "1" } [
        getBuilder { Type = "A"; Id = "2" } []
        getBuilder { Type = "A"; Id = "3" } [
          getBuilder { Type = "A"; Id = "5" } []
        ]
        getBuilder { Type = "A"; Id = "4" } [
          getBuilder { Type = "A"; Id = "5" } []
        ]
      ]
      getBuilder { Type = "A"; Id = "2" } []
      getBuilder { Type = "A"; Id = "3" } []
    ]

    let main, included = builders |> ResourceBuilder.build |> Async.RunSynchronously
      
    let allIds =
      (main |> List.map (fun s -> s.Type, s.Id))
      @ (included |> List.map (fun s -> s.Type, s.Id))
    test <@ allIds = List.distinct allIds @>

  [<Fact>]
  let ``building should not take much longer than the sum of the longest delay at each level`` () =
    let createBuilder attrDelay relDelay linkDelay metaDelay children =
      let id = Guid.NewGuid().ToString()
      ResourceBuilder
        .Create(Obj, { Type = "A"; Id = id })
        .WithAttributes(async { 
          do! Async.Sleep attrDelay
          return Skip
        })
        .WithRelationships(async { 
          do! Async.Sleep relDelay
          return Skip, children
        })
        .WithLinks(async { 
          do! Async.Sleep linkDelay
          return Skip
        })
        .WithMeta(async { 
          do! Async.Sleep metaDelay
          return Skip
        })

    let builders = [
      createBuilder 100 200 300 400 [
        createBuilder 600 700 800 900 [
          createBuilder 10 20 30 1000 []
        ]
      ]
      createBuilder 500 600 700 800 []
    ]

    let sumLongestDelays = 800 + 900 + 1000

    let sw = Diagnostics.Stopwatch.StartNew()
    let _ = ResourceBuilder.build builders |> Async.RunSynchronously
    sw.Stop()

    test <@ float sw.ElapsedMilliseconds < (float sumLongestDelays * 1.2) @>


  [<Fact>]
  let ``relationships for the same resource should be merged`` () =

    let createToOneRel setRelated setSelf setMetaA setMetaB (related: ResourceIdentifier option Skippable) =
      ToOne.empty
      |> if setRelated then ToOne.setRelatedLink (Uri "http://test.com/foo") "toOne" else id
      |> if setSelf then ToOne.setSelfLink (Uri "http://test.com/foo") "toOne" else id
      |> ToOne.addMetaIf setMetaA "A" "foo"
      |> ToOne.addMetaIf setMetaB "B" "bar"
      |> match related with Skip -> id | Include identifier -> ToOne.setData identifier
      |> Include

    let createToManyRel setRelated setSelf setMetaA setMetaB (related: ResourceIdentifier list Skippable) =
      ToMany.empty
      |> if setRelated then ToMany.setRelatedLink (Uri "http://test.com/foo") "toMany" else id
      |> if setSelf then ToMany.setSelfLink (Uri "http://test.com/foo") "toMany" else id
      |> ToMany.addMetaIf setMetaA "A" "foo"
      |> ToMany.addMetaIf setMetaB "B" "bar"
      |> match related with Skip -> id | Include identifiers -> ToMany.setData identifiers
      |> Include

    let createBuilder rid setRelated setSelf setMetaA setMetaB (toOne: ResourceBuilder<obj, Rels> option Skippable) (toMany: ResourceBuilder<obj, Rels> list Skippable) =
      ResourceBuilder
        .Create(Rels, rid)
        .WithRelationships(async { 
          return
            Include {
              ToOne = toOne |> Skippable.map (Option.map (fun b -> b.Identifier)) |> createToOneRel setRelated setSelf setMetaA setMetaB
              ToMany = toMany |> Skippable.map (List.map (fun b -> b.Identifier)) |> createToManyRel setRelated setSelf setMetaA setMetaB
            },
            ( (toOne |> Skippable.bind Skippable.ofOption |> Skippable.map List.singleton |> Skippable.defaultValue [])
              @ (toMany |> Skippable.defaultValue [])
              |> List.map ResourceBuilder.box
            )
        })

    let id1 = { Type = "A"; Id = "1" }
    let id2 = { Type = "A"; Id = "2" }
    let id3 = { Type = "A"; Id = "3" }

    let builders = [
      createBuilder id1 true false true false Skip (Include [
        createBuilder id2 false false false false Skip (Include [
          createBuilder id1 false true false true (Include (Some (createBuilder id3 true false false false (Include None) Skip))) Skip
        ])
      ])
      createBuilder id2 true true true false Skip Skip
    ]

    let main, included = ResourceBuilder.build builders |> Async.RunSynchronously

    let rels = 
      main @ (included |> List.map Resource.unbox)
      |> List.map (fun r -> getIdentifier r, (r.Relationships |> Skippable.defaultWith (fun () -> failwith "Missing Relationships")))
      |> Map.ofList

    let expectedRels = 
      [
        id1, { ToOne = createToOneRel true true true true (Include (Some id3)); ToMany = createToManyRel true true true true (Include [id2]) }
        id2, { ToOne = createToOneRel true true true false Skip; ToMany = createToManyRel true true true false (Include [id1]) }
        id3, { ToOne = createToOneRel true false false false  (Include None); ToMany = createToManyRel true false false false Skip }
      ]
      |> Map.ofList

    let actual1, expected1 = rels.TryFind(id1).Value, expectedRels.TryFind(id1).Value
    let actual2, expected2 = rels.TryFind(id2).Value, expectedRels.TryFind(id2).Value
    let actual3, expected3 = rels.TryFind(id3).Value, expectedRels.TryFind(id3).Value

    test <@ actual1.ToOne.Value.Data = expected1.ToOne.Value.Data @>
    test <@ actual1.ToOne.Value.Links = expected1.ToOne.Value.Links @>
    test <@ Skippable.map ExpandoObject.toMap actual1.ToOne.Value.Meta = Skippable.map ExpandoObject.toMap expected1.ToOne.Value.Meta @>
    test <@ actual1.ToMany.Value.Data = expected1.ToMany.Value.Data @>
    test <@ actual1.ToMany.Value.Links = expected1.ToMany.Value.Links @>
    test <@ Skippable.map ExpandoObject.toMap actual1.ToMany.Value.Meta = Skippable.map ExpandoObject.toMap expected1.ToMany.Value.Meta @>

    test <@ actual2.ToOne.Value.Data = expected2.ToOne.Value.Data @>
    test <@ actual2.ToOne.Value.Links = expected2.ToOne.Value.Links @>
    test <@ Skippable.map ExpandoObject.toMap actual2.ToOne.Value.Meta = Skippable.map ExpandoObject.toMap expected2.ToOne.Value.Meta @>
    test <@ actual2.ToMany.Value.Data = expected2.ToMany.Value.Data @>
    test <@ actual2.ToMany.Value.Links = expected2.ToMany.Value.Links @>
    test <@ Skippable.map ExpandoObject.toMap actual2.ToMany.Value.Meta = Skippable.map ExpandoObject.toMap expected2.ToMany.Value.Meta @>

    test <@ actual3.ToOne.Value.Data = expected3.ToOne.Value.Data @>
    test <@ actual3.ToOne.Value.Links = expected3.ToOne.Value.Links @>
    test <@ Skippable.map ExpandoObject.toMap actual3.ToOne.Value.Meta = Skippable.map ExpandoObject.toMap expected3.ToOne.Value.Meta @>
    test <@ actual3.ToMany.Value.Data = expected3.ToMany.Value.Data @>
    test <@ actual3.ToMany.Value.Links = expected3.ToMany.Value.Links @>
    test <@ Skippable.map ExpandoObject.toMap actual3.ToMany.Value.Meta = Skippable.map ExpandoObject.toMap expected3.ToMany.Value.Meta @>


  [<Fact>]
  let ``Attributes, links, and meta should be built once for each resource, even if there are several builders for the resource`` () =
    let createBuilder rid incrementAttrs incrementLinks incrementMeta children =
      ResourceBuilder
        .Create(Obj, rid)
        .WithAttributes(async {
          incrementAttrs ()
          return Skip
        })
        .WithRelationships(async { 
          return Skip, children
        })
        .WithLinks(async {
          incrementLinks ()
          return Skip
        })
        .WithMeta(async {
          incrementMeta ()
          return Skip
        })

    let id1 = { Type = "A"; Id = "1" }
    let id2 = { Type = "A"; Id = "2" }

    let locker = obj()
    let mutable attributes = 0
    let mutable links = 0
    let mutable meta = 0
    let incrementAttrs () = lock locker (fun () -> attributes <- attributes + 1)
    let incrementLinks () = lock locker (fun () -> links <- links + 1)
    let incrementMeta () = lock locker (fun () -> meta <- meta + 1)

    let builders = [
      createBuilder id1 incrementAttrs incrementLinks incrementMeta []
      createBuilder id2 ignore ignore ignore [
        createBuilder id1 incrementAttrs incrementLinks incrementMeta []
      ]
    ]

    let _ = ResourceBuilder.build builders |> Async.RunSynchronously

    test <@ (attributes, links, meta) = (1, 1, 1) @>


  [<Fact>]
  let ``empty relationships should be Skip even if included in the builder`` () =
    let builder =
      ResourceBuilder
        .Create(Rels, { Type = "A"; Id = "1" })
        .WithRelationships(async { 
          return Include { ToOne = Include ToOne.empty; ToMany = Include ToMany.empty }, []
        })

    let main, _ = ResourceBuilder.buildOne builder |> Async.RunSynchronously

    test <@ main.Relationships.Value.ToOne = Skip && main.Relationships.Value.ToMany = Skip @>


let getBuilder
    identifier
    (selfUrl: Uri option)
    (attrs: Skippable<_>)
    (rels: Skippable<_>)
    (links: Skippable<_>)
    (meta: Skippable<_>)
    included =
  ResourceBuilder
    .Create(AttrsRels, identifier)
    .WithSelfUrl(selfUrl)
    .WithAttributes(async { return attrs })
    .WithRelationships(async { return rels, included })
    .WithLinks(async { return links })
    .WithMeta(async { return meta })


module mergeRelationships =

  [<Fact>]
  let ``if one of the relationship collections is Skip, uses the other as-is`` () =
    Property.check <| property {
      let! rels = GenX.auto<Rels>

      test <@ ResourceBuilder.mergeRelationships Skip (Include rels) = Include rels @>
      test <@ ResourceBuilder.mergeRelationships (Include rels) Skip = Include rels @>
    }

  [<Fact>]
  let ``if both relationship collections are included, returns the first collection (reference check)`` () =
    Property.check <| property {
      let! rels1 = GenX.auto<Rels>
      let! rels2 = GenX.auto<Rels>

      test <@ Object.ReferenceEquals((ResourceBuilder.mergeRelationships (Include rels1) (Include rels2)).Value, rels1) @>
      test <@ Object.ReferenceEquals((ResourceBuilder.mergeRelationships (Include rels2) (Include rels1)).Value, rels2) @>
    }

  [<Fact>]
  let ``if both relationship collections are included, merges the links for each relationship`` () =
    Property.check <| property {
      let! rels1 = GenX.auto<Rels>
      let! rels2 = GenX.auto<Rels>
      
      let rels1ToOneLinks = rels1.ToOne |> Skippable.bind (fun x -> x.Links)
      let rels1ToManyLinks = rels1.ToMany |> Skippable.bind (fun x -> x.Links)
      let rels2ToOneLinks = rels2.ToOne |> Skippable.bind (fun x -> x.Links)
      let rels2ToManyLinks = rels2.ToMany |> Skippable.bind (fun x -> x.Links)
      
      let mergedRels = ResourceBuilder.mergeRelationships (Include rels1) (Include rels2)

      let mergedToOneLinks = mergedRels.Value.ToOne |> Skippable.bind (fun x -> x.Links)
      let mergedToManyLinks = mergedRels.Value.ToMany |> Skippable.bind (fun x -> x.Links)

      let getExpectedLinks sl1 sl2 =
        match sl1, sl2 with
        | Skip, Skip -> Skip
        | Include l, Skip | Skip, Include l -> Include l
        | Include (Links l1), Include (Links l2) -> 
            [Map.toSeq l1; Map.toSeq l2] |> Seq.concat |> Map.ofSeq |> Links |> Include

      test <@ mergedToOneLinks = getExpectedLinks rels1ToOneLinks rels2ToOneLinks @>
      test <@ mergedToManyLinks = getExpectedLinks rels1ToManyLinks rels2ToManyLinks @>
    }

  [<Fact>]
  let ``if both relationship collections are included, merges the meta`` () =
    Property.check <| property {
      let! rels1 = GenX.auto<Rels>
      let! rels2 = GenX.auto<Rels>
      
      let rels1ToOneMeta = rels1.ToOne |> Skippable.bind (fun x -> x.Meta)
      let rels1ToManyMeta = rels1.ToMany |> Skippable.bind (fun x -> x.Meta)
      let rels2ToOneMeta = rels2.ToOne |> Skippable.bind (fun x -> x.Meta)
      let rels2ToManyMeta = rels2.ToMany |> Skippable.bind (fun x -> x.Meta)
      
      let mergedRels = ResourceBuilder.mergeRelationships (Include rels1) (Include rels2)

      let mergedToOneMeta = mergedRels.Value.ToOne |> Skippable.bind (fun x -> x.Meta)
      let mergedToManyMeta = mergedRels.Value.ToMany |> Skippable.bind (fun x -> x.Meta)

      let getExpectedMeta (sm1: Skippable<ExpandoObject>) sm2 =
        match sm1, sm2 with
        | Skip, Skip -> Skip
        | Include m, Skip | Skip, Include m -> Include (ExpandoObject.toMap m)
        | Include m1, Include m2 ->
            ExpandoObject.merge m1 m2 |> ExpandoObject.toMap |> Include

      test <@ mergedToOneMeta |> Skippable.map ExpandoObject.toMap = getExpectedMeta rels1ToOneMeta rels2ToOneMeta @>
      test <@ mergedToManyMeta |> Skippable.map ExpandoObject.toMap = getExpectedMeta rels1ToManyMeta rels2ToManyMeta @>
    }

  /// This helper must be outside because quotations don't support inner generic functions
  let getExpectedData sd1 sd2 =
    match sd1, sd2 with
    | Skip, Skip -> Skip
    | Include d, Skip | Skip, Include d -> Include d
    | Include d1, Include d2 -> Include d1

  [<Fact>]
  let ``if both relationship collections are included, uses the included data for each relationship`` () =
    Property.check <| property {
      let! rels1 = GenX.auto<Rels>
      let! rels2 = GenX.auto<Rels>
      
      let rels1ToOneData = rels1.ToOne |> Skippable.bind (fun x -> x.Data)
      let rels1ToManyData = rels1.ToMany |> Skippable.bind (fun x -> x.Data)
      let rels2ToOneData = rels2.ToOne |> Skippable.bind (fun x -> x.Data)
      let rels2ToManyData = rels2.ToMany |> Skippable.bind (fun x -> x.Data)
      
      let mergedRels = ResourceBuilder.mergeRelationships (Include rels1) (Include rels2)

      let mergedToOneData = mergedRels.Value.ToOne |> Skippable.bind (fun x -> x.Data)
      let mergedToManyData = mergedRels.Value.ToMany |> Skippable.bind (fun x -> x.Data)

      test <@ mergedToOneData = getExpectedData rels1ToOneData rels2ToOneData @>
      test <@ mergedToManyData = getExpectedData rels1ToManyData rels2ToManyData @>
    }


module cleanRelationships =

  [<Fact>]
  let ``removes any empty relationships`` () =
    Property.check <| property {
      let! resource = GenX.auto<Resource<obj, Rels>>
      
      ResourceBuilder.cleanRelationships resource

      let toOneRel = resource.Relationships |> Skippable.bind (fun r -> r.ToOne)
      let toManyRel = resource.Relationships |> Skippable.bind (fun r -> r.ToMany)

      test <@ toOneRel <> Include ToOne.empty @>
      test <@ toManyRel <> Include ToMany.empty @>
    }


module buildAndGetRelated =

  [<Fact>]
  let ``returns resource with correct data`` () =
    Property.check <| property {
      let! identifier = GenX.auto
      let! selfUrl = GenX.auto
      let! attrs = GenX.auto
      let! rels = GenX.auto
      let! links = GenX.auto
      let! meta = GenX.auto
      let builder = getBuilder identifier selfUrl attrs rels links meta []

      let resource, _ = ResourceBuilder.buildAndGetRelated (fun _ _ -> ()) builder |> Async.RunSynchronously

      test <@ resource.Type = identifier.Type @>
      test <@ resource.Id = Include identifier.Id @>
      test <@ resource.Attributes = attrs @>
      test <@ resource.Relationships = Skip @>
      test <@ resource.Links = links @>
      test <@ resource.Meta |> Skippable.map ExpandoObject.toMap = meta @>
    }

  [<Fact>]
  let ``returns included builders as-is`` () =
    Property.check <| property {
      let! identifier = GenX.auto
      let! selfUrl = GenX.auto
      let! attrs = GenX.auto
      let! rels = GenX.auto
      let! links = GenX.auto
      let! meta = GenX.auto
      let childBuilder = 
        getBuilder identifier selfUrl attrs rels links meta []
        |> ResourceBuilder.box
      let builder = getBuilder identifier selfUrl attrs rels links meta [childBuilder]

      let _, included = ResourceBuilder.buildAndGetRelated (fun _ _ -> ()) builder |> Async.RunSynchronously

      test <@ Object.ReferenceEquals(included.Head, childBuilder) @>
    }

  [<Fact>]
  let ``calls addRelationship with the correct data`` () =
    Property.check <| property {
      let! identifier = GenX.auto
      let! selfUrl = GenX.auto
      let! attrs = GenX.auto
      let! rels = GenX.auto
      let! links = GenX.auto
      let! meta = GenX.auto
      let childBuilder = 
        getBuilder identifier selfUrl attrs rels links meta []
        |> ResourceBuilder.box
      let builder = getBuilder identifier selfUrl attrs rels links meta [childBuilder]

      let mutable calledData = None

      let _ = ResourceBuilder.buildAndGetRelated (fun id rels -> calledData <- Some (id, rels)) builder |> Async.RunSynchronously

      let calledData' = calledData
      test <@ calledData' = Some (identifier, rels) @>
    }


module getRelated =

  [<Fact>]
  let ``returns included builders as-is`` () =
    Property.check <| property {
      let! identifier = GenX.auto
      let! selfUrl = GenX.auto
      let! attrs = GenX.auto
      let! rels = GenX.auto
      let! links = GenX.auto
      let! meta = GenX.auto
      let childBuilder = 
        getBuilder identifier selfUrl attrs rels links meta []
        |> ResourceBuilder.box
      let builder = getBuilder identifier selfUrl attrs rels links meta [childBuilder]

      let included = ResourceBuilder.getRelated (fun _ _ -> ()) builder |> Async.RunSynchronously

      test <@ Object.ReferenceEquals(included.Head, childBuilder) @>
    }

  [<Fact>]
  let ``calls addRelationship with the correct data`` () =
    Property.check <| property {
      let! identifier = GenX.auto
      let! selfUrl = GenX.auto
      let! attrs = GenX.auto
      let! rels = GenX.auto
      let! links = GenX.auto
      let! meta = GenX.auto
      let childBuilder = 
        getBuilder identifier selfUrl attrs rels links meta []
        |> ResourceBuilder.box
      let builder = getBuilder identifier selfUrl attrs rels links meta [childBuilder]

      let mutable calledData = None

      let _ = ResourceBuilder.getRelated (fun id rels -> calledData <- Some (id, rels)) builder |> Async.RunSynchronously

      let calledData' = calledData
      test <@ calledData' = Some (identifier, rels) @>
    }
