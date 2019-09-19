module FSharp.JsonApi.Tests.ResourceBuildContextTests

open System
open Xunit
open Hedgehog
open Swensen.Unquote
open FSharp.JsonSkippable
open FSharp.JsonApi


type ResourceDiscriminator =
  | Obj of Resource<obj, obj>


let private create fields includes currentType selfUrl =
  { Fields = fields
    Includes = includes
    CurrentType = currentType
    SelfUrl = selfUrl }

module Gen =

  let buildCtx fieldsGen includesGen currentTypeGen selfUrlGen = gen {
    let! fields = fieldsGen
    let! includes = includesGen
    let! currentType = currentTypeGen
    let! selfUrl = selfUrlGen
    return create fields includes currentType selfUrl
  }

module UseField =

  let buildCtx currentType fieldsGen =
    Gen.buildCtx fieldsGen Gen.includePaths (Gen.constant currentType) (GenX.uri |> Gen.option)

  [<Fact>]
  let ``returns true if fields contain entry for current type and field`` () =
    Property.check <| property {
      let! currentType = Gen.memberName
      let! currentField = Gen.memberName
      let! ctx = buildCtx currentType (Gen.fieldsetsWith currentType currentField)
      test <@ ctx.UseField currentField = true @>
    }

  [<Fact>]
  let ``returns true if fields does not contain entry for current type`` () =
    Property.check <| property {
      let! currentType = Gen.memberName
      let! currentField = Gen.memberName
      let! ctx = buildCtx currentType (Gen.fieldsetsWithoutType currentType)
      test <@ ctx.UseField currentField = true @>
    }

  [<Fact>]
  let ``returns false if fields contains entry for current type but not field`` () =
    Property.check <| property {
      let! currentType = Gen.memberName
      let! currentField = Gen.memberName
      let! ctx = buildCtx currentType (Gen.fieldsetsWithTypeWithoutField currentType currentField)
      test <@ ctx.UseField currentField = false @>
    }


module UseExplicitField =

  let buildCtx currentType fieldsGen =
    Gen.buildCtx fieldsGen Gen.includePaths (Gen.constant currentType) (GenX.uri |> Gen.option)

  [<Fact>]
  let ``returns true if fields contain entry for current type and field`` () =
    Property.check <| property {
      let! currentType = Gen.memberName
      let! currentField = Gen.memberName
      let! ctx = buildCtx currentType (Gen.fieldsetsWith currentType currentField)
      test <@ ctx.UseExplicitField currentField = true @>
    }

  [<Fact>]
  let ``returns false if fields does not contain entry for current type`` () =
    Property.check <| property {
      let! currentType = Gen.memberName
      let! currentField = Gen.memberName
      let! ctx = buildCtx currentType (Gen.fieldsetsWithoutType currentType)
      test <@ ctx.UseExplicitField currentField = false @>
    }

  [<Fact>]
  let ``returns false if fields contains entry for current type but not field`` () =
    Property.check <| property {
      let! currentType = Gen.memberName
      let! currentField = Gen.memberName
      let! ctx = buildCtx currentType (Gen.fieldsetsWithTypeWithoutField currentType currentField)
      test <@ ctx.UseExplicitField currentField = false @>
    }


module GetAttribute_value_skippable =

  let buildCtx currentType fieldsGen =
    Gen.buildCtx fieldsGen Gen.includePaths (Gen.constant currentType) (GenX.uri |> Gen.option)

  [<Fact>]
  let ``returns the value as-is if fields contains entry for current type and field`` () =
    Property.check <| property {
      let! value = Gen.bool |> Gen.skippable
      let! currentType = Gen.memberName
      let! attrName = Gen.memberName
      let! ctx = buildCtx currentType (Gen.fieldsetsWith currentType attrName)
      test <@ ctx.GetAttribute(attrName, value) = value @>
    }

  [<Fact>]
  let ``returns the value as-is if fields does not contain entry for current type`` () =
    Property.check <| property {
      let! value = Gen.bool |> Gen.skippable
      let! currentType = Gen.memberName
      let! attrName = Gen.memberName
      let! ctx = buildCtx currentType (Gen.fieldsetsWithoutType currentType)
      test <@ ctx.GetAttribute(attrName, value) = value @>
    }

  [<Fact>]
  let ``returns Skip if fields contain entry for current type but not field`` () =
    Property.check <| property {
      let! value = Gen.bool |> Gen.skippable
      let! currentType = Gen.memberName
      let! attrName = Gen.memberName
      let! ctx = buildCtx currentType (Gen.fieldsetsWithTypeWithoutField currentType attrName)
      test <@ ctx.GetAttribute(attrName, value) = Skip @>
    }


module GetAttribute_value_nonSkippable =

  let buildCtx currentType fieldsGen =
    Gen.buildCtx fieldsGen Gen.includePaths (Gen.constant currentType) (GenX.uri |> Gen.option)

  [<Fact>]
  let ``returns the value wrapped in Include if fields contains entry for current type and field`` () =
    Property.check <| property {
      let! value = Gen.bool
      let! currentType = Gen.memberName
      let! attrName = Gen.memberName
      let! ctx = buildCtx currentType (Gen.fieldsetsWith currentType attrName)
      test <@ ctx.GetAttribute(attrName, value) = Include value @>
    }

  [<Fact>]
  let ``returns the value wrapped in Include if fields does not contain entry for current type`` () =
    Property.check <| property {
      let! value = Gen.bool
      let! currentType = Gen.memberName
      let! attrName = Gen.memberName
      let! ctx = buildCtx currentType (Gen.fieldsetsWithoutType currentType)
      test <@ ctx.GetAttribute(attrName, value) = Include value @>
    }

  [<Fact>]
  let ``returns Skip if fields contain entry for current type but not field`` () =
    Property.check <| property {
      let! value = Gen.bool
      let! currentType = Gen.memberName
      let! attrName = Gen.memberName
      let! ctx = buildCtx currentType (Gen.fieldsetsWithTypeWithoutField currentType attrName)
      test <@ ctx.GetAttribute(attrName, value) = Skip @>
    }


module GetAttribute_getVal_skippable =
  
  let buildCtx currentType fieldsGen =
    Gen.buildCtx fieldsGen Gen.includePaths (Gen.constant currentType) (GenX.uri |> Gen.option)

  [<Fact>]
  let ``returns the value as-is if fields contains entry for current type and field`` () =
    Property.check <| property {
      let! arg = Gen.bool
      let! value = Gen.bool |> Gen.skippable
      let! currentType = Gen.memberName
      let! attrName = Gen.memberName
      let! ctx = buildCtx currentType (Gen.fieldsetsWith currentType attrName)
      let getVal x = if x = arg then value else failwith "Unexpected argument"
      test <@ ctx.GetAttribute(attrName, arg, getVal) = value @>
    }

  [<Fact>]
  let ``returns the value as-is if fields does not contain entry for current type`` () =
    Property.check <| property {
      let! arg = Gen.bool
      let! value = Gen.bool |> Gen.skippable
      let! currentType = Gen.memberName
      let! attrName = Gen.memberName
      let! ctx = buildCtx currentType (Gen.fieldsetsWithoutType currentType)
      let getVal x = if x = arg then value else failwith "Unexpected argument"
      test <@ ctx.GetAttribute(attrName, arg, getVal) = value @>
    }

  [<Fact>]
  let ``returns Skip if fields contain entry for current type but not field`` () =
    Property.check <| property {
      let! arg = Gen.bool
      let! value = Gen.bool |> Gen.skippable
      let! currentType = Gen.memberName
      let! attrName = Gen.memberName
      let! ctx = buildCtx currentType (Gen.fieldsetsWithTypeWithoutField currentType attrName)
      let getVal x = if x = arg then value else failwith "Unexpected argument"
      test <@ ctx.GetAttribute(attrName, arg, getVal) = Skip @>
    }


module GetAttribute_getVal_nonSkippable =
  
  let buildCtx currentType fieldsGen =
    Gen.buildCtx fieldsGen Gen.includePaths (Gen.constant currentType) (GenX.uri |> Gen.option)

  [<Fact>]
  let ``returns the value wrapped in Include if fields contains entry for current type and field`` () =
    Property.check <| property {
      let! arg = Gen.bool
      let! value = Gen.bool
      let! currentType = Gen.memberName
      let! attrName = Gen.memberName
      let! ctx = buildCtx currentType (Gen.fieldsetsWith currentType attrName)
      let getVal x = if x = arg then value else failwith "Unexpected argument"
      test <@ ctx.GetAttribute(attrName, arg, getVal) = Include value @>
    }

  [<Fact>]
  let ``returns the value wrapped in Include if fields does not contain entry for current type`` () =
    Property.check <| property {
      let! arg = Gen.bool
      let! value = Gen.bool
      let! currentType = Gen.memberName
      let! attrName = Gen.memberName
      let! ctx = buildCtx currentType (Gen.fieldsetsWithoutType currentType)
      let getVal x = if x = arg then value else failwith "Unexpected argument"
      test <@ ctx.GetAttribute(attrName, arg, getVal) = Include value @>
    }

  [<Fact>]
  let ``returns Skip if fields contain entry for current type but not field`` () =
    Property.check <| property {
      let! arg = Gen.bool
      let! value = Gen.bool
      let! currentType = Gen.memberName
      let! attrName = Gen.memberName
      let! ctx = buildCtx currentType (Gen.fieldsetsWithTypeWithoutField currentType attrName)
      let getVal x = if x = arg then value else failwith "Unexpected argument"
      test <@ ctx.GetAttribute(attrName, arg, getVal) = Skip @>
    }


module GetExplicitAttribute_value_skippable =

  let buildCtx currentType fieldsGen =
    Gen.buildCtx fieldsGen Gen.includePaths (Gen.constant currentType) (GenX.uri |> Gen.option)

  [<Fact>]
  let ``returns the value as-is if fields contains entry for current type and field`` () =
    Property.check <| property {
      let! value = Gen.bool |> Gen.skippable
      let! currentType = Gen.memberName
      let! attrName = Gen.memberName
      let! ctx = buildCtx currentType (Gen.fieldsetsWith currentType attrName)
      test <@ ctx.GetExplicitAttribute(attrName, value) = value @>
    }

  [<Fact>]
  let ``returns skip if fields does not contain entry for current type`` () =
    Property.check <| property {
      let! value = Gen.bool |> Gen.skippable
      let! currentType = Gen.memberName
      let! attrName = Gen.memberName
      let! ctx = buildCtx currentType (Gen.fieldsetsWithoutType currentType)
      test <@ ctx.GetExplicitAttribute(attrName, value) = Skip @>
    }

  [<Fact>]
  let ``returns Skip if fields contain entry for current type but not field`` () =
    Property.check <| property {
      let! value = Gen.bool |> Gen.skippable
      let! currentType = Gen.memberName
      let! attrName = Gen.memberName
      let! ctx = buildCtx currentType (Gen.fieldsetsWithTypeWithoutField currentType attrName)
      test <@ ctx.GetExplicitAttribute(attrName, value) = Skip @>
    }


module GetExplicitAttribute_value_nonSkippable =

  let buildCtx currentType fieldsGen =
    Gen.buildCtx fieldsGen Gen.includePaths (Gen.constant currentType) (GenX.uri |> Gen.option)

  [<Fact>]
  let ``returns the value wrapped in Include if fields contains entry for current type and field`` () =
    Property.check <| property {
      let! value = Gen.bool
      let! currentType = Gen.memberName
      let! attrName = Gen.memberName
      let! ctx = buildCtx currentType (Gen.fieldsetsWith currentType attrName)
      test <@ ctx.GetExplicitAttribute(attrName, value) = Include value @>
    }

  [<Fact>]
  let ``returns skip if fields does not contain entry for current type`` () =
    Property.check <| property {
      let! value = Gen.bool
      let! currentType = Gen.memberName
      let! attrName = Gen.memberName
      let! ctx = buildCtx currentType (Gen.fieldsetsWithoutType currentType)
      test <@ ctx.GetExplicitAttribute(attrName, value) = Skip @>
    }

  [<Fact>]
  let ``returns Skip if fields contain entry for current type but not field`` () =
    Property.check <| property {
      let! value = Gen.bool
      let! currentType = Gen.memberName
      let! attrName = Gen.memberName
      let! ctx = buildCtx currentType (Gen.fieldsetsWithTypeWithoutField currentType attrName)
      test <@ ctx.GetExplicitAttribute(attrName, value) = Skip @>
    }


module GetExplicitAttribute_getVal_skippable =

  let buildCtx currentType fieldsGen =
    Gen.buildCtx fieldsGen Gen.includePaths (Gen.constant currentType) (GenX.uri |> Gen.option)

  [<Fact>]
  let ``returns the value as-is if fields contains entry for current type and field`` () =
    Property.check <| property {
      let! arg = Gen.bool
      let! value = Gen.bool |> Gen.skippable
      let! currentType = Gen.memberName
      let! attrName = Gen.memberName
      let! ctx = buildCtx currentType (Gen.fieldsetsWith currentType attrName)
      let getVal x = if x = arg then value else failwith "Unexpected argument"
      test <@ ctx.GetExplicitAttribute(attrName, arg, getVal) = value @>
    }

  [<Fact>]
  let ``returns skip if fields does not contain entry for current type`` () =
    Property.check <| property {
      let! arg = Gen.bool
      let! value = Gen.bool |> Gen.skippable
      let! currentType = Gen.memberName
      let! attrName = Gen.memberName
      let! ctx = buildCtx currentType (Gen.fieldsetsWithoutType currentType)
      let getVal x = if x = arg then value else failwith "Unexpected argument"
      test <@ ctx.GetExplicitAttribute(attrName, arg, getVal) = Skip @>
    }

  [<Fact>]
  let ``returns Skip if fields contain entry for current type but not field`` () =
    Property.check <| property {
      let! arg = Gen.bool
      let! value = Gen.bool |> Gen.skippable
      let! currentType = Gen.memberName
      let! attrName = Gen.memberName
      let! ctx = buildCtx currentType (Gen.fieldsetsWithTypeWithoutField currentType attrName)
      let getVal x = if x = arg then value else failwith "Unexpected argument"
      test <@ ctx.GetExplicitAttribute(attrName, arg, getVal) = Skip @>
    }


module GetExplicitAttribute_getVal_nonSkippable =

  let buildCtx currentType fieldsGen =
    Gen.buildCtx fieldsGen Gen.includePaths (Gen.constant currentType) (GenX.uri |> Gen.option)

  [<Fact>]
  let ``returns the value wrapped in Include if fields contains entry for current type and field`` () =
    Property.check <| property {
      let! arg = Gen.bool
      let! value = Gen.bool
      let! currentType = Gen.memberName
      let! attrName = Gen.memberName
      let! ctx = buildCtx currentType (Gen.fieldsetsWith currentType attrName)
      let getVal x = if x = arg then value else failwith "Unexpected argument"
      test <@ ctx.GetExplicitAttribute(attrName, arg, getVal) = Include value @>
    }

  [<Fact>]
  let ``returns skip if fields does not contain entry for current type`` () =
    Property.check <| property {
      let! arg = Gen.bool
      let! value = Gen.bool
      let! currentType = Gen.memberName
      let! attrName = Gen.memberName
      let! ctx = buildCtx currentType (Gen.fieldsetsWithoutType currentType)
      let getVal x = if x = arg then value else failwith "Unexpected argument"
      test <@ ctx.GetExplicitAttribute(attrName, arg, getVal) = Skip @>
    }

  [<Fact>]
  let ``returns Skip if fields contain entry for current type but not field`` () =
    Property.check <| property {
      let! arg = Gen.bool
      let! value = Gen.bool
      let! currentType = Gen.memberName
      let! attrName = Gen.memberName
      let! ctx = buildCtx currentType (Gen.fieldsetsWithTypeWithoutField currentType attrName)
      let getVal x = if x = arg then value else failwith "Unexpected argument"
      test <@ ctx.GetExplicitAttribute(attrName, arg, getVal) = Skip @>
    }


module IncludeToMany_arg_async =

  [<Fact>]
  let ``throws if relatedLink or selfLink is specified and ctx contains no self URL`` () =
    Property.check <| property {
      let! relName = Gen.memberName
      let! arg = Gen.bool
      let! related = Gen.resourceIdentifier |> GenX.cList 0 5
      let getRelated (x: bool) = async.Return related
      let getRelatedBuilder (ctx: ResourceBuildContext) related = 
        ResourceBuilder.Create(Obj, related)
      let! ctx = Gen.buildCtx Gen.fieldsets Gen.includePaths Gen.memberName (Gen.constant None)

      raises <@ ctx.IncludeToMany(relName, arg, getRelated, getRelatedBuilder, relatedLink = true) |> Async.RunSynchronously @>
      raises <@ ctx.IncludeToMany(relName, arg, getRelated, getRelatedBuilder, selfLink = true) |> Async.RunSynchronously @>
    }

  [<Fact>]
  let ``does not throw if no links are specified and ctx contains no self URL`` () =
    Property.check <| property {
      let! relName = Gen.memberName
      let! arg = Gen.bool
      let! related = Gen.resourceIdentifier |> GenX.cList 0 5
      let getRelated (x: bool) = async.Return related
      let getRelatedBuilder (ctx: ResourceBuildContext) related = 
        ResourceBuilder.Create(Obj, related)
      let! ctx = Gen.buildCtx Gen.fieldsets Gen.includePaths Gen.memberName (Gen.constant None)

      ctx.IncludeToMany(relName, arg, getRelated, getRelatedBuilder) |> Async.RunSynchronously |> ignore
    }

  [<Fact>]
  let ``if links are specified, ctx contains self URL, and relationship is included in sparse fieldsets, relationship has correct links`` () =
    Property.check <| property {
      let! useRelatedLink = Gen.bool
      let! useSelfLink = Gen.bool
      where (useRelatedLink || useSelfLink)
      let! currentType = Gen.memberName
      let! relName = Gen.memberName
      let! selfUrl = GenX.uri
      let! arg = Gen.bool
      let! related = Gen.resourceIdentifier |> GenX.cList 0 5
      let getRelated (x: bool) = async.Return related
      let getRelatedBuilder (ctx: ResourceBuildContext) related = 
        ResourceBuilder.Create(Obj, related)
      let! ctx = 
        Gen.buildCtx 
          (Gen.choice [Gen.fieldsetsWith currentType relName; Gen.fieldsetsWithoutType currentType])
          Gen.includePaths (Gen.constant currentType) (Gen.constant (Some selfUrl))

      let rels, _ = ctx.IncludeToMany(relName, arg, getRelated, getRelatedBuilder, relatedLink = useRelatedLink, selfLink = useSelfLink) |> Async.RunSynchronously

      let (Links links) = rels.Value.Links.Value

      if useRelatedLink then
        let relatedUrl = links.TryFind(Link.related).Value.Href.Value
        test <@ relatedUrl = selfUrl.AddSegment relName @>
      else
        test <@ not <| links.ContainsKey Link.related @>

      if useSelfLink then
        let relatedUrl = links.TryFind(Link.self).Value.Href.Value
        test <@ relatedUrl = selfUrl.AddSegments ["relationships"; relName] @>
      else
        test <@ not <| links.ContainsKey Link.self @>
    }

  [<Fact>]
  let ``if type is included in sparse fieldsets but relationship is not, returned relationship is Skip`` () =
    Property.check <| property {
      let! currentType = Gen.memberName
      let! relName = Gen.memberName
      let! arg = Gen.bool
      let! related = Gen.resourceIdentifier |> GenX.cList 0 5
      let getRelated (x: bool) = async.Return related
      let getRelatedBuilder (ctx: ResourceBuildContext) related = 
        ResourceBuilder.Create(Obj, related)
      let! ctx =
        Gen.buildCtx
          (Gen.fieldsetsWithTypeWithoutField currentType relName)
          Gen.includePaths (Gen.constant currentType) (GenX.uri |> Gen.option)

      let rels, _ = ctx.IncludeToMany(relName, arg, getRelated, getRelatedBuilder) |> Async.RunSynchronously

      test <@ rels = Skip @>
    }

  [<Fact>]
  let ``if relationship is included in sparse fieldsets but not at the head of an include path, returns a relationship with no data`` () =
    Property.check <| property {
      let! currentType = Gen.memberName
      let! relName = Gen.memberName
      let! arg = Gen.bool
      let! related = Gen.resourceIdentifier |> GenX.cList 0 5
      let getRelated (x: bool) = async.Return related
      let getRelatedBuilder (ctx: ResourceBuildContext) related = 
        ResourceBuilder.Create(Obj, related)
      let! ctx =
        Gen.buildCtx 
          (Gen.choice [Gen.fieldsetsWith currentType relName; Gen.fieldsetsWithoutType currentType])
          (Gen.includePathsWithoutHead relName) (Gen.constant currentType) (GenX.uri |> Gen.option)

      let rels, _ = ctx.IncludeToMany(relName, arg, getRelated, getRelatedBuilder) |> Async.RunSynchronously

      test <@ rels.Value.Data = Skip @>
    }

  [<Fact>]
  let ``if relationship is included in sparse fieldsets and at the head of an include path, returns a relationship with correct data`` () =
    Property.check <| property {
      let! currentType = Gen.memberName
      let! relName = Gen.memberName
      let! arg = Gen.bool
      let! related = Gen.resourceIdentifier |> GenX.cList 0 5
      let getRelated (x: bool) = async.Return related
      let getRelatedBuilder (ctx: ResourceBuildContext) related = 
        ResourceBuilder.Create(Obj, related)
      let! ctx =
        Gen.buildCtx
          (Gen.choice [Gen.fieldsetsWith currentType relName; Gen.fieldsetsWithoutType currentType])
          (Gen.includePathsWithHead relName) (Gen.constant currentType) (GenX.uri |> Gen.option)

      let rels, _ = ctx.IncludeToMany(relName, arg, getRelated, getRelatedBuilder) |> Async.RunSynchronously

      test <@ rels.Value.Data.Value = related @>
    }

  [<Fact>]
  let ``if the relationship is at the head of an include path, returns the related builders`` () =
    Property.check <| property {
      let! currentType = Gen.memberName
      let! relName = Gen.memberName
      let! arg = Gen.bool
      let! related = Gen.resourceIdentifier |> GenX.cList 0 5
      let getRelated (x: bool) = async.Return related
      let getRelatedBuilder (ctx: ResourceBuildContext) related = 
        ResourceBuilder.Create(Obj, related)
      let! ctx =
        Gen.buildCtx
          (Gen.choice [Gen.fieldsetsWith currentType relName; Gen.fieldsetsWithoutType currentType])
          (Gen.includePathsWithHead relName) (Gen.constant currentType) (GenX.uri |> Gen.option)

      let _, builders = ctx.IncludeToMany(relName, arg, getRelated, getRelatedBuilder) |> Async.RunSynchronously

      let builderIds = builders |> List.map (fun b -> b.Identifier)
      let relatedIds = related
      test <@ builderIds = relatedIds @>
    }

  [<Fact>]
  let ``if the relationship is at the head of an include path and there are related resources, the related builder function receives a ctx with correct data`` () =
    Property.check <| property {
      let! currentType = Gen.memberName
      let! relName = Gen.memberName
      let! arg = Gen.bool
      let! related = Gen.resourceIdentifier |> GenX.cList 1 5
      let! includePaths = Gen.includePathsWithHead relName

      let getRelated (x: bool) = async.Return related

      let mutable receivedCtx = Map.empty

      let getSelfUrl (id: ResourceIdentifier) =
        Uri(sprintf "http://example.com/%s/%s" id.Type id.Id)

      let getRelatedBuilder (ctx: ResourceBuildContext) related = 
        receivedCtx <- receivedCtx.Add(related, ctx)
        ResourceBuilder
          .Create(Obj, related)
          .WithSelfUrl(getSelfUrl related)
      let! ctx = Gen.buildCtx Gen.fieldsets (Gen.constant includePaths) (Gen.constant currentType) (GenX.uri |> Gen.option)

      let _ = ctx.IncludeToMany(relName, arg, getRelated, getRelatedBuilder) |> Async.RunSynchronously
      
      // Only the include paths where it was at the head, each with the previous head removed
      let expectedIncludePaths =
        includePaths
        |> List.filter (fun path -> path |> List.tryHead = Some relName)
        |> List.map List.tail
      let distinctPaths = receivedCtx |> Map.toList |> List.map (fun (_, ctx) -> ctx.Includes) |> List.distinct
      test <@ distinctPaths = [expectedIncludePaths] @>

      // CurrentType set to the type from the new builder
      let receivedCtx' = receivedCtx
      test <@ receivedCtx' |> Map.forall (fun id ctx -> ctx.CurrentType = id.Type) @>

      // SelfUrl set to the url from the new builder
      let receivedCtx' = receivedCtx
      test <@ receivedCtx' |> Map.forall (fun id ctx -> ctx.SelfUrl = Some (getSelfUrl id)) @>
    }

  [<Fact>]
  let ``if the relationship is at the head of an include path and there are related resources, getRelated is called with the specified arg`` () =
    Property.check <| property {
      let! currentType = Gen.memberName
      let! relName = Gen.memberName
      let! arg = Gen.bool
      let! related = Gen.resourceIdentifier |> GenX.cList 1 5

      let mutable wasCalled = false
      let getRelated (x: bool) =
        async {
          wasCalled <- true
          return if x = arg then related else failwith "Unexpected argument"
        }
      let getRelatedBuilder (ctx: ResourceBuildContext) related = 
        ResourceBuilder.Create(Obj, related)
      let! ctx = Gen.buildCtx Gen.fieldsets (Gen.includePathsWithHead relName) (Gen.constant currentType) (GenX.uri |> Gen.option)

      let _ = ctx.IncludeToMany(relName, arg, getRelated, getRelatedBuilder) |> Async.RunSynchronously
      
      let wasCalled' = wasCalled
      test <@ wasCalled' @>
    }

  [<Fact>]
  let ``if the relationship is at the head of an include path but there are no related resources, getRelatedBuilder is not called`` () =
    Property.check <| property {
      let! currentType = Gen.memberName
      let! relName = Gen.memberName
      let! arg = Gen.bool
      let getRelated x = async.Return []
      let getRelatedBuilder (ctx: ResourceBuildContext) related = failwith "Should not be called"
      let! ctx = Gen.buildCtx Gen.fieldsets (Gen.includePathsWithHead relName) (Gen.constant currentType) (GenX.uri |> Gen.option)

      ctx.IncludeToMany(relName, arg, getRelated, getRelatedBuilder) |> Async.RunSynchronously |> ignore
    }

  [<Fact>]
  let ``if the relationship is not at the head of an include path, getRelated and getRelatedBuilder are not called`` () =
    Property.check <| property {
      let! currentType = Gen.memberName
      let! relName = Gen.memberName
      let! arg = Gen.bool
      let getRelated x : Async<ResourceIdentifier list> = failwith "Should not be called"
      let getRelatedBuilder (ctx: ResourceBuildContext) related = failwith "Should not be called"
      let! ctx = Gen.buildCtx Gen.fieldsets (Gen.includePathsWithoutHead relName) (Gen.constant currentType) (GenX.uri |> Gen.option)

      ctx.IncludeToMany(relName, arg, getRelated, getRelatedBuilder) |> Async.RunSynchronously |> ignore
    }


module IncludeToMany_arg_sync =

  [<Fact>]
  let ``can resolve overload`` () =
    let __ (ctx: ResourceBuildContext) =
      let arg: char = failwith ""
      let getRelated : char -> int list = failwith ""
      let getRelatedBuilder : ResourceBuildContext -> int -> ResourceBuilder<obj, obj> = failwith ""
      ctx.IncludeToMany("", arg, getRelated, getRelatedBuilder)
    true

module IncludeToMany_noArg =

  [<Fact>]
  let ``can resolve overload`` () =
    let __ (ctx: ResourceBuildContext) =
      let related : int list = failwith ""
      let getRelatedBuilder : ResourceBuildContext -> int -> ResourceBuilder<obj, obj> = failwith ""
      ctx.IncludeToMany("", related, getRelatedBuilder)
    true


module IncludeToOne_arg_async_option =

  [<Fact>]
  let ``throws if relatedLink or selfLink is specified and ctx contains no self URL`` () =
    Property.check <| property {
      let! relName = Gen.memberName
      let! arg = Gen.bool
      let! related = Gen.resourceIdentifier |> Gen.option
      let getRelated (x: bool) = async.Return related
      let getRelatedBuilder (ctx: ResourceBuildContext) related = 
        ResourceBuilder.Create(Obj, related)
      let! ctx = Gen.buildCtx Gen.fieldsets Gen.includePaths Gen.memberName (Gen.constant None)

      raises <@ ctx.IncludeToOne(relName, arg, getRelated, getRelatedBuilder, relatedLink = true) |> Async.RunSynchronously @>
      raises <@ ctx.IncludeToOne(relName, arg, getRelated, getRelatedBuilder, selfLink = true) |> Async.RunSynchronously @>
    }

  [<Fact>]
  let ``does not throw if no links are specified and ctx contains no self URL`` () =
    Property.check <| property {
      let! relName = Gen.memberName
      let! arg = Gen.bool
      let! related = Gen.resourceIdentifier |> Gen.option
      let getRelated (x: bool) = async.Return related
      let getRelatedBuilder (ctx: ResourceBuildContext) related = 
        ResourceBuilder.Create(Obj, related)
      let! ctx = Gen.buildCtx Gen.fieldsets Gen.includePaths Gen.memberName (Gen.constant None)

      ctx.IncludeToOne(relName, arg, getRelated, getRelatedBuilder) |> Async.RunSynchronously |> ignore
    }

  [<Fact>]
  let ``if links are specified, ctx contains self URL, and relationship is included in sparse fieldsets, relationship has correct links`` () =
    Property.check <| property {
      let! useRelatedLink = Gen.bool
      let! useSelfLink = Gen.bool
      where (useRelatedLink || useSelfLink)
      let! currentType = Gen.memberName
      let! relName = Gen.memberName
      let! selfUrl = GenX.uri
      let! arg = Gen.bool
      let! related = Gen.resourceIdentifier |> Gen.option
      let getRelated (x: bool) = async.Return related
      let getRelatedBuilder (ctx: ResourceBuildContext) related = 
        ResourceBuilder.Create(Obj, related)
      let! ctx = 
        Gen.buildCtx 
          (Gen.choice [Gen.fieldsetsWith currentType relName; Gen.fieldsetsWithoutType currentType])
          Gen.includePaths (Gen.constant currentType) (Gen.constant (Some selfUrl))

      let rels, _ = ctx.IncludeToOne(relName, arg, getRelated, getRelatedBuilder, relatedLink = useRelatedLink, selfLink = useSelfLink) |> Async.RunSynchronously

      let (Links links) = rels.Value.Links.Value

      if useRelatedLink then
        let relatedUrl = links.TryFind(Link.related).Value.Href.Value
        test <@ relatedUrl = selfUrl.AddSegment relName @>
      else
        test <@ not <| links.ContainsKey Link.related @>

      if useSelfLink then
        let relatedUrl = links.TryFind(Link.self).Value.Href.Value
        test <@ relatedUrl = selfUrl.AddSegments ["relationships"; relName] @>
      else
        test <@ not <| links.ContainsKey Link.self @>
    }

  [<Fact>]
  let ``if type is included in sparse fieldsets but relationship is not, returned relationship is Skip`` () =
    Property.check <| property {
      let! currentType = Gen.memberName
      let! relName = Gen.memberName
      let! arg = Gen.bool
      let! related = Gen.resourceIdentifier |> Gen.option
      let getRelated (x: bool) = async.Return related
      let getRelatedBuilder (ctx: ResourceBuildContext) related = 
        ResourceBuilder.Create(Obj, related)
      let! ctx =
        Gen.buildCtx
          (Gen.fieldsetsWithTypeWithoutField currentType relName)
          Gen.includePaths (Gen.constant currentType) (GenX.uri |> Gen.option)

      let rels, _ = ctx.IncludeToOne(relName, arg, getRelated, getRelatedBuilder) |> Async.RunSynchronously

      test <@ rels = Skip @>
    }

  [<Fact>]
  let ``if relationship is included in sparse fieldsets but not at the head of an include path, returns a relationship with no data`` () =
    Property.check <| property {
      let! currentType = Gen.memberName
      let! relName = Gen.memberName
      let! arg = Gen.bool
      let! related = Gen.resourceIdentifier |> Gen.option
      let getRelated (x: bool) = async.Return related
      let getRelatedBuilder (ctx: ResourceBuildContext) related = 
        ResourceBuilder.Create(Obj, related)
      let! ctx =
        Gen.buildCtx 
          (Gen.choice [Gen.fieldsetsWith currentType relName; Gen.fieldsetsWithoutType currentType])
          (Gen.includePathsWithoutHead relName) (Gen.constant currentType) (GenX.uri |> Gen.option)

      let rels, _ = ctx.IncludeToOne(relName, arg, getRelated, getRelatedBuilder) |> Async.RunSynchronously

      test <@ rels.Value.Data = Skip @>
    }

  [<Fact>]
  let ``if relationship is included in sparse fieldsets and at the head of an include path, returns a relationship with correct data`` () =
    Property.check <| property {
      let! currentType = Gen.memberName
      let! relName = Gen.memberName
      let! arg = Gen.bool
      let! related = Gen.resourceIdentifier |> Gen.option
      let getRelated (x: bool) = async.Return related
      let getRelatedBuilder (ctx: ResourceBuildContext) related = 
        ResourceBuilder.Create(Obj, related)
      let! ctx =
        Gen.buildCtx
          (Gen.choice [Gen.fieldsetsWith currentType relName; Gen.fieldsetsWithoutType currentType])
          (Gen.includePathsWithHead relName) (Gen.constant currentType) (GenX.uri |> Gen.option)

      let rels, _ = ctx.IncludeToOne(relName, arg, getRelated, getRelatedBuilder) |> Async.RunSynchronously

      test <@ rels.Value.Data.Value = related @>
    }

  [<Fact>]
  let ``if the relationship is at the head of an include path, returns the related builder`` () =
    Property.check <| property {
      let! currentType = Gen.memberName
      let! relName = Gen.memberName
      let! arg = Gen.bool
      let! related = Gen.resourceIdentifier |> Gen.option
      let getRelated (x: bool) = async.Return related
      let getRelatedBuilder (ctx: ResourceBuildContext) related = 
        ResourceBuilder.Create(Obj, related)
      let! ctx =
        Gen.buildCtx
          (Gen.choice [Gen.fieldsetsWith currentType relName; Gen.fieldsetsWithoutType currentType])
          (Gen.includePathsWithHead relName) (Gen.constant currentType) (GenX.uri |> Gen.option)

      let _, builder = ctx.IncludeToOne(relName, arg, getRelated, getRelatedBuilder) |> Async.RunSynchronously

      let builderId = builder |> Option.map (fun b -> b.Identifier)
      let relatedId = related
      test <@ builderId = relatedId @>
    }

  [<Fact>]
  let ``if the relationship is at the head of an include path and there is a related resource, the related builder function receives a ctx with correct data`` () =
    Property.check <| property {
      let! currentType = Gen.memberName
      let! relName = Gen.memberName
      let! arg = Gen.bool
      let! related = Gen.resourceIdentifier |> Gen.map Some
      let! includePaths = Gen.includePathsWithHead relName

      let getRelated (x: bool) = async.Return related

      let mutable receivedCtx = Map.empty

      let getSelfUrl (id: ResourceIdentifier) =
        Uri(sprintf "http://example.com/%s/%s" id.Type id.Id)

      let getRelatedBuilder (ctx: ResourceBuildContext) related = 
        receivedCtx <- receivedCtx.Add(related, ctx)
        ResourceBuilder
          .Create(Obj, related)
          .WithSelfUrl(getSelfUrl related)
      let! ctx = Gen.buildCtx Gen.fieldsets (Gen.constant includePaths) (Gen.constant currentType) (GenX.uri |> Gen.option)

      let _ = ctx.IncludeToOne(relName, arg, getRelated, getRelatedBuilder) |> Async.RunSynchronously
      
      // Only the include paths where it was at the head, each with the previous head removed
      let expectedIncludePaths =
        includePaths
        |> List.filter (fun path -> path |> List.tryHead = Some relName)
        |> List.map List.tail
      let distinctPaths = receivedCtx |> Map.toList |> List.map (fun (_, ctx) -> ctx.Includes) |> List.distinct
      test <@ distinctPaths = [expectedIncludePaths] @>

      // CurrentType set to the type from the new builder
      let receivedCtx' = receivedCtx
      test <@ receivedCtx' |> Map.forall (fun id ctx -> ctx.CurrentType = id.Type) @>

      // SelfUrl set to the url from the new builder
      let receivedCtx' = receivedCtx
      test <@ receivedCtx' |> Map.forall (fun id ctx -> ctx.SelfUrl = Some (getSelfUrl id)) @>
    }

  [<Fact>]
  let ``if the relationship is at the head of an include path and there is a related resource, getRelated is called with the specified arg`` () =
    Property.check <| property {
      let! currentType = Gen.memberName
      let! relName = Gen.memberName
      let! arg = Gen.bool
      let! related = Gen.resourceIdentifier |> Gen.map Some

      let mutable wasCalled = false
      let getRelated (x: bool) =
        async {
          wasCalled <- true
          return if x = arg then related else failwith "Unexpected argument"
        }
        
      let getRelatedBuilder (ctx: ResourceBuildContext) related = 
        ResourceBuilder.Create(Obj, related)
      let! ctx = Gen.buildCtx Gen.fieldsets (Gen.includePathsWithHead relName) (Gen.constant currentType) (GenX.uri |> Gen.option)

      let _ = ctx.IncludeToOne(relName, arg, getRelated, getRelatedBuilder) |> Async.RunSynchronously
      
      let wasCalled' = wasCalled
      test <@ wasCalled' @>
    }

  [<Fact>]
  let ``if the relationship is at the head of an include path but there is no related resource, getRelatedBuilder is not called`` () =
    Property.check <| property {
      let! currentType = Gen.memberName
      let! relName = Gen.memberName
      let! arg = Gen.bool
      let getRelated x = async.Return None
      let getRelatedBuilder (ctx: ResourceBuildContext) related = failwith "Should not be called"
      let! ctx = Gen.buildCtx Gen.fieldsets (Gen.includePathsWithHead relName) (Gen.constant currentType) (GenX.uri |> Gen.option)

      ctx.IncludeToOne(relName, arg, getRelated, getRelatedBuilder) |> Async.RunSynchronously |> ignore
    }

  [<Fact>]
  let ``if the relationship is not at the head of an include path, getRelated and getRelatedBuilder are not called`` () =
    Property.check <| property {
      let! currentType = Gen.memberName
      let! relName = Gen.memberName
      let! arg = Gen.bool
      let getRelated x : Async<ResourceIdentifier option> = failwith "Should not be called"
      let getRelatedBuilder (ctx: ResourceBuildContext) related = failwith "Should not be called"
      let! ctx = Gen.buildCtx Gen.fieldsets (Gen.includePathsWithoutHead relName) (Gen.constant currentType) (GenX.uri |> Gen.option)

      ctx.IncludeToOne(relName, arg, getRelated, getRelatedBuilder) |> Async.RunSynchronously |> ignore
    }


module IncludeToOne_arg_sync_option =

  [<Fact>]
  let ``can resolve overload`` () =
    let __ (ctx: ResourceBuildContext) =
      let arg: char = failwith ""
      let getRelated : char -> int option = failwith ""
      let getRelatedBuilder : ResourceBuildContext -> int -> ResourceBuilder<obj, obj> = failwith ""
      ctx.IncludeToOne("", arg, getRelated, getRelatedBuilder)
    true


module IncludeToOne_noArg_option =

  [<Fact>]
  let ``can resolve overload`` () =
    let __ (ctx: ResourceBuildContext) =
      let related: int option = failwith ""
      let getRelatedBuilder : ResourceBuildContext -> int -> ResourceBuilder<obj, obj> = failwith ""
      ctx.IncludeToOne("", related, getRelatedBuilder)
    true


module IncludeToOne_arg_async_noOption =

  [<Fact>]
  let ``can resolve overload`` () =
    let __ (ctx: ResourceBuildContext) =
      let arg: char = failwith ""
      let getRelated : char -> Async<int> = failwith ""
      let getRelatedBuilder : ResourceBuildContext -> int -> ResourceBuilder<obj, obj> = failwith ""
      ctx.IncludeToOne("", arg, getRelated, getRelatedBuilder)
    true


module IncludeToOne_arg_sync_noOption =

  [<Fact>]
  let ``can resolve overload`` () =
    let __ (ctx: ResourceBuildContext) =
      let arg: char = failwith ""
      let getRelated : char -> int = failwith ""
      let getRelatedBuilder : ResourceBuildContext -> int -> ResourceBuilder<obj, obj> = failwith ""
      ctx.IncludeToOne("", arg, getRelated, getRelatedBuilder)
    true
