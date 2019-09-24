namespace FSharp.JsonApi

open System
open FSharp.JsonSkippable


/// Represents data needed when building resources.
type ResourceBuildContext =
  internal
    { Fields: Fieldsets
      Includes: IncludePath list
      CurrentType: TypeName
      SelfUrl: Uri option }

  /// Indicates if the given field on the given or current type should be included in the
  /// response (sparse fieldsets).
  member this.UseField (fieldName: FieldName) : bool =
    this.CurrentType
    |> this.Fields.TryFind
    |> Option.map (Set.contains fieldName)
    |> Option.defaultValue true

  /// Indicates if the given field on the given or current type has been explicitly specified
  /// in the request (sparse fieldsets). This can be used e.g. for expensive fields
  /// that should only be included if explicitly specified. For normal sparse fieldset
  /// usage, see UseField.
  member this.UseExplicitField (fieldName: FieldName) : bool =
    this.CurrentType
    |> this.Fields.TryFind
    |> Option.map (Set.contains fieldName)
    |> Option.defaultValue false

  /// Returns the specified value if the attribute is not excluded using sparse fieldsets.
  /// Otherwise returns Skip.
  member this.GetAttribute (attrName: AttributeName, value: Skippable<'a>) : Skippable<'a> =
    if this.UseField attrName then value else Skip

  /// Returns the result of the specified getter if the attribute is not excluded using
  /// sparse fieldsets. Otherwise returns Skip.
  member this.GetAttribute (attrName: AttributeName, arg, getValue: 'arg -> Skippable<'a>) : Skippable<'a> =
    if this.UseField attrName then getValue arg else Skip

  /// Returns the specified value if the attribute is not excluded using sparse fieldsets.
  /// Otherwise returns Skip.
  member this.GetAttribute (attrName: AttributeName, value: Async<Skippable<'a>>) : Async<Skippable<'a>> =
    if this.UseField attrName then value else async.Return Skip

  /// Returns the specified value if the attribute is explicitly included using
  /// sparse fieldsets. Otherwise returns Skip.
  member this.GetExplicitAttribute (attrName: AttributeName, value: Skippable<'a>) : Skippable<'a> =
    if this.UseExplicitField attrName then value else Skip

  /// Returns the result of the specified getter if the attribute is explicitly
  /// included using sparse fieldsets. Otherwise returns Skip.
  member this.GetExplicitAttribute (attrName: AttributeName, arg, getValue: 'arg -> Skippable<'a>) : Skippable<'a> =
    if this.UseExplicitField attrName then getValue arg else Skip

  /// Returns the specified value if the attribute is explicitly included using
  /// sparse fieldsets. Otherwise returns Skip.
  member this.GetExplicitAttribute (attrName: AttributeName, value: Async<Skippable<'a>>) : Async<Skippable<'a>> =
    if this.UseExplicitField attrName then value else async.Return Skip

  member private this.GetRelatedBuilders
      ( relName: RelationshipName,
        arg: 'arg,
        getRelated: 'arg -> Async<'related list>,
        getRelatedBuilder: ResourceBuildContext -> 'related -> ResourceBuilder<'relatedAttrs, 'relatedRels>
      ) : Async<ResourceBuilder<obj, obj> list option> =
    async {
      let includePaths = this.Includes |> List.filter (fun s -> s |> List.tryHead = Some relName)
      match includePaths with
      | [] -> return None
      | paths ->
          let! related = getRelated arg
          let relatedBuilders =
            related
            |> List.map (fun r ->
                let relatedBuilder = r |> getRelatedBuilder this
                let subCtx =
                  { this with
                      Includes = paths |> List.map List.tail
                      CurrentType = relatedBuilder.Identifier.Type
                      SelfUrl = relatedBuilder.SelfUrl }
                r, subCtx
            )
            |> List.map (fun (r, subCtx) -> getRelatedBuilder subCtx r |> ResourceBuilder.box)
          return Some relatedBuilders
    }

  /// Based on sparse fieldsets and includes, returns resource builders for the related
  /// resources and a relationship with correct linkage data as well as the specified
  /// links (an exception is thrown if links are specified but the resource builder
  /// does not supply a self link).
  member this.IncludeToMany
      ( relName: RelationshipName,
        arg: 'arg,
        getRelated: 'arg -> Async<'related list>,
        getRelatedBuilder: ResourceBuildContext -> 'related -> ResourceBuilder<'relatedAttrs, 'relatedRels>,
        ?relatedLink: bool,
        ?selfLink: bool
      ) : Async<ToMany Skippable * ResourceBuilder<obj, obj> list> =
    async {
      let addLinks =
        match this.SelfUrl with
        | None ->
            if relatedLink = Some true || selfLink = Some true then
              invalidOp (sprintf "Attempted to add link to relationship %s on type %s, but the builder for this type did not provide a self link" relName this.CurrentType)
            else id
        | Some url ->
            if relatedLink = Some true then ToMany.setRelatedLink url relName else id
            >> if selfLink = Some true then ToMany.setSelfLink url relName else id
      let! relatedBuilders = this.GetRelatedBuilders(relName, arg, getRelated, getRelatedBuilder)
      match this.UseField relName, relatedBuilders with
      | false, None -> return Skip, []
      | false, Some builders -> return Skip, builders
      | true, None -> return ToMany.empty |> addLinks |> Include, []
      | true, Some builders ->
          let relationship =
            ToMany.empty
            |> ToMany.setData (builders |> List.map (fun b -> b.Identifier))
            |> addLinks
          return Include relationship, builders
    }

  /// Based on sparse fieldsets and includes, returns resource builders for the related
  /// resources and a relationship with correct linkage data as well as the specified
  /// links (an exception is thrown if links are specified but the resource builder
  /// does not supply a self link).
  member this.IncludeToMany
      ( relName: RelationshipName,
        arg: 'arg,
        getRelated: 'arg -> 'related list,
        getRelatedBuilder: ResourceBuildContext -> 'related -> ResourceBuilder<'relatedAttrs, 'relatedRels>,
        ?relatedLink: bool,
        ?selfLink: bool
      ) : ToMany Skippable * ResourceBuilder<obj, obj> list =
    this.IncludeToMany(
      relName, arg, getRelated >> async.Return, getRelatedBuilder,
      ?relatedLink = relatedLink,
      ?selfLink = selfLink)
    |> Async.RunSynchronously

  /// Based on sparse fieldsets and includes, returns resource builders for the related
  /// resources and a relationship with correct linkage data as well as the specified
  /// links (an exception is thrown if links are specified but the resource builder
  /// does not supply a self link).
  member this.IncludeToMany
      ( relName: RelationshipName,
        related: 'related list,
        getRelatedBuilder: ResourceBuildContext -> 'related -> ResourceBuilder<'relatedAttrs, 'relatedRels>,
        ?relatedLink: bool,
        ?selfLink: bool
      ) : ToMany Skippable * ResourceBuilder<obj, obj> list =
    this.IncludeToMany(
      relName, (), (fun () -> related), getRelatedBuilder,
      ?relatedLink = relatedLink,
      ?selfLink = selfLink)

  /// Based on sparse fieldsets and includes, returns the resource builder for the related
  /// resource and a relationship with correct linkage data as well as the specified
  /// links (an exception is thrown if links are specified but the resource builder
  /// does not supply a self link).
  member this.IncludeToOne
      ( relName: RelationshipName,
        arg: 'arg,
        getRelated: 'arg -> Async<'related option>,
        getRelatedBuilder: ResourceBuildContext -> 'related -> ResourceBuilder<'relatedAttrs, 'relatedRels>,
        ?relatedLink: bool,
        ?selfLink: bool
      ) : Async<ToOne Skippable * ResourceBuilder<obj, obj> option> =
    async {
      let addLinks =
        match this.SelfUrl with
        | None ->
            if relatedLink = Some true || selfLink = Some true then
              invalidOp (sprintf "Attempted to add link to relationship %s on type %s, but the builder for this type did not provide a self link" relName this.CurrentType)
            else id
        | Some url ->
            if relatedLink = Some true then ToOne.setRelatedLink url relName else id
            >> if selfLink = Some true then ToOne.setSelfLink url relName else id
      let! relatedBuilder =
        this.GetRelatedBuilders(relName, arg, getRelated >> Async.map Option.toList, getRelatedBuilder)
        |> Async.map (Option.map List.tryHead)
      match this.UseField relName, relatedBuilder with
      | false, None -> return Skip, None
      | false, Some builder -> return Skip, builder
      | true, None -> return ToOne.empty |> addLinks |> Include, None
      | true, Some builder ->
          let relationship =
            ToOne.empty
            |> ToOne.setData (builder |> Option.map (fun b -> b.Identifier))
            |> addLinks
          return Include relationship, builder
    }

  /// Based on sparse fieldsets and includes, returns the resource builder for the related
  /// resource and a relationship with correct linkage data as well as the specified
  /// links (an exception is thrown if links are specified but the resource builder
  /// does not supply a self link).
  member this.IncludeToOne
      ( relName: RelationshipName,
        arg: 'arg,
        getRelated: 'arg -> 'related option,
        getRelatedBuilder: ResourceBuildContext -> 'related -> ResourceBuilder<'relatedAttrs, 'relatedRels>,
        ?relatedLink: bool,
        ?selfLink: bool
      ) : ToOne Skippable * ResourceBuilder<obj, obj> option =
    this.IncludeToOne(
      relName, arg, getRelated >> async.Return, getRelatedBuilder,
      ?relatedLink = relatedLink,
      ?selfLink = selfLink)
    |> Async.RunSynchronously

  /// Based on sparse fieldsets and includes, returns the resource builder for the related
  /// resource and a relationship with correct linkage data as well as the specified
  /// links (an exception is thrown if links are specified but the resource builder
  /// does not supply a self link).
  member this.IncludeToOne
      ( relName: RelationshipName,
        related: 'related option,
        getRelatedBuilder: ResourceBuildContext -> 'related -> ResourceBuilder<'relatedAttrs, 'relatedRels>,
        ?relatedLink: bool,
        ?selfLink: bool
      ) : ToOne Skippable * ResourceBuilder<obj, obj> option =
    this.IncludeToOne(
      relName, (), (fun () -> related), getRelatedBuilder,
      ?relatedLink = relatedLink,
      ?selfLink = selfLink)

  /// Returns a link collection that contains the self link for this resource.
  /// Throws if the current resource builder does not provide a self link.
  member this.CreateSelfLink () : Links =
    match this.SelfUrl with
    | None -> invalidOp (sprintf "Attempted to create a self link for type %s, but the builder for this type did not provide a self link" this.CurrentType)
    | Some url -> Links.create Link.self url

  /// Returns a function that adds the specified link to a link collection,
  /// optionally only if the specified condition is true.
  /// The path segment appended to the self URL is the same as the link name.
  /// Throws if the current resource builder does not provide a self link.
  member this.AddLink(linkName, ?condition) : Links -> Links =
    match this.SelfUrl with
    | None -> invalidOp (sprintf "Attempted to add a link for type %s, but the builder for this type did not provide a self link" this.CurrentType)
    | Some url ->
        match condition with
        | None -> Links.add linkName (url.AddSegment linkName)
        | Some cond -> Links.addIf cond linkName (url.AddSegment linkName)


[<AutoOpen>]
module ResourceBuildContextExtensions =

  type ResourceBuildContext with

    /// Returns the specified value (wrapped in Include) if the attribute is
    /// not excluded using sparse fieldsets. Otherwise returns Skip.
    member this.GetAttribute (attrName: AttributeName, value: 'a) : Skippable<'a> =
      this.GetAttribute (attrName, Include value)

    /// Returns the result of the specified getter (wrapped in Include) if the
    /// attribute is not excluded using sparse fieldsets. Otherwise returns Skip.
    member this.GetAttribute (attrName: AttributeName, arg, getValue: 'arg -> 'a) : Skippable<'a> =
      this.GetAttribute (attrName, arg, getValue >> Include)

    /// Returns the specified value (wrapped in Include) if the attribute is
    /// not excluded using sparse fieldsets. Otherwise returns Skip.
    member this.GetAttribute (attrName: AttributeName, value: Async<'a>) : Async<Skippable<'a>> =
      this.GetAttribute (attrName, value |> Async.map Include)

    /// Returns the specified value (wrapped in Include) if the attribute is
    /// explicitly included using sparse fieldsets. Otherwise returns Skip.
    member this.GetExplicitAttribute (attrName: AttributeName, value: 'a) : Skippable<'a> =
      this.GetExplicitAttribute (attrName, Include value)

    /// Returns the result of the specified getter (wrapped in Include) if the
    /// attribute is explicitly included using sparse fieldsets. Otherwise returns Skip.
    member this.GetExplicitAttribute (attrName: AttributeName, arg, getValue: 'arg -> 'a) : Skippable<'a> =
      this.GetExplicitAttribute (attrName, arg, getValue >> Include)

    /// Returns the specified value (wrapped in Include) if the attribute is
    /// explicitly included using sparse fieldsets. Otherwise returns Skip.
    member this.GetExplicitAttribute (attrName: AttributeName, value: Async<'a>) : Async<Skippable<'a>> =
      this.GetExplicitAttribute (attrName, value |> Async.map Include)

    /// Based on sparse fieldsets and includes, returns the resource builder for the related
    /// resource and a relationship with correct linkage data as well as the specified
    /// links (an exception is thrown if links are specified but the resource builder
    /// does not supply a self link).
    member this.IncludeToOne
        ( relName: RelationshipName,
          arg: 'arg,
          getRelated: 'arg -> Async<'related>,
          getRelatedBuilder: ResourceBuildContext -> 'related -> ResourceBuilder<'relatedAttrs, 'relatedRels>,
          ?relatedLink: bool,
          ?selfLink: bool
        ) : Async<ToOne Skippable * ResourceBuilder<obj, obj> option> =
      this.IncludeToOne(
        relName, arg, getRelated >> Async.map Some, getRelatedBuilder,
        ?relatedLink = relatedLink,
        ?selfLink = selfLink)

    /// Based on sparse fieldsets and includes, returns the resource builder for the related
    /// resource and a relationship with correct linkage data as well as the specified
    /// links (an exception is thrown if links are specified but the resource builder
    /// does not supply a self link).
    member this.IncludeToOne
        ( relName: RelationshipName,
          arg: 'arg,
          getRelated: 'arg -> 'related,
          getRelatedBuilder: ResourceBuildContext -> 'related -> ResourceBuilder<'relatedAttrs, 'relatedRels>,
          ?relatedLink: bool,
          ?selfLink: bool
        ) : ToOne Skippable * ResourceBuilder<obj, obj> option =
      this.IncludeToOne(
        relName, arg, getRelated >> Some >> async.Return, getRelatedBuilder,
        ?relatedLink = relatedLink,
        ?selfLink = selfLink)
      |> Async.RunSynchronously
