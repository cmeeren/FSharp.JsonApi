namespace FSharp.JsonApi

open System
open System.Dynamic
open Newtonsoft.Json
open FSharp.JsonSkippable

/// Specifies the resource type name for the resource type represented by a
/// resource discriminator case.
[<AttributeUsage(AttributeTargets.Property)>]
type ResourceNameAttribute(resourceName: string) =
  inherit Attribute()
  member __.ResourceName = resourceName


/// Represents strict mode errors.
[<RequireQualifiedAccess>]
type StrictError =
  /// Sparse fieldsets contained an unknown type.
  | FieldsetTypeUnknown of typeName: string
  /// Sparse fieldsets contained an unknown field.
  | FieldsetFieldUnknown of typeName: string * fieldName: string
  /// An include path contained an unknown relationship.
  | UnknownIncludeRelationship of unknownRelName: string * fullPath: string


type internal ResourceInfo<'ResourceDiscriminator> =
  {
    TypeName: TypeName
    ResourceType: Type
    DiscriminatorTag: int
    DiscriminatorConstructor: Resource<obj, obj> -> 'ResourceDiscriminator
    ReadOnly: Set<FieldName>
    WriteOnly: Set<FieldName>
    NotNull: Set<FieldName>
    AllowedRelationshipTypes: Map<RelationshipName, Set<TypeName>>
    AttributeValidators: FieldValidator list
    RelationshipValidators: FieldValidator list
    AttributeNames: Set<AttributeName>
    RelationshipNames: Set<RelationshipName>
  }


type JsonApiContext<'ResourceDiscriminator> =
  internal
    { ResourceInfo: Map<TypeName, ResourceInfo<'ResourceDiscriminator>>
      SerializerSettings: JsonSerializerSettings
      RequestValidationContext: ValidationContext }

    member private this.HasTypeFor (resType: Type) =
      if not resType.IsGenericType
         || not (resType.GetGenericTypeDefinition() = typedefof<Resource<_,_>>)
      then invalidArg "resType" "resType must be a Resource<_,_> type"
      this.ResourceInfo
      |> Map.exists (fun _ i -> i.ResourceType = resType)

    member private this.VerifyRegisteredOrFail(resType: Type) =
      if not resType.IsGenericType
         || not (resType.GetGenericTypeDefinition() = typedefof<Resource<_,_>>)
      then invalidArg "resType" "resType must be a Resource<_,_> type"
      if this.HasTypeFor resType |> not then
        failwithf "JsonApiContext has no registration for resource type '%s'" resType.FullName

    member private this.TypeNamesFor (resType: Type) =
      if not resType.IsGenericType
          || not (resType.GetGenericTypeDefinition() = typedefof<Resource<_,_>>)
      then invalidArg "resType" "resType must be a Resource<_,_> type"
      this.ResourceInfo
      |> Map.toList
      |> List.filter (fun (_, i) -> i.ResourceType = resType)
      |> List.map fst

    /// Converts a weakly typed resource to a strongly typed resource wrapped in
    /// the resource discriminator. Returns None if there is no discriminator case
    /// for the resource type.
    member this.ToDiscriminator (res: Resource<obj, obj>) =
      this.ResourceInfo.TryFind res.Type
      |> Option.map (fun i -> i.DiscriminatorConstructor res)

    /// Converts a strongly typed resource wrapped in the resource discriminator
    /// to a weakly typed resource.
    member this.FromDiscriminator (discriminatedRes: 'ResourceDiscriminator) : Resource<obj, obj> =
      this.ResourceInfo
      |> Map.toList
      |> List.map snd
      |> List.pick (fun i ->
          if ReflectionHelpers.getUnionTagOfValue discriminatedRes = i.DiscriminatorTag
          then ReflectionHelpers.getUnionFields discriminatedRes |> snd |> Array.item 0 |> Resource.dynamicBox i.ResourceType |> Some
          else None
      )

    /// Specifies that the specified attribute or relationship on the specified
    /// type should or should not be read-only, overriding any ReadOnlyAttribute
    /// and previous read-only overrides for that field. Default is true
    /// (read-only).
    member this.WithReadOnly(typeName, fieldName, ?isReadOnly) =
      { this with
          RequestValidationContext =
            this.RequestValidationContext
            |> ValidationContext.overrideReadOnly typeName fieldName (defaultArg isReadOnly true) }

    /// Specifies that the specified attribute or relationship on the specified
    /// type should not be read-only, overriding any ReadOnlyAttribute and
    /// previous read-only overrides for that field. This is simply an alias for
    /// WithReadOnly(typeName, fieldName, false).
    member this.WithoutReadOnly(typeName, fieldName) =
      this.WithReadOnly(typeName, fieldName, false)

    /// Specifies that the specified attribute or relationship should or should
    /// not be read-only (regardless of the resource type), overriding any
    /// ReadOnlyAttribute and previous read-only overrides for that field.
    /// Default is true (read-only).
    member this.WithReadOnly(fieldName, ?isReadOnly) =
      this.ResourceInfo
      |> Map.toList
      |> List.map fst
      |> List.fold (fun (this: JsonApiContext<_>) typeName -> this.WithReadOnly(typeName, fieldName, ?isReadOnly = isReadOnly)) this

    /// Specifies that the specified attribute or relationship should not be
    /// read-only (regardless of the resource type), overriding any
    /// ReadOnlyAttribute and previous read-only overrides for that field. This
    /// is simply an alias for WithReadOnly(fieldName, false).
    member this.WithoutReadOnly(fieldName) =
      this.WithReadOnly(fieldName, false)

    /// Specifies that all attributes and relationships on the specified type
    /// should be read-only, overriding any ReadOnlyAttributes and previous
    /// read-only overrides.
    member this.WithAllReadOnly(typeName) =
      this.ResourceInfo.TryFind typeName
      |> Option.map (fun ri -> Set.union ri.AttributeNames ri.RelationshipNames)
      |> Option.defaultValue Set.empty
      |> Set.fold (fun (this: JsonApiContext<_>) fieldName -> this.WithReadOnly(typeName, fieldName, true)) this

    /// Specifies that the specified attribute or relationship on the specified
    /// type should or should not be write-only, overriding any
    /// WriteOnlyAttribute and previous write-only overrides for that field.
    /// Default is true (write-only).
    member this.WithWriteOnly(typeName, fieldName, ?isWriteOnly) =
      { this with
          RequestValidationContext =
            this.RequestValidationContext
            |> ValidationContext.overrideWriteOnly typeName fieldName (defaultArg isWriteOnly true) }

    /// Specifies that the specified attribute or relationship on the specified
    /// type should not be write-only, overriding any WriteOnlyAttribute and
    /// previous write-only overrides for that field. This is simply an alias
    /// for WithWriteOnly(typeName, fieldName, false).
    member this.WithoutWriteOnly(typeName, fieldName) =
      this.WithWriteOnly(typeName, fieldName, false)

    /// Specifies that the specified attribute or relationship should or should
    /// not be write-only (regardless of the resource type), overriding any
    /// WriteOnlyAttribute and previous write-only write-only overrides for that
    /// field. Default is true (write-only).
    member this.WithWriteOnly(fieldName, ?isWriteOnly) =
      this.ResourceInfo
      |> Map.toList
      |> List.map fst
      |> List.fold (fun (this: JsonApiContext<_>) typeName -> this.WithWriteOnly(typeName, fieldName, ?isWriteOnly = isWriteOnly)) this

    /// Specifies that the specified attribute or relationship should not be
    /// write-only (regardless of the resource type), overriding any
    /// WriteOnlyAttribute and previous write-only overrides for that field.
    /// This is simply an alias for WithWriteOnly(fieldName, false).
    member this.WithoutWriteOnly(fieldName) =
      this.WithWriteOnly(fieldName, false)

    /// Specifies that the specified relationship on the specified type may or
    /// may not be null, overriding any NotNullAttribute and previous
    /// nullability overrides for that relationship. Default is true (not null).
    member this.WithNotNull(typeName, relName, ?isNotNull) =
      { this with
          RequestValidationContext =
            this.RequestValidationContext
            |> ValidationContext.overrideNotNull typeName relName (defaultArg isNotNull true) }

    /// Specifies that the specified relationship on the specified type may be
    /// null, overriding any NotNullAttribute and previous nullability overrides
    /// for that relationship. This is simply an alias for WithNotNull(relName,
    /// relName, false).
    member this.WithNullable(typeName, relName) =
      this.WithNotNull(typeName, relName, false)

    /// Specifies that the specified relationship may or may not be null
    /// (regardless of the resource type), overriding any NotNullAttribute and
    /// previous nullability overrides for that relationship. Default is true
    /// (not null).
    member this.WithNotNull(relName, ?isNotNull) =
      this.ResourceInfo
      |> Map.toList
      |> List.map fst
      |> List.fold (fun (this: JsonApiContext<_>) typeName -> this.WithNotNull(typeName, relName, ?isNotNull = isNotNull)) this

    /// Specifies that the specified relationship may be null (regardless of the
    /// resource type), overriding any NotNullAttribute and previous nullability
    /// overrides for that relationship. This is simply an alias for
    /// WithNotNull(relName, false).
    member this.WithNullable(relName) =
      this.WithNotNull(relName, false)

    /// Specifies that the resource document's main resource may not contain a
    /// client-generated ID for this POST request.
    member this.WithNoIdForPost() =
      { this with
          RequestValidationContext =
            { this.RequestValidationContext with RequireNoIdForPost = true }
      }

    /// Specifies that the resource document's main resource ID must match the
    /// specified resource ID for this patch request.
    member this.WithIdForPatch(resourceId) =
      { this with
          RequestValidationContext =
            { this.RequestValidationContext with RequiredIdForPatch = Some resourceId }
      }

    /// Deserializes raw JSON to a single-resource document. Returns None for empty input.
    member this.DeserializeResourceDocument
        (json: string)
        : Result<ResourceDocument option, RequestDocumentError list> =
      try
        JsonConvert.DeserializeObject<ResourceDocument>(
          json, this.SerializerSettings)
        |> Option.ofObjBoxed
        |> Ok
      with ex ->
        RequestDocumentError.Malformed (ex, json) |> List.singleton |> Error

    /// Deserializes raw JSON to a resource collection document. Returns None for empty
    /// input.
    member this.DeserializeResourceCollectionDocument
        (json: string)
        : Result<ResourceCollectionDocument option, RequestDocumentError list> =
      try
        JsonConvert.DeserializeObject<ResourceCollectionDocument>(
          json, this.SerializerSettings)
        |> Option.ofObjBoxed
        |> Ok
      with ex ->
        RequestDocumentError.Malformed (ex, json) |> List.singleton |> Error

    /// Deserializes raw JSON to a resource identifier document. Returns None for empty
    /// input.
    member this.DeserializeResourceIdentifierDocument
        (json: string)
        : Result<ResourceIdentifierDocument option, RequestDocumentError list> =
      try
        JsonConvert.DeserializeObject<ResourceIdentifierDocument>(
          json, this.SerializerSettings)
        |> Option.ofObjBoxed
        |> Ok
      with ex ->
        RequestDocumentError.Malformed (ex, json) |> List.singleton |> Error

    /// Deserializes raw JSON to a resource identifier collection document. Returns None for
    /// empty input.
    member this.DeserializeResourceIdentifierCollectionDocument
        (json: string)
        : Result<ResourceIdentifierCollectionDocument option, RequestDocumentError list> =
      try
        JsonConvert.DeserializeObject<ResourceIdentifierCollectionDocument>(
          json, this.SerializerSettings)
        |> Option.ofObjBoxed
        |> Ok
      with ex ->
        RequestDocumentError.Malformed (ex, json) |> List.singleton |> Error

    /// Deserializes raw JSON to an error document. Returns None for empty input.
    member this.DeserializeErrorDocument
        (json: string)
        : Result<ErrorDocument option, RequestDocumentError list> =
      try
        JsonConvert.DeserializeObject<ErrorDocument>(
          json, this.SerializerSettings)
        |> Option.ofObjBoxed
        |> Ok
      with ex ->
        RequestDocumentError.Malformed (ex, json) |> List.singleton |> Error

    /// Validates a request document. Passes through the document if validation succeeds, or
    /// returns a list of errors.
    member this.ValidateRequest
        (doc: #IJsonApiDocument)
        : Result<#IJsonApiDocument, RequestDocumentError list> =
      match Validate.document this.RequestValidationContext doc with
      | [] -> Ok doc
      | errs -> errs |> List.choose RequestDocumentError.OfDocumentError |> Error

    /// Validates a response document. Passes through the document if validation succeeds,
    /// or returns a list of errors.
    member this.ValidateResponse
        (doc: #IJsonApiDocument)
        : Result<#IJsonApiDocument, ResponseDocumentError list> =
      let validationCtx =
        { this.RequestValidationContext with
            ValidationType = Response }
      match Validate.document validationCtx doc with
      | [] -> Ok doc
      | errs -> errs |> List.choose ResponseDocumentError.OfDocumentError |> Error

    /// Performs strict validation. Currently it verifies fieldsets and include paths in a
    /// request. This should not be done in production environments since it might
    /// conflict with the must-ignore clause of the JSON-API specification, but it can be
    /// useful in testing environments to catch typos in these query parameters.
    ///
    /// mainDataTypes are the resource type names that can be returned as main data, and
    /// is needed to validate include paths. If it is empty, include paths will not be
    /// validated.
    member this.ValidateStrict
        ( fields: Fieldsets,
          includes: IncludePath list,
          [<ParamArray>] mainDataTypes: TypeName []
        ) : Result<unit, StrictError list> =
      let fieldsetErrs =
        fields
        |> Map.toList
        |> List.collect (fun (typeName, fieldsetFields) ->
            match this.ResourceInfo.TryFind typeName with
            | None -> [StrictError.FieldsetTypeUnknown typeName]
            | Some info ->
                let allFields = Set.union info.RelationshipNames info.AttributeNames
                let unknownFields = Set.difference fieldsetFields allFields
                unknownFields |> Set.toList |> List.map (fun f -> StrictError.FieldsetFieldUnknown(typeName, f))
        )

      let rec getIncludePathError (currentTypes: TypeName list) (pathSoFar: IncludePath) (remainingPath: IncludePath) : StrictError option =
        match remainingPath with
        | [] -> None
        | currentRel :: tail ->
            let infosForCurrentTypes = currentTypes |> List.choose this.ResourceInfo.TryFind
            let allowedTypesForRel =
              infosForCurrentTypes
              |> List.collect (fun info ->
                  match info.AllowedRelationshipTypes.TryFind currentRel with
                  | None -> []
                  | Some ts -> Set.toList ts
              )
            match allowedTypesForRel with
            | [] ->
                let fullPath = String.concat "." (pathSoFar @ [currentRel])
                StrictError.UnknownIncludeRelationship (currentRel, fullPath) |> Some
            | ts -> getIncludePathError ts (pathSoFar @ [currentRel]) tail

      let includeErrs =
        if mainDataTypes.Length = 0 then []
        else
          includes |> List.choose (fun path ->
            getIncludePathError (Array.toList mainDataTypes) [] path
          )

      let allErrs = fieldsetErrs @ includeErrs
      match allErrs with
      | [] -> Ok ()
      | _ -> Error allErrs

    /// Performs strict validation. Currently it verifies fieldsets and include paths in a
    /// request. This should not be done in production environments since it might
    /// conflict with the must-ignore clause of the JSON-API specification, but it can be
    /// useful in testing environments to catch typos in these query parameters.
    ///
    /// mainDataTypeDiscriminatorCase indicates the resource type that can be returned as
    /// main data.
    member this.ValidateStrict
        ( fields: Fieldsets,
          includes: IncludePath list,
          mainDataTypeDiscriminatorCase: Resource<'attrs, 'rels> -> 'ResourceDiscriminator
        ) : Result<unit, StrictError list> =
      let mainTypeNames = this.TypeNamesFor typeof<Resource<'attrs, 'rels>> |> List.toArray
      this.ValidateStrict(fields, includes, mainTypeNames)

    /// Performs strict validation. Currently it verifies fieldsets and include paths in a
    /// request. This should not be done in production environments since it might
    /// conflict with the must-ignore clause of the JSON-API specification, but it can be
    /// useful in testing environments to catch typos in these query parameters.
    ///
    /// the mainDataTypeDiscriminatorCase arguments indicate the resource types that can
    /// be returned as main data.
    member this.ValidateStrict
        ( fields: Fieldsets,
          includes: IncludePath list,
          mainDataTypeDiscriminatorCase1: Resource<'attrs1, 'rels1> -> 'ResourceDiscriminator,
          mainDataTypeDiscriminatorCase2: Resource<'attrs2, 'rels2> -> 'ResourceDiscriminator
        ) : Result<unit, StrictError list> =
      let mainTypeNames =
        this.TypeNamesFor typeof<Resource<'attrs1, 'rels1>>
        @ this.TypeNamesFor typeof<Resource<'attrs2, 'rels2>>
        |> List.toArray
      this.ValidateStrict(fields, includes, mainTypeNames)

    /// Performs strict validation. Currently it verifies fieldsets and include paths in a
    /// request. This should not be done in production environments since it might
    /// conflict with the must-ignore clause of the JSON-API specification, but it can be
    /// useful in testing environments to catch typos in these query parameters.
    ///
    /// the mainDataTypeDiscriminatorCase arguments indicate the resource types that can
    /// be returned as main data.
    member this.ValidateStrict
        ( fields: Fieldsets,
          includes: IncludePath list,
          mainDataTypeDiscriminatorCase1: Resource<'attrs1, 'rels1> -> 'ResourceDiscriminator,
          mainDataTypeDiscriminatorCase2: Resource<'attrs2, 'rels2> -> 'ResourceDiscriminator,
          mainDataTypeDiscriminatorCase3: Resource<'attrs3, 'rels3> -> 'ResourceDiscriminator
        ) : Result<unit, StrictError list> =
      let mainTypeNames =
        this.TypeNamesFor typeof<Resource<'attrs1, 'rels1>>
        @ this.TypeNamesFor typeof<Resource<'attrs2, 'rels2>>
        @ this.TypeNamesFor typeof<Resource<'attrs3, 'rels3>>
        |> List.toArray
      this.ValidateStrict(fields, includes, mainTypeNames)

    /// Gets the main data resource from a resource document, typed as a
    /// resource discriminator. Returns Ok None if there is no resource, and
    /// errors if the resource type is unknown.
    member this.GetResource(doc: ResourceDocument) : Result<'ResourceDiscriminator option, RequestDocumentError list> =
      match doc.Data with
      | None -> Ok None
      | Some d ->
          match this.ToDiscriminator d with
          | None -> Error [RequestDocumentError.UnknownMainResourceType ("/data/type", d.Type)]
          | Some x -> Ok (Some x)

    /// Gets the main data resource from a resource document, typed as a
    /// resource discriminator. Returns errors if there is no resource or if the
    /// resource type is unknown.
    member this.RequireResource(doc: ResourceDocument) : Result<'ResourceDiscriminator, RequestDocumentError list> =
      this.GetResource(doc)
      |> Result.bind (Result.requireSome [RequestDocumentError.RequiredFieldMissing "/data/type"])

    /// Gets a main data resource of the specified type from the resource document. Returns
    /// errors if the type doesn't match.
    member this.GetResource
        ( discriminatorCase: Resource<'attrs, 'rels> -> 'ResourceDiscriminator,
          doc: ResourceDocument
        ) : Result<Resource<'attrs, 'rels> option, RequestDocumentError list> =
      let resType = typeof<Resource<'attrs, 'rels>>
      this.VerifyRegisteredOrFail(resType)
      match doc.Data with
      | None -> Ok None
      | Some res ->
          match res |> Resource.tryUnbox with
          | None ->
              let pointer = "/data/type"
              let expectedTypeNames = this.TypeNamesFor resType
              RequestDocumentError.UnexpectedMainResourceType (pointer, res.Type, expectedTypeNames)
              |> List.singleton
              |> Error
          | Some r -> Some r |> Ok

    /// Gets a main data resource of one of the specified types from the resource document.
    /// Returns errors if the type doesn't match.
    member this.GetResource
        ( discriminatorCase1: Resource<'attrs1, 'rels1> -> 'ResourceDiscriminator,
          discriminatorCase2: Resource<'attrs2, 'rels2> -> 'ResourceDiscriminator,
          doc: ResourceDocument
        ) : Result<Choice<Resource<'attrs1, 'rels1>, Resource<'attrs2, 'rels2>> option, RequestDocumentError list> =
      let resType1 = typeof<Resource<'attrs1, 'rels1>>
      let resType2 = typeof<Resource<'attrs2, 'rels2>>
      this.VerifyRegisteredOrFail(resType1)
      this.VerifyRegisteredOrFail(resType2)
      match doc.Data with
      | None -> Ok None
      | Some res ->
          let result =
            res |> Resource.tryUnbox |> Option.map Choice1Of2
            |> Option.orElseWith (fun () -> res |> Resource.tryUnbox |> Option.map Choice2Of2)
          match result with
          | None ->
              let pointer = "/data/type"
              let expectedTypeNames =
                this.TypeNamesFor resType1
                @ this.TypeNamesFor resType2
              RequestDocumentError.UnexpectedMainResourceType (pointer, res.Type, expectedTypeNames)
              |> List.singleton
              |> Error
          | Some r -> Some r |> Ok

    /// Gets a main data resource of one of the specified types from the resource
    /// document. Returns errors if the type doesn't match.
    member this.GetResource
        ( discriminatorCase1: Resource<'attrs1, 'rels1> -> 'ResourceDiscriminator,
          discriminatorCase2: Resource<'attrs2, 'rels2> -> 'ResourceDiscriminator,
          discriminatorCase3: Resource<'attrs3, 'rels3> -> 'ResourceDiscriminator,
          doc: ResourceDocument
        ) : Result<Choice<Resource<'attrs1, 'rels1>, Resource<'attrs2, 'rels2>, Resource<'attrs3, 'rels3>> option, RequestDocumentError list> =
      let resType1 = typeof<Resource<'attrs1, 'rels1>>
      let resType2 = typeof<Resource<'attrs2, 'rels2>>
      let resType3 = typeof<Resource<'attrs3, 'rels3>>
      this.VerifyRegisteredOrFail(resType1)
      this.VerifyRegisteredOrFail(resType2)
      this.VerifyRegisteredOrFail(resType3)
      match doc.Data with
      | None -> Ok None
      | Some res ->
          let result =
            res |> Resource.tryUnbox |> Option.map Choice1Of3
            |> Option.orElseWith (fun () -> res |> Resource.tryUnbox |> Option.map Choice2Of3)
            |> Option.orElseWith (fun () -> res |> Resource.tryUnbox |> Option.map Choice3Of3)
          match result with
          | None ->
              let pointer = "/data/type"
              let expectedTypeNames =
                this.TypeNamesFor resType1
                @ this.TypeNamesFor resType2
                @ this.TypeNamesFor resType3
              RequestDocumentError.UnexpectedMainResourceType (pointer, res.Type, expectedTypeNames)
              |> List.singleton
              |> Error
          | Some r -> Some r |> Ok

    /// Gets all main data resources from a resource collection document, typed as resource
    /// discriminators. Ignores resources with unknown types.
    member this.GetResources(doc: ResourceCollectionDocument) =
      doc.Data |> List.choose this.ToDiscriminator

    /// Gets all main data resources from a resource collection document. Returns errors if
    /// any resource does not match the specified type.
    member this.GetResources
        ( discriminatorCase: Resource<'attrs, 'rels> -> 'ResourceDiscriminator,
          doc: ResourceCollectionDocument
        ) : Result<Resource<'attrs, 'rels> list, RequestDocumentError list> =
      let resType = typeof<Resource<'attrs, 'rels>>
      this.VerifyRegisteredOrFail(resType)
      doc.Data
      |> List.mapi (fun i res ->
          match Resource.tryUnbox res with
          | None ->
              let pointer = sprintf "/data/%i/type" i
              let expectedTypeNames = this.TypeNamesFor resType
              RequestDocumentError.UnexpectedMainResourceType (pointer, res.Type, expectedTypeNames)
              |> List.singleton
              |> Error
          | Some r -> Ok [r]
      )
      |> List.fold Result.combine (Ok [])

    /// Gets all main data resources of the specified type from a resource collection
    /// document. Ignores resources with non-matching types.
    member this.GetMatchingResources
        ( discriminatorCase: Resource<'attrs, 'rels> -> 'ResourceDiscriminator,
          doc: ResourceCollectionDocument
        ) : Resource<'attrs, 'rels> list =
      this.VerifyRegisteredOrFail(typeof<Resource<'attrs, 'rels>>)
      doc.Data |> List.choose Resource.tryUnbox

    /// Gets all included resources from a compound document, typed as resource
    /// discriminators. Ignores resources with unknown types.
    member this.GetIncluded(doc: #ICompoundDocument) : 'ResourceDiscriminator list =
      doc.Included
      |> Skippable.defaultValue []
      |> List.choose this.ToDiscriminator

    /// Gets all unknown main data resources from a resource collection document.
    member __.GetUnknownResources
        (doc: ResourceCollectionDocument)
        : Resource<ExpandoObject, ExpandoObject> list =
      doc.Data |> List.choose Resource.tryUnbox

    /// Gets all included resources from a compound document that do not match any known
    /// type.
    member __.GetUnknownIncluded
        (doc: #ICompoundDocument)
        : Resource<ExpandoObject, ExpandoObject> list =
      doc.Included
      |> Skippable.defaultValue []
      |> List.choose Resource.tryUnbox

    /// Deserializes a single-resource request document, validates it (unless
    /// validate is false), and extracts the main data resource as a resource
    /// discriminator. Returns errors if the resource type is unknown.
    member this.Parse
        ( json: string,
          ?validate: bool
        ) : Result<'ResourceDiscriminator option, RequestDocumentError list> =
      this.DeserializeResourceDocument(json)
      |> ResultOption.bindResult (if validate = Some false then Ok else this.ValidateRequest)
      |> ResultOption.bind (fun d -> this.GetResource(d))

    /// Deserializes a single-resource request document, validates it (unless
    /// validate is false), and extracts a resource of the specified type.
    /// Returns errors if the type doesn't match.
    member this.Parse
        ( discriminatorCase: Resource<'attrs, 'rels> -> 'ResourceDiscriminator,
          json: string,
          ?validate: bool
        ) : Result<Resource<'attrs, 'rels> option, RequestDocumentError list> =
      this.DeserializeResourceDocument(json)
      |> ResultOption.bindResult (if validate = Some false then Ok else this.ValidateRequest)
      |> ResultOption.bind (fun d -> this.GetResource(discriminatorCase, d))

    /// Deserializes a single-resource request document, validates it (unless
    /// validate is false), and extracts a resource of one of the specified
    /// types. Returns errors if the type doesn't match.
    member this.Parse
        ( discriminatorCase1: Resource<'attrs1, 'rels1> -> 'ResourceDiscriminator,
          discriminatorCase2: Resource<'attrs2, 'rels2> -> 'ResourceDiscriminator,
          json: string,
          ?validate: bool
        ) : Result<Choice<Resource<'attrs1, 'rels1>,
                          Resource<'attrs2, 'rels2>> option,
                   RequestDocumentError list> =
      this.DeserializeResourceDocument(json)
      |> ResultOption.bindResult (if validate = Some false then Ok else this.ValidateRequest)
      |> ResultOption.bind (fun d -> this.GetResource(discriminatorCase1, discriminatorCase2, d))

    /// Deserializes a single-resource request document, validates it (unless
    /// validate is false), and extracts a resource of one of the specified
    /// types. Returns errors if the type doesn't match.
    member this.Parse
          ( discriminatorCase1: Resource<'attrs1, 'rels1> -> 'ResourceDiscriminator,
            discriminatorCase2: Resource<'attrs2, 'rels2> -> 'ResourceDiscriminator,
            discriminatorCase3: Resource<'attrs3, 'rels3> -> 'ResourceDiscriminator,
            json: string,
            ?validate: bool
          ) : Result<Choice<Resource<'attrs1, 'rels1>,
                            Resource<'attrs2, 'rels2>,
                            Resource<'attrs3, 'rels3>> option,
                     RequestDocumentError list> =
      this.DeserializeResourceDocument(json)
      |> ResultOption.bindResult (if validate = Some false then Ok else this.ValidateRequest)
      |> ResultOption.bind (fun d -> this.GetResource(discriminatorCase1, discriminatorCase2, discriminatorCase3, d))

    /// Deserializes a single-resource request document, validates it (unless
    /// validate is false), and extracts the main data resource as a resource
    /// discriminator. Returns errors if the resource type is unknown or if
    /// there is no resource.
    member this.ParseRequired
        ( json: string,
          ?validate: bool
        ) : Result<'ResourceDiscriminator, RequestDocumentError list> =
      this.DeserializeResourceDocument(json)
      |> Result.bind (Result.requireSome [RequestDocumentError.RequiredFieldMissing "/data/type"])
      |> Result.bind (if validate = Some false then Ok else this.ValidateRequest)
      |> Result.bind this.RequireResource

    /// Deserializes a single-resource request document, validates it (unless
    /// validate is false), and extracts a resource of the specified type.
    /// Returns errors if the type doesn't match or if there is no resource.
    member this.ParseRequired
        ( discriminatorCase: Resource<'attrs, 'rels> -> 'ResourceDiscriminator,
          json: string,
          ?validate: bool
        ) : Result<Resource<'attrs, 'rels>, RequestDocumentError list> =
      this.Parse(discriminatorCase, json, ?validate = validate)
      |> Result.bind (Result.requireSome [RequestDocumentError.RequiredFieldMissing "/data/type"])

    /// Deserializes a single-resource request document, validates it (unless
    /// validate is false), and extracts a resource of one of the specified
    /// types. Returns errors if the type doesn't match or if there is no
    /// resource.
    member this.ParseRequired
        ( discriminatorCase1: Resource<'attrs1, 'rels1> -> 'ResourceDiscriminator,
          discriminatorCase2: Resource<'attrs2, 'rels2> -> 'ResourceDiscriminator,
          json: string,
          ?validate: bool
        ) : Result<Choice<Resource<'attrs1, 'rels1>,
                          Resource<'attrs2, 'rels2>>,
                   RequestDocumentError list> =
      this.Parse(discriminatorCase1, discriminatorCase2, json, ?validate = validate)
      |> Result.bind (Result.requireSome [RequestDocumentError.RequiredFieldMissing "/data/type"])

    /// Deserializes a single-resource request document, validates it (unless
    /// validate is false), and extracts a resource of one of the specified
    /// types. Returns errors if the type doesn't match or if there is no
    /// resource.
    member this.ParseRequired
          ( discriminatorCase1: Resource<'attrs1, 'rels1> -> 'ResourceDiscriminator,
            discriminatorCase2: Resource<'attrs2, 'rels2> -> 'ResourceDiscriminator,
            discriminatorCase3: Resource<'attrs3, 'rels3> -> 'ResourceDiscriminator,
            json: string,
            ?validate: bool
          ) : Result<Choice<Resource<'attrs1, 'rels1>,
                            Resource<'attrs2, 'rels2>,
                            Resource<'attrs3, 'rels3>>,
                     RequestDocumentError list> =
      this.Parse(discriminatorCase1, discriminatorCase2, discriminatorCase3, json, ?validate = validate)
      |> Result.bind (Result.requireSome [RequestDocumentError.RequiredFieldMissing "/data/type"])

    /// Deserializes a resource collection request document, validates it
    /// (unless validate is false), and extracts the main data resources as
    /// resource discriminators. Ignores resources with unknown types.
    member this.ParseCollection
        ( json: string,
          ?validate: bool
        ) : Result<'ResourceDiscriminator list, RequestDocumentError list> =
      this.DeserializeResourceCollectionDocument(json)
      |> ResultOption.bindResult (if validate = Some false then Ok else this.ValidateRequest)
      |> ResultOption.map (fun d -> this.GetResources(d))
      |> Result.map (Option.defaultValue [])


    /// Deserializes a resource collection request document, validates it
    /// (unless validate is false), and extracts all main data resources. If
    /// ignoreUnknown is true, ignores resources with non-matching types,
    /// otherwise returns errors if any resource does not match the specified
    /// type.
    member this.ParseCollection
        ( discriminatorCase: Resource<'attrs, 'rels> -> 'ResourceDiscriminator,
          json: string,
          ?ignoreUnknown: bool,
          ?validate: bool
        ) : Result<Resource<'attrs, 'rels> list, RequestDocumentError list> =
      let extract d =
        match ignoreUnknown with
        | None | Some false -> this.GetResources(discriminatorCase, d)
        | Some true -> this.GetMatchingResources(discriminatorCase, d) |> Ok
      this.DeserializeResourceCollectionDocument(json)
      |> ResultOption.bindResult (if validate = Some false then Ok else this.ValidateRequest)
      |> ResultOption.bindResult extract
      |> Result.map (Option.defaultValue [])

  /// Serializes a JSON-API document.
  member this.Serialize(doc: #IJsonApiDocument) =
    JsonConvert.SerializeObject(doc, this.SerializerSettings)

  /// Serializes a JSON-API document and converts it to a byte array to be
  /// written to a body.
  member this.SerializeAndGetBytes(doc: #IJsonApiDocument) =
    JsonConvert.SerializeObject(doc, this.SerializerSettings)
    |> System.Text.Encoding.UTF8.GetBytes

  /// Serializes an arbitrary type using the context's serialization settings.
  /// This is outside normal usage; for serializing documents, use Serialize.
  member this.SerializeAny(x: 'a) =
    JsonConvert.SerializeObject(x, this.SerializerSettings)

  /// Returns a single-resource document for the specified resource.
  member __.BuildDocument
      ( entity: 'entity option,
        getBuilder: ResourceBuildContext -> 'entity -> ResourceBuilder<'attrs, 'rels>,
        ?fieldsets: Fieldsets,
        ?includePaths: IncludePath list)
      : Async<ResourceDocument>
      =
    async {
      match entity with
      | None -> return ResourceDocument.noResource
      | Some e ->
          let trialBuildCtx =
            { Fields = defaultArg fieldsets Map.empty
              Includes = defaultArg includePaths []
              CurrentType = ""
              SelfUrl = None }
          let trialBuilder = getBuilder trialBuildCtx e
          let finalBuildCtx =
            { trialBuildCtx with
                CurrentType = trialBuilder.Identifier.Type
                SelfUrl = trialBuilder.SelfUrl }
          let builder = getBuilder finalBuildCtx e
          let! main, included = ResourceBuilder.buildOne builder
          return ResourceDocument.ofResourceAndIncluded (Some main) included
    }

  /// Returns a resource collection document for the specified resources.
  member __.BuildDocument
      ( entities: 'entity list,
        getBuilder: ResourceBuildContext -> 'entity -> ResourceBuilder<'attrs, 'rels>,
        ?fieldsets: Fieldsets,
        ?includePaths: IncludePath list)
      : Async<ResourceCollectionDocument>
      =
    async {
      let builders =
        entities
        |> List.map (fun e ->
            let trialBuildCtx =
              { Fields = defaultArg fieldsets Map.empty
                Includes = defaultArg includePaths []
                CurrentType = ""
                SelfUrl = None }
            let trialBuilder = getBuilder trialBuildCtx e
            let finalBuildCtx =
              { trialBuildCtx with
                  CurrentType = trialBuilder.Identifier.Type
                  SelfUrl = trialBuilder.SelfUrl }
            getBuilder finalBuildCtx e
        )
      let! main, included = ResourceBuilder.build builders
      return ResourceCollectionDocument.ofResourcesAndIncluded main included
    }


[<AutoOpen>]
module JsonApiContextExtensions =

  type JsonApiContext<'ResourceDiscriminator> with

    /// Returns a single-resource document for the specified resource.
    member this.BuildDocument
        ( entity: 'entity,
          getBuilder: ResourceBuildContext -> 'entity -> ResourceBuilder<'attrs, 'rels>,
          ?fieldsets: Fieldsets,
          ?includePaths: IncludePath list)
        : Async<ResourceDocument>
        =
      this.BuildDocument(
        Some entity, getBuilder,
        fieldsets = defaultArg fieldsets Map.empty,
        includePaths = defaultArg includePaths [])


module private RelationshipValidationHelpers =

  let validateToOne (ctx: ValidationContext) (rel: ToOne) =
    [ if ctx.ValidationType = Request && ctx.CurrentFieldIsReadOnly then yield DocumentError.FieldReadOnly (ctx.Pointer, ctx.CurrentFieldReadOnlyIsOverride)
      if ctx.ValidationType = Response && ctx.CurrentFieldIsWriteOnly then yield DocumentError.FieldWriteOnly (ctx.Pointer, ctx.CurrentFieldWriteOnlyIsOverride)
      yield! Validate.toOne ctx rel ]

  let validateToMany (ctx: ValidationContext) (rel: ToMany) =
    [ if ctx.ValidationType = Request && ctx.CurrentFieldIsReadOnly then yield DocumentError.FieldReadOnly (ctx.Pointer, ctx.CurrentFieldReadOnlyIsOverride)
      if ctx.ValidationType = Response && ctx.CurrentFieldIsWriteOnly then yield DocumentError.FieldWriteOnly (ctx.Pointer, ctx.CurrentFieldWriteOnlyIsOverride)
      yield! Validate.toMany ctx rel ]

  let getRelMemberValidationErrors
      (getValue: 'rels -> obj)
      ctx
      (rels: 'rels) =
    match getValue rels with
    | :? Skippable<ToOne> as sr ->
        sr
        |> Skippable.map (validateToOne ctx)
        |> Skippable.defaultValue []
    | :? Skippable<ToMany> as sr ->
        sr
        |> Skippable.map (validateToMany ctx)
        |> Skippable.defaultValue []
    | _ -> []


module private AttributeValidationHelpers =

  /// Returns true if the value is null (not including Option.None).
  let isInvalidNull (t: Type) (value: obj) =
    let isOption = t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<Option<_>>
    not isOption && isNull value

  let getAttrMemberValidationErrors
      (innerType: Type)
      (getBoxed: ('attrs -> obj Skippable))
      (ctx: ValidationContext)
      (attrs: 'attrs) =
    match getBoxed attrs with
    | Skip -> []
    | Include innerValue ->
        [ if ctx.ValidationType = Request && ctx.CurrentFieldIsReadOnly then yield DocumentError.FieldReadOnly (ctx.Pointer, ctx.CurrentFieldReadOnlyIsOverride)
          if ctx.ValidationType = Response && ctx.CurrentFieldIsWriteOnly then yield DocumentError.FieldWriteOnly (ctx.Pointer, ctx.CurrentFieldWriteOnlyIsOverride)
          if isInvalidNull innerType innerValue then yield DocumentError.InvalidNull (ctx.Pointer, ctx.CurrentFieldNotNullIsOverride) ]



module JsonApiContext =

  open System.Reflection
  open FSharp.Reflection

  open ReflectionHelpers
  open AttributeValidationHelpers
  open RelationshipValidationHelpers


  /// Creates a JSON-API context using the specified function (and inferred
  /// resource discriminator type) to inspect all resources and get the resource
  /// names.
  let create<'ResourceDiscriminator> =
    let resourceInfos =
      FSharpType.GetUnionCases typeof<'ResourceDiscriminator>
      |> Array.map (fun ci ->
          let fields = ci.GetFields()
          let field = fields |> Array.exactlyOne
          if field.PropertyType.GetGenericTypeDefinition() <> typedefof<Resource<_,_>> then
            failwithf "All cases of the resource discriminator must have a single field of type Resource<_,_>, but case '%s' has type %s" ci.Name field.PropertyType.FullName

          let typeName =
            match ci.GetCustomAttributes(typeof<ResourceNameAttribute>) with
            | [|attr|] -> (attr :?> ResourceNameAttribute).ResourceName
            | attrs -> failwithf "Resource discriminator case '%s' has %i instances of ResourceNameAttribute, expected 1" ci.Name attrs.Length

          let genericArgs = field.PropertyType.GetGenericArguments()
          let attrType = genericArgs.[0]
          let relType = genericArgs.[1]

          if attrType.GetConstructor(Type.EmptyTypes) |> isNull then
            failwithf "Attribute type '%s' for resource '%s' lacks default constructor (consider using CLIMutableAttribute)" attrType.Name typeName

          if relType.GetConstructor(Type.EmptyTypes) |> isNull then
            failwithf "Relationship type '%s' for resource '%s' lacks default constructor (consider using CLIMutableAttribute)" relType.Name typeName

          let attrProps = attrType.GetProperties()
          let relProps = relType.GetProperties()

          let isValidFieldName s =
            MemberName.isValid s
            && s <> "id"
            && s <> "type"

          let ignoreInvalidFieldName (pi: PropertyInfo) =
            pi.GetCustomAttributes(typeof<AllowIllegalNameAttribute>)
            |> Seq.isEmpty
            |> not

          for pi in attrProps do
            if not (isValidFieldName pi.Name || ignoreInvalidFieldName pi) then
              failwithf "Attribute type '%s' for resource '%s' has property '%s', which is not a valid JSON-API attribute name. Use another name or add AllowIllegalNameAttribute to this property to skip this check." attrType.Name typeName pi.Name

          for pi in relProps do
            if not (isValidFieldName pi.Name || ignoreInvalidFieldName pi) then
              failwithf "Relationship type '%s' for resource '%s' has property '%s', which is not a valid JSON-API relationship name. Use another name or add AllowIllegalNameAttribute to this property to skip this check." relType.Name typeName pi.Name

          for pi in attrProps do
            if isNull pi.SetMethod then
              failwithf "Attribute type '%s' for resource '%s' has non-settable property '%s' (consider using CLIMutableAttribute)" attrType.Name typeName pi.Name

          for pi in relProps do
            if isNull pi.SetMethod then
              failwithf "Relationship type '%s' for resource '%s' has non-settable property '%s' (consider using CLIMutableAttribute)" relType.Name typeName pi.Name

          for pi in attrProps do
            if not pi.PropertyType.IsGenericType || pi.PropertyType.GetGenericTypeDefinition() <> typedefof<Skippable<_>> then
              failwithf "All attributes must be of type Skippable<_>, but attribute '%s' on resource '%s' has type %s" pi.Name typeName pi.PropertyType.FullName

          for pi in relProps do
            if not (pi.PropertyType = typeof<Skippable<ToOne>>) && not (pi.PropertyType = typeof<Skippable<ToMany>>) then
              failwithf "All relationships must be of type Skippable<ToOne> or Skippable<ToMany>, but relationship '%s' on resource '%s' has type %s" pi.Name typeName pi.PropertyType.FullName

          let attrNames = attrProps |> Array.map (fun pi -> pi.Name) |> Set.ofArray
          let relNames = relProps |> Array.map (fun pi -> pi.Name) |> Set.ofArray
          let nameCollisions = Set.intersect attrNames relNames
          if not nameCollisions.IsEmpty then
            failwithf "Field names must be unique between attributes and fields, but the resource '%s' uses the following names for both attributes and fields: %A" typeName (Set.toList nameCollisions)

          let getFieldsWithAttr attribType =
            [attrProps; relProps]
            |> Array.concat
            |> Array.filter (fun pi -> pi.GetCustomAttributes(attribType, true) |> Array.isEmpty |> not)
            |> Array.map (fun pi -> pi.Name)
            |> Set.ofArray

          let fieldsWithNotNullAttr = getFieldsWithAttr typeof<NotNullAttribute>
          let attrsWithNotNullAttr = Set.intersect fieldsWithNotNullAttr attrNames
          if not attrsWithNotNullAttr.IsEmpty then
            failwithf "NotNullAttribute can only be used on relationships, but was used on the following attributes of type %s (use the option type to signify nullability of properties): %A" typeName (Set.toList attrsWithNotNullAttr)

          let fieldsWithAllowedTypeAttr = getFieldsWithAttr typeof<AllowedTypesAttribute>
          let attrsWithAllowedTypeAttr = Set.intersect fieldsWithAllowedTypeAttr attrNames
          if not attrsWithAllowedTypeAttr.IsEmpty then
            failwithf "AllowedTypesAttribute can only be used on relationships, but was used on the following attributes of type %s: %A" typeName (Set.toList attrsWithAllowedTypeAttr)

          let relsAndAllowedTypes =
            relProps
            |> Array.map (fun pi ->
                let allowedTypes =
                  pi.GetCustomAttributes(typeof<AllowedTypesAttribute>, true)
                  |> Array.collect (fun a -> (a :?> AllowedTypesAttribute).TypeNames)
                  |> Set.ofArray
                pi.Name, allowedTypes
            )
            |> Map.ofArray

          let resourceType = typedefof<Resource<_,_>>.MakeGenericType(attrType, relType)
          let discriminatorConstructor (res: Resource<obj, obj>) =
            FSharpValue.MakeUnion(ci, [| Resource.dynamicUnbox resourceType res |]) :?> 'ResourceDiscriminator

          let attrValidators =
            attrProps
            |> Array.map (fun pi ->
                let cases = getUnionCases pi.PropertyType
                let skip, incl = cases.[0], cases.[1]
                let innerType = (getUnionCaseProperyInfoFields incl).[0].PropertyType
                let fastGetter = buildUntypedGetter pi
                let getBoxed attrs =
                  let value = fastGetter attrs
                  let case, fields = getUnionFields value
                  if case = skip then Skip else Include fields.[0]
                fun ctx rels ->
                  getAttrMemberValidationErrors
                    innerType
                    getBoxed
                    { ctx with
                        Pointer = sprintf "%s/%s" ctx.Pointer pi.Name
                        CurrentField = Some pi.Name }
                    rels
            )
            |> Array.toList

          let relValidators =
            relProps
            |> Array.map (fun pi ->
                let fastGetter = buildUntypedGetter pi
                fun ctx rels ->
                  getRelMemberValidationErrors
                    fastGetter
                    { ctx with
                        Pointer = sprintf "%s/%s" ctx.Pointer pi.Name
                        CurrentField = Some pi.Name }
                    rels
            )
            |> Array.toList

          { TypeName = typeName
            ResourceType = field.PropertyType
            DiscriminatorTag = ci.Tag
            DiscriminatorConstructor = discriminatorConstructor
            ReadOnly = getFieldsWithAttr typeof<ReadOnlyAttribute>
            WriteOnly = getFieldsWithAttr typeof<WriteOnlyAttribute>
            NotNull = getFieldsWithAttr typeof<NotNullAttribute>
            AllowedRelationshipTypes = relsAndAllowedTypes
            AttributeValidators = attrValidators
            RelationshipValidators = relValidators
            AttributeNames = attrProps |> Array.map (fun pi -> pi.Name) |> Set.ofArray
            RelationshipNames = relProps |> Array.map (fun pi -> pi.Name) |> Set.ofArray
          }
      )

    let duplicateNames =
      resourceInfos
      |> Array.groupBy (fun i -> i.TypeName)
      |> Array.filter (fun (n, is) -> is.Length > 1)
      |> Array.map fst
    if duplicateNames.Length > 0 then
      failwithf "Resource discriminator has more than one case for the following resource names: %A" (duplicateNames |> Array.toList)

    let resInfo = resourceInfos |> Array.map (fun i -> i.TypeName, i) |> Map.ofArray
    { ResourceInfo = resInfo
      SerializerSettings =
        resourceInfos
        |> Array.map (fun i -> i.TypeName, i.ResourceType)
        |> Map.ofArray
        |> Serialization.getSettings
      RequestValidationContext =
        { Pointer = ""
          CurrentType = None
          CurrentField = None
          ReadOnly =
            resInfo
            |> Map.toList
            |> List.collect (fun (_, i) ->
                i.ReadOnly |> Set.toList |> List.map (fun n -> i.TypeName, n))
            |> Set.ofList
          WriteOnly =
            resInfo
            |> Map.toList
            |> List.collect (fun (_, i) ->
                i.WriteOnly |> Set.toList |> List.map (fun n -> i.TypeName, n))
            |> Set.ofList
          NotNull =
            resInfo
            |> Map.toList
            |> List.collect (fun (_, i) ->
                i.NotNull |> Set.toList |> List.map (fun n -> i.TypeName, n))
            |> Set.ofList
          ReadOnlyIsOverridden = Set.empty
          WriteOnlyIsOverridden = Set.empty
          NotNullIsOverridden = Set.empty
          RequireNoIdForPost = false
          RequiredIdForPatch = None
          AllowedRelationshipTypes =
            resInfo
            |> Map.toList
            |> List.collect (fun (_, i) ->
                i.AllowedRelationshipTypes
                |> Map.toList
                |> List.map (fun (fn, names) -> (i.TypeName, fn), names))
            |> Map.ofList
          AttributeValidators =
            resInfo
            |> Map.map (fun k i -> Some k |> ignore; i.AttributeValidators)
          RelationshipValidators =
            resInfo
            |> Map.map (fun k i -> Some k |> ignore; i.RelationshipValidators)
          ValidationType = Request }
    }
