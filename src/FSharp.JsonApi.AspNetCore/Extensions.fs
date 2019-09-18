namespace FSharp.JsonApi

open System
open System.IO
open System.Text
open System.Threading.Tasks
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Primitives
open Microsoft.Net.Http.Headers


[<AutoOpen>]
module HttpContextExtensions =

  type HttpContext with

    /// Sets the HTTP "Location" header to the value of the main resource's
    /// "self" URL, if present.
    member this.SetLocationHeaderFromMainSelfUrl (doc: ResourceDocument) =
      match ResourceDocument.mainSelfUrl doc with
      | None -> ()
      | Some url -> this.Response.Headers.Add("Location", url |> string |> StringValues)

    /// Serializes the document and writes the output to the body of the HTTP
    /// response. Also sets the HTTP `Content-Type` header to
    /// `application/vnd.api+json` and sets the `Content-Length` header
    /// accordingly.
    member this.WriteJsonApiAsync (dataObj: #IJsonApiDocument, jsonApiCtx: JsonApiContext<'ResourceDiscriminator>) =
      let bytes = jsonApiCtx.Serialize dataObj |> Encoding.UTF8.GetBytes
      this.Response.Headers.[HeaderNames.ContentType] <- MediaTypes.jsonApi |> StringValues
      this.Response.Headers.[HeaderNames.ContentLength] <- bytes.Length |> string |> StringValues
      if this.Request.Method <> HttpMethods.Head then
        this.Response.Body.WriteAsync(bytes, 0, bytes.Length)
      else Task.CompletedTask


    /// Validates the JSON-API request (not including the request body). The
    /// customQueryNameRegexes parameter can be used to whitelist custom query
    /// parameter names.
    member this.ValidateJsonApiRequest (customQueryNameRegexes: string list) =
      let errs =
        [
          if not <| Validation.acceptsJsonApi this then yield RequestError.InvalidAccept
          if Validation.hasNonJsonApiContent this then yield RequestError.InvalidContentType
          if Validation.allJsonApiAcceptsHaveParams this then yield RequestError.InvalidAcceptParams
          if Validation.jsonApiContentTypeHasParams this then yield RequestError.InvalidContentTypeParams

          match Validation.getIllegalQueryStringParams customQueryNameRegexes this with
          | [] -> ()
          | names -> yield! names |> List.map RequestError.IllegalQueryParamName
        ]
      match errs with
      | [] -> Ok ()
      | errs -> Error errs

    /// Validates the JSON-API request (not including the request body).
    member this.ValidateJsonApiRequest () =
      this.ValidateJsonApiRequest([])


[<AutoOpen>]
module JsonApiContextExtensions =

  let private getBody (ctx: HttpContext) =
    async {
      use reader = new StreamReader(ctx.Request.Body, Encoding.UTF8)
      return! reader.ReadToEndAsync() |> Async.AwaitTask
    }

  type JsonApiContext<'ResourceDiscriminator> with

    /// Performs strict validation. Currently it verifies fieldsets and include paths in a
    /// request. This should not be done in production environments since it might
    /// conflict with the must-ignore clause of the JSON-API specification, but it can be
    /// useful in testing environments to catch typos in these query parameters.
    ///
    /// mainDataTypes are the resource type names that can be returned as main data, and
    /// is needed to validate include paths. If it is empty, include paths will not be
    /// validated.
    member this.ValidateStrict
        ( ctx: HttpContext,
          [<ParamArray>] mainDataTypes: TypeName []
        ) : Result<unit, StrictError list> =
      this.ValidateStrict(Query.GetFieldsets ctx, Query.GetIncludePaths ctx, mainDataTypes)

    /// Performs strict validation. Currently it verifies fieldsets and include paths in a
    /// request. This should not be done in production environments since it might
    /// conflict with the must-ignore clause of the JSON-API specification, but it can be
    /// useful in testing environments to catch typos in these query parameters.
    ///
    /// mainDataTypeDiscriminatorCase indicates the resource type that can be returned as
    /// main data.
    member this.ValidateStrict
        ( ctx: HttpContext,
          mainDataTypeDiscriminatorCase: Resource<'attrs, 'rels> -> 'ResourceDiscriminator
        ) : Result<unit, StrictError list> =
      this.ValidateStrict(
        Query.GetFieldsets ctx, Query.GetIncludePaths ctx, mainDataTypeDiscriminatorCase)

    /// Performs strict validation. Currently it verifies fieldsets and include paths in a
    /// request. This should not be done in production environments since it might
    /// conflict with the must-ignore clause of the JSON-API specification, but it can be
    /// useful in testing environments to catch typos in these query parameters.
    ///
    /// the mainDataTypeDiscriminatorCase arguments indicate the resource types that can
    /// be returned as main data.
    member this.ValidateStrict
        ( ctx: HttpContext,
          mainDataTypeDiscriminatorCase1: Resource<'attrs1, 'rels1> -> 'ResourceDiscriminator,
          mainDataTypeDiscriminatorCase2: Resource<'attrs2, 'rels2> -> 'ResourceDiscriminator
        ) : Result<unit, StrictError list> =
      this.ValidateStrict(
        Query.GetFieldsets ctx, Query.GetIncludePaths ctx,
        mainDataTypeDiscriminatorCase1, mainDataTypeDiscriminatorCase2)

    /// Performs strict validation. Currently it verifies fieldsets and include paths in a
    /// request. This should not be done in production environments since it might
    /// conflict with the must-ignore clause of the JSON-API specification, but it can be
    /// useful in testing environments to catch typos in these query parameters.
    ///
    /// the mainDataTypeDiscriminatorCase arguments indicate the resource types that can
    /// be returned as main data.
    member this.ValidateStrict
        ( ctx: HttpContext,
          mainDataTypeDiscriminatorCase1: Resource<'attrs1, 'rels1> -> 'ResourceDiscriminator,
          mainDataTypeDiscriminatorCase2: Resource<'attrs2, 'rels2> -> 'ResourceDiscriminator,
          mainDataTypeDiscriminatorCase3: Resource<'attrs3, 'rels3> -> 'ResourceDiscriminator
        ) : Result<unit, StrictError list> =
      this.ValidateStrict(
        Query.GetFieldsets ctx, Query.GetIncludePaths ctx,
        mainDataTypeDiscriminatorCase1, mainDataTypeDiscriminatorCase2, mainDataTypeDiscriminatorCase3)

    /// Reads and deserializes the request content to a single-resource document. Returns
    /// None if the request body is empty.
    member this.DeserializeResourceDocument
        (ctx: HttpContext)
        : Async<Result<ResourceDocument option, RequestDocumentError list>> =
      async {
        let! json = getBody ctx
        return this.DeserializeResourceDocument(json)
      }

    /// Reads and deserializes the request content to a resource collection document.
    /// Returns None if the request body is empty.
    member this.DeserializeResourceCollectionDocument
        (ctx: HttpContext)
        : Async<Result<ResourceCollectionDocument option, RequestDocumentError list>> =
      async {
        let! json = getBody ctx
        return this.DeserializeResourceCollectionDocument(json)
      }

    /// Reads and deserializes the request content to a resource identifier document.
    /// Returns None if the request body is empty.
    member this.DeserializeResourceIdentifierDocument
        (ctx: HttpContext)
        : Async<Result<ResourceIdentifierDocument option, RequestDocumentError list>> =
      async {
        let! json = getBody ctx
        return this.DeserializeResourceIdentifierDocument(json)
      }

    /// Reads and deserializes the request content to a resource identifier collection
    /// document. Returns None if the request body is empty.
    member this.DeserializeResourceIdentifierCollectionDocument
        (ctx: HttpContext)
        : Async<Result<ResourceIdentifierCollectionDocument option, RequestDocumentError list>> =
      async {
        let! json = getBody ctx
        return this.DeserializeResourceIdentifierCollectionDocument(json)
      }

    /// Reads and deserializes the request content to an error document. Returns None if
    /// the request body is empty.
    member this.DeserializeErrorDocument
        (ctx: HttpContext)
        : Async<Result<ErrorDocument option, RequestDocumentError list>> =
      async {
        let! json = getBody ctx
        return this.DeserializeErrorDocument(json)
      }

    /// Reads the request body, deserializes it to single-resource document,
    /// validates it (unless validate is false), and extracts the main data
    /// resource as a resource discriminator. Returns None if the resource type
    /// is unknown.
    member this.Parse
        ( ctx: HttpContext,
          ?validate: bool
        )
        : Async<Result<'ResourceDiscriminator option, RequestDocumentError list>> =
      async {
        let! json = getBody ctx
        return this.Parse(json, ?validate = validate)
      }

    /// Reads the request body, deserializes it to a single-resource document,
    /// validates it (unless validate is false), and extracts a resource of the
    /// specified type. Returns errors if the type doesn't match.
    member this.Parse
        ( discriminatorCase: Resource<'attrs, 'rels> -> 'ResourceDiscriminator,
          ctx: HttpContext,
          ?validate: bool
        ) : Async<Result<Resource<'attrs, 'rels> option, RequestDocumentError list>> =
      async {
        let! json = getBody ctx
        return this.Parse(discriminatorCase, json, ?validate = validate)
      }

    /// Reads the request body, deserializes it to a single-resource document,
    /// validates it (unless validate is false), and extracts a resource of one
    /// of the specified types. Returns errors if the type doesn't match. Use
    /// ParseRequired if the request document is required.
    member this.Parse
        ( discriminatorCase1: Resource<'attrs1, 'rels1> -> 'ResourceDiscriminator,
          discriminatorCase2: Resource<'attrs2, 'rels2> -> 'ResourceDiscriminator,
          ctx: HttpContext,
          ?validate: bool
        ) : Async<Result<Choice<Resource<'attrs1, 'rels1>,
                                Resource<'attrs2, 'rels2>> option,
                         RequestDocumentError list>> =
      async {
        let! json = getBody ctx
        return this.Parse(discriminatorCase1, discriminatorCase2, json, ?validate = validate)
      }

    /// Reads the request body, deserializes it to a single-resource document,
    /// validates it (unless validate is false), and extracts a resource of one
    /// of the specified types. Returns errors if the type doesn't match. Use
    /// ParseRequired if the request document is required.
    member this.Parse
        ( discriminatorCase1: Resource<'attrs1, 'rels1> -> 'ResourceDiscriminator,
          discriminatorCase2: Resource<'attrs2, 'rels2> -> 'ResourceDiscriminator,
          discriminatorCase3: Resource<'attrs3, 'rels3> -> 'ResourceDiscriminator,
          ctx: HttpContext,
          ?validate: bool
        ) : Async<Result<Choice<Resource<'attrs1, 'rels1>,
                                Resource<'attrs2, 'rels2>,
                                Resource<'attrs3, 'rels3>> option,
                          RequestDocumentError list>> =
      async {
        let! json = getBody ctx
        return this.Parse(discriminatorCase1, discriminatorCase2, discriminatorCase3, json, ?validate = validate)
      }

    /// Reads the request body, deserializes it to a single-resource document,
    /// validates it (unless validate is false), and extracts a resource of one
    /// of the specified types. Returns errors if the type doesn't match or if
    /// there is no resource.
    member this.ParseRequired
        ( discriminatorCase1: Resource<'attrs1, 'rels1> -> 'ResourceDiscriminator,
          discriminatorCase2: Resource<'attrs2, 'rels2> -> 'ResourceDiscriminator,
          ctx: HttpContext,
          ?validate: bool
        ) : Async<Result<Choice<Resource<'attrs1, 'rels1>,
                                Resource<'attrs2, 'rels2>>,
                         RequestDocumentError list>> =
      async {
        let! json = getBody ctx
        return this.ParseRequired(discriminatorCase1, discriminatorCase2, json, ?validate = validate)
      }

    /// Reads the request body, deserializes it to a single-resource document,
    /// validates it (unless validate is false), and extracts a resource of one
    /// of the specified types. Returns errors if the type doesn't match or if
    /// there is no resource.
    member this.ParseRequired
        ( discriminatorCase1: Resource<'attrs1, 'rels1> -> 'ResourceDiscriminator,
          discriminatorCase2: Resource<'attrs2, 'rels2> -> 'ResourceDiscriminator,
          discriminatorCase3: Resource<'attrs3, 'rels3> -> 'ResourceDiscriminator,
          ctx: HttpContext,
          ?validate: bool
        ) : Async<Result<Choice<Resource<'attrs1, 'rels1>,
                                Resource<'attrs2, 'rels2>,
                                Resource<'attrs3, 'rels3>>,
                          RequestDocumentError list>> =
      async {
        let! json = getBody ctx
        return this.ParseRequired(discriminatorCase1, discriminatorCase2, discriminatorCase3, json, ?validate = validate)
      }

    /// Reads the request body, deserializes it to a resource collection
    /// document, validates it (unless validate is false), and extracts the main
    /// data resources as resource discriminators. Ignores resources with
    /// unknown types.
    member this.ParseCollection
        ( ctx: HttpContext,
          ?validate: bool)
        : Async<Result<'ResourceDiscriminator list, RequestDocumentError list>> =
      async {
        let! json = getBody ctx
        return this.ParseCollection(json, ?validate = validate)
      }

    /// Reads the request body, deserializes it to a resource collection
    /// document, validates it (unless validate is false), and extracts all main
    /// data resources. If ignoreUnknown is true, ignores resources with
    /// non-matching types, otherwise returns errors if any resource does not
    /// match the specified type.
    member this.ParseCollection
        ( discriminatorCase: Resource<'attrs, 'rels> -> 'ResourceDiscriminator,
          ctx: HttpContext,
          ?ignoreUnknown: bool,
          ?validate: bool
        ) : Async<Result<Resource<'attrs, 'rels> list, RequestDocumentError list>> =
      async {
        let! json = getBody ctx
        return this.ParseCollection(discriminatorCase, json, ?ignoreUnknown = ignoreUnknown, ?validate = validate)
      }

    /// Returns a single-resource document for the specified resource.
    /// Automatically parses sparse fieldsets and includes from the HttpContext.
    member this.BuildDocument
        ( entity: 'entity option,
          getBuilder: ResourceBuildContext -> 'entity -> ResourceBuilder<'attrs, 'rels>,
          ctx: HttpContext
        ) : Async<ResourceDocument> =
      this.BuildDocument(
        entity, getBuilder, Query.GetFieldsets ctx, Query.GetIncludePaths ctx)

    /// Returns a resource collection document for the specified resources.
    /// Automatically parses sparse fieldsets and includes from the HttpContext.
    member this.BuildDocument
        ( entities: 'entity list,
          getBuilder: ResourceBuildContext -> 'entity -> ResourceBuilder<'attrs, 'rels>,
          ctx: HttpContext
        ) : Async<ResourceCollectionDocument> =
      this.BuildDocument(
        entities, getBuilder, Query.GetFieldsets ctx, Query.GetIncludePaths ctx)


// Hack to make this have lower priority (to help compiler determine correct overload
// between 'entity and 'entity option)
[<AutoOpen>]
module JsonApiContextExtensions2 =

  type JsonApiContext<'ResourceDiscriminator> with

    /// Returns a single-resource document for the specified resource.
    /// Automatically parses sparse fieldsets and includes from the HttpContext.
    member this.BuildDocument
        ( entity: 'entity,
          getBuilder: ResourceBuildContext -> 'entity -> ResourceBuilder<'attrs, 'rels>,
          ctx: HttpContext)
        : Async<ResourceDocument>
        =
      this.BuildDocument(
        entity, getBuilder, Query.GetFieldsets ctx, Query.GetIncludePaths ctx)
