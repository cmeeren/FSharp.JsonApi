FSharp.JsonApi
==============

A library that allows you to use F# to easily create and consume flexible, strongly typed web APIs following the [JSON-API specification](https://jsonapi.org/) – and an almost-production-ready API implementation sample to get you started on the right foot!

Core features:

* Full support for sparse fieldsets and included resources
* Support for loading included resources asynchronously on-demand, in parallel
* Uses [FSharp.JsonSkippable](https://github.com/cmeeren/FSharp.JsonSkippable) for strong typing of whether JSON properties are included or excluded
* Plays very nicely with the robust error handling of [FsToolkit.ErrorHandling](https://github.com/demystifyfp/FsToolkit.ErrorHandling/), whether monadic or applicative (the latter works perfectly to return multiple JSON-API errors at once)
* And much more

The focus is on server implementations, but it may also be useful when implementing clients (please get in touch!).

### Production readiness

We use this library for at least two mission-critical production APIs. I have developed and tweaked it internally for around one and a half years before finally polishing it and publishing to NuGet. I’m not claiming it’s perfect, or even bug-free, but it’s battle-tested, and I have a vested interest in keeping this library working properly.

Installation
------------

FSharp.JsonApi consists of three NuGet packages:

* **FSharp.JsonApi** contains all the core stuff: JSON-API document models for serialization/deserialization, resource builders, parsing and validation of query parameters and documents, etc. If you don’t use ASP.NET Core, you can easily use this library to build your own abstractions.
* **FSharp.JsonApi.AspNetCore** contains lots of useful helpers and additional overloads for parsing and validating requests using ASP.NET Core’s `HttpContext`.
* **FSharp.JsonApi.Giraffe** contains a few simple helpers that may be useful if using [Giraffe](https://github.com/giraffe-fsharp/Giraffe/).

Install all packages that are relevant for you. You can get away with installing only the highest-level package – e.g. FSharp.JsonApi.Giraffe – and have the rest installed automatically as transitive dependencies, but depending on your package manager, you might not be able to easily update the lower-level (transitive) packages (Paket is better at this than NuGet).

Contributing
------------

Contributions and ideas are welcome! Please see [Contributing.md](https://github.com/cmeeren/FSharp.JsonApi/blob/master/.github/CONTRIBUTING.md) for details.

Quick start
-----------

I highly recommend you check out the [sample API](https://github.com/cmeeren/FSharp.JsonApi/tree/master/src/FSharp.JsonApi.SampleApp) in this repo, which is a simple but complete and almost-production-ready example API implementation. Open the main solution in VS, start at the topmost file, and read through the project in compilation order. There are lots of comments along the way to explain what’s going on.

As a very short introduction, I hope the steps below are useful, but bear in mind that it only skims the surface.

### 1. Define resources

Define each resource’s attributes and relationships using records, enums, and any relevant .NET attributes. The names will be used as-is; use double backticks for special names. Use `option` for nullable attributes. Use `ToOne` and `ToMany` for relationships. All attributes and relationships must be wrapped in `Skippable` (see [FSharp.JsonSkippable](https://github.com/cmeeren/FSharp.JsonSkippable/)).

```f#
type ArticleType =
  | personal = 0
  | commercial = 1

[<CLIMutable>]
type ArticleAttrs = {
  title: string Skippable
  articleType: ArticleType Skippable
  [<ReadOnly>] updated: DateTimeOffset option Skippable
}

[<CLIMutable>]
type ArticleRels = {
  [<NotNull; AllowedTypes("person")>]
  author: ToOne Skippable
  [<ReadOnly; AllowedTypes("comment")>]
  comments: ToMany Skippable
}
```

### 2. Define the resource discriminator and the `JsonApiContext`

The resource discriminator is a DU where each case is a `Resource<'attrs, 'rels>`. It’s used to provide FSharp.JsonApi with all relevant information about the resources, and allows for a nice and friendly syntax when parsing resources.

```f#
type ResourceDiscriminator =
  | [<ResourceName("article")>]
    Article of Resource<ArticleAttrs, ArticleRels>
```

The `JsonApiContext` is what actually contains all information about the resources, obtained from the resource discriminator. It is used to serialize, deserialize, create, parse, and validate documents. Define it once and use it everywhere.

```f#
let jsonApiCtx = JsonApiContext.create<ResourceDiscriminator>
```

### 3. Define the resource builders

The `ResourceBuildContext` is your friend when building attributes, relationships, links, and meta. It contains all information about sparse fieldsets, the current resource being built, its place in the include path, etc. and provides many helpers to get attributes, relationships, and related resources. Most `ResourceBuildContext` methods have many overloads for convenience; only some are used below.

```f#
module Article =

  let private getIdentifier (a: Article) =
    ResourceIdentifier.create TypeNames.article a.Id

  let private getAttributes (ctx: ResourceBuildContext) (a: Article) =
    {
      title = ctx.GetAttribute("title", a.Title)
      articleType = ctx.GetAttribute("articleType", a.Type, ArticleType.toApi)
      updated = ctx.GetAttribute("updated", a.Updated)
    }

  let private getRelationships baseUrl (ctx: ResourceBuildContext) (a: Article) =
    async {

      // Use Async.StartChild to fetch the included resources in parallel

      let! authorComp =
        ctx.IncludeToOne("author", a.Id, Db.Person.authorForArticle, Person.getBuilder baseUrl)
        |> Async.StartChild

      let! commentsComp =
        ctx.IncludeToMany("comments", a.Id, Db.Comment.allForArticle, Comment.getBuilder baseUrl)
        |> Async.StartChild

      // Actually wait for them to be fetched

      let! authorRelationship, authorBuilder = authorComp
      let! commentsRelationship, commentsBuilders = commentsComp

      // Return the relationships and the builders that will be
      // used to build the related resources

      let relationships = {
        author = authorRelationship
        comments = commentsRelationship
      }

      let builders = [
        yield! authorBuilder |> Option.toList
        yield! commentsBuilders
      ]

      return relationships, builders
    }

  // Here is the function to actually get the resource builder.
  // It must be cheap (do not eagerly evaluate attributes or resources)
  // and the two final parameters must be the ResourceBuildContext
  // and the domain object to be built.

  let getBuilder baseUrl (ctx: ResourceBuildContext) a =
    ResourceBuilder
      .Create(Article, getIdentifier a)
      .WithAttributes(fun () -> getAttributes ctx a)
      .WithRelationships(getRelationships baseUrl ctx a)
      // You can add links and meta, too; check out the sample API
```

### Profit!

#### Build documents

You can now build JSON-API documents like so:

```f#
jsonApiCtx.BuildDocument(article, Article.getBuilder, ctx)
```

where `ctx` is the ASP.NET Core `HttpContext`. Your resource document will be built and all sparse fieldsets, included resources etc. will be handled automatically for you.

#### Receive documents

For example:

```f#
let result =
  jsonApiCtx
    .WithNoIdForPost()
    .Parse(Article, ctx)
```

This returns `Async<Result<Resource<ArticleAttrs, ArticleRels>, RequestDocumentError list>>`. See the [sample API](https://github.com/cmeeren/FSharp.JsonApi/tree/master/src/FSharp.JsonApi.SampleApp) for more information on simple and robust error handling, and more deserialization/parsing/validation options.

#### Parse query parameters

Example of applicative error handling (using operators from [FsToolkit.ErrorHandling](https://github.com/demystifyfp/FsToolkit.ErrorHandling/)):

```f#
ArticleSearchArgs.create
<!> Query.GetSingle("filter[title]", ctx)
<*> Query.GetBoundInt("page[offset]", 0, ctx, min=0)
<*> Query.GetBoundInt("page[limit]", 10, ctx, min=1)
```

#### And more

Check out the sample API y’all 😉

Documentation
-------------

TODO

In the meantime, I highly recommend you check out the [sample API](https://github.com/cmeeren/FSharp.JsonApi/tree/master/src/FSharp.JsonApi.SampleApp) in this repo, which is a simple but complete and almost-production-ready example API implementation. Open the main solution in VS, start at the topmost file, and read through the project in compilation order. There are lots of comments along the way to explain what’s going on.

Release notes
-------------

### vNext (FSharp.JsonApi 2.0.0-alpha-08, FSharp.JsonApi.AspNetCore 2.0.0-alpha-06, FSharp.JsonApi.Giraffe 2.0.0-alpha-06)

* **Breaking:** Removed `SimpleResource` and related methods/extensions on `JsonApiContext`. Use `Resource.attributesOrDefault` and `Resource.relationshipsOrDefault` instead to get a (possibly default) attribute/relationship instance from a resource.
* **Breaking:** Added `RequestDocumentError.UnknownMainResourceType`
* **Breaking:** `JsonApiContext.GetResource` and the `JsonApiContext.Parse` overload returning a resource discriminator now return errors if the resource type is unknown. The signature of the former method is changed; the latter is a behaviour change only.
* **Breaking:** Renamed `Query` to `QueryParser`, added static methods to create an instance with a query parameter map or `HttpContext`, and changed most methods to instance members that do not depend on a query parameter map or `HttpContext`
* **Breaking:**  `JsonApiContext.create` now throws on invalid attribute and relationship names. Use the new `AllowIllegalNameAttribute` to disable the check for specific attributes/relationships.
* Added `Setter` type with helper methods to chain and lift normal "immutable setter"
  functions to accept parsed, possibly optional arguments, combining any errors
* Added `JsonApiContext` methods `RequireResource`, `WithAllReadOnly`, `ToDiscriminator`, `FromDiscriminator`, and `SerializeAndGetBytes`
* Added `JsonApiContext` overloads for `ParseRequired`, `WithReadOnly`, `WithWriteOnly`, and `WithNotNull`
*  Added async overloads for `ResourceBuildContext` methods `GetAttribute` and `GetExplicitAttribute`
*  Added an `HttpContext.WriteJsonApiAsync` extension overload accepting a byte array
*  Added new Giraffe HTTP handlers `jsonApiETag`, `jsonApiETagWith`, and `jsonApiBytes`
*  Fixed `Uri.addQuery` and `Uri.setQuery` not behaving correctly for multiple identical query keys and for query keys that only differ by case
*  Fixed validation when `links` collection is `null` (JSON: `"links": null`)
*  Serializes `Uri`s to canonical format using `Uri.ToString()` instead of `Uri.OriginalString` which is normally used by Newtonsoft.Json. See [dotnet/corefx#41679](https://github.com/dotnet/corefx/issues/41679) and [JamesNK/Newtonsoft.Json/2190](https://github.com/JamesNK/Newtonsoft.Json/issues/2190).
*  Made order of included resources deterministic (needed to get stable hashes of response for ETag)

### FSharp.JsonApi 1.4.1

* Fixed `Attribute.GetNonNull` returning errors for skipped values

### FSharp.JsonApi.AspNetCore 1.1.0

* Add `validate` optional parameter to all `JsonApiContext.Parse` extensions
* Add `ParseRequired` and `ParseSimpleRequired` extensions members for `JsonApiContext`

### FSharp.JsonApi 1.4.0

* Add `ParseRequired` and `ParseSimpleRequired` to `JsonApiContext`

### FSharp.JsonApi 1.3.0

* Add more `Attribute` overloads

### FSharp.JsonApi 1.1.0

* Add more `Relationship` overloads

### FSharp.JsonApi 1.0.0, FSharp.JsonApi.AspNetCore 1.0.0, FSharp.JsonApi.Giraffe 1.0.0

* Initial release
