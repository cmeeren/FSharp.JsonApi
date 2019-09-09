FSharp.JsonApi
==============

A library that allows you to use F# to easily create and consume flexible, strongly typed web APIs following the [JSON-API specification](https://jsonapi.org/) – and an almost-production-ready API implementation sample to get you started on the right foot!

Core features:

* Full support for sparse fieldsets and included resources
* Support for loading included resources asynchronously on-demand, in parallel
* Uses [FSharp.JsonSkippable](https://github.com/cmeeren/FSharp.JsonSkippable) for strong typing of whether JSON properties are included or excluded
* And much more

The focus is on server implementations, but it may also be useful when implementing clients (please get in touch!).

Installation
------------

FSharp.JsonApi consists of three NuGet packages:

* **FSharp.JsonApi** contains all the core stuff: JSON-API document models for serialization/deserialization, resource builders, parsing and validation of query parameters and documents, etc.
* **FSharp.JsonApi.AspNetCore** contains lots of useful helpers and additional overloads for parsing and validating requests using ASP.NET Core’s `HttpContext`
* **FSharp.JsonApi.Giraffe** – a few simple helpers that may be useful if using [Giraffe](https://github.com/giraffe-fsharp/Giraffe/)

If using Giraffe, just install FSharp.JsonApi.Giraffe and you’ll get the other two automatically.

If using ASP.NET Core, but not Giraffe, install FSharp.JsonApi.AspNetCore and you’ll also get the core library automatically.

If you don’t use ASP.NET Core, you can easily use the core library to build your own abstractions.

Contributing
------------

Contributions and ideas are welcome! Please see [Contributing.md](https://github.com/cmeeren/FSharp.JsonApi/blob/master/.github/CONTRIBUTING.md) for details.

Quick start
-----------

I highly recommend you check out the [sample API](https://github.com/cmeeren/FSharp.JsonApi/tree/master/src/FSharp.JsonApi.SampleApp) in this repo, which is a simple but complete and almost-production-ready example API implementation. Open the main solution in VS, start at the topmost file, and read through the project in compilaton order. There are lots of comments along the way to explain what’s going on.

### 1. Define resources

Define each resource’s attributes and relationships using records, enums, and any relevant .NET attributes. The names will be used as-is; use double backticks for special names. Use `option` for nullable attributes. Use `ToOne` and `ToMany` for relationships. All attributes and relationships must be wrapped in `Skippable` (see [FSharp.JsonSkippable](https://github.com/cmeeren/FSharp.JsonSkippable/)).

```f#
type ArticleType =
  | personal = 0
  | commercial = 1

[<CLIMutable>]
type ArticleAttrs = {
  title: string Skippable
  ``type``: ArticleType Skippable
  [<ReadOnly>] updated: DateTimeOffset option Skippable
}

[<CLIMutable>]
type ArticleRels = {
  [<NotNull; AllowedTypes("person")>]
  author: ToOne Skippable
  [<ReadOnly; NotNull; AllowedTypes("comment")>]
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

The `JsonApiContext` is what actually contains all information about the resources, obtained from the resource discriminator. It is used to serialize, deserialize, create, parse, and validate documents.

```f#
let jsonApiCtx = JsonApiContext.create<ResourceDiscriminator>
```

### 3. Define the resource builders

The `ResourceBuildContext` is your friend when building attributes, relationships, links, and meta. It contains all information about sparse fieldsets, the current resource being built, its place in the include path, etc. and provides many helpers to get attributes, relationships, and related resources. Most `ResourceBuildContext` methods have many overloads for convenience; only some are used below.

```f#
module Article =

  let private getIdentifier (a: Article) =
    ResourceIdentifier.create TypeNames.article (ArticleId.toApi a.Id)

  let private getAttributes (ctx: ResourceBuildContext) (a: Article) =
    {
      title = ctx.GetAttribute("title", a.Title)
      ``type`` = ctx.GetAttribute("type", a.Type, ArticleType.toApi)
      updated = ctx.GetAttribute("updated", a.Updated)
    }

  let private getRelationships baseUrl (ctx: ResourceBuildContext) (a: Article) =
    async {

      // Use Async.StartChild to fetch the included resources in parallel

      let! authorComp =
        ctx.IncludeToOne(nameof <@ any<ArticleRels>.author @>,
          a.Id, Db.Person.authorForArticle, Person.getBuilder baseUrl, relatedLink = true)
        |> Async.StartChild

      let! commentsComp =
        ctx.IncludeToMany(nameof <@ any<ArticleRels>.comments @>,
          a.Id, Db.Comment.allForArticle, Comment.getBuilder baseUrl, relatedLink = true)
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

### 4A. Build documents

You can now build JSON-API documents like so:

```f#
jsonApiCtx.BuildDocument(article, Article.getBuilder, ctx)
```

where `ctx` is the ASP.NET Core `HttpContext`. Your resource document will be built and all sparse fieldsets, included resources etc. will be handled automatically for you.

### 4B. Receive documents

For example:

```f#
let result =
  jsonApiCtx
    .WithNoIdForPost()
    .Parse(Article, ctx)
```

This returns `Async<Result<Resource<ArticleAttrs, ArticleRels>, RequestDocumentError list>>`. See the [sample API](https://github.com/cmeeren/FSharp.JsonApi/tree/master/src/FSharp.JsonApi.SampleApp) for more information on simple and robust error handling, and more deserialization/parsing/validation options.

Documentation
-------------

TODO

In the meantime, I highly recommend you check out the [sample API](https://github.com/cmeeren/FSharp.JsonApi/tree/master/src/FSharp.JsonApi.SampleApp) in this repo, which is a simple but complete and almost-production-ready example API implementation. Open the main solution in VS, start at the topmost file, and read through the project in compilaton order. There are lots of comments along the way to explain what’s going on.

Release notes
-------------

TODO

