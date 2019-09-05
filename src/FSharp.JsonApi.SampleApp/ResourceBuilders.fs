module rec ResourceBuilders

open FSharp.JsonApi
open Domain
open Resources
open Converters


(*
This module contains the code that actually builds the resources. It needs to be
recursive since relationships can be recursive, and thus the builders may need
to reference each other.

The ResourceBuildContext type is your friend when building attributes,
relationships, links, and meta. It contains all information about sparse
fieldsets, the current resource being built, its place in the include path, etc.
and provides many helpers to get attributes, relationships, and related
resources. Read the documentation of each method to see exactly what it does.

Most ResourceBuildContext methods have many overloads for convenience; only some
are used below.
*)


module Person =

  let private getIdentifier (p: Person) =
    ResourceIdentifier.create TypeNames.person (PersonId.toApi p.Id)

  let private getSelfUrl baseUrl p =
    // The Uri module contains a few useful helpers. The functions are also
    // available as extension members on Uri.
    baseUrl |> Uri.addSegments [CollectionUrlSegments.person; (getIdentifier p).Id]

  let private getAttributes (ctx: ResourceBuildContext) (p: Person) =
    {
      firstName =
        ctx.GetAttribute(nameof <@ any<PersonAttrs>.firstName @>,
          p.FirstName)
      lastName =
        ctx.GetAttribute(nameof <@ any<PersonAttrs>.lastName @>,
          p.LastName)
      twitter =
        ctx.GetAttribute(nameof <@ any<PersonAttrs>.twitter @>,
          p.Twitter)
      gender =
        ctx.GetAttribute(nameof <@ any<PersonAttrs>.gender @>,
          p.Gender, Option.map Gender.toApi)
    }

  let private getRelationships baseUrl (ctx: ResourceBuildContext) (p: Person) =
    async {
      let! articlesRel, articlesBuilders =
        ctx.IncludeToMany(nameof <@ any<PersonRels>.articles @>,
          p.Id, Db.Article.allByAuthor, Article.getBuilder baseUrl, relatedLink = true)

      return
        { articles = articlesRel },
        articlesBuilders
    }

  let private getLinks (ctx: ResourceBuildContext) =
    ctx.CreateSelfLink ()
    // Just an example to show how to easily add more links
    |> ctx.AddLink("foo")
    // Links can also be added conditionally
    |> ctx.AddLink("bar", false)

  (*
  The function to get the actual resource builder must conform to the following
  rules:
    - It must be cheap - do not eagerly evaluate attributes/relationships
    - The two last parameters must be the ResourceBuildContext and the entity to
      be built (in order to be usable from JsonApiContext.BuildDocument as shown
      in HttpHandlers.fs). You can of course pass as many parameters as you need
      before that (e.g. the api base URL, authenticated user objects, etc.)
  *)

  let getBuilder baseUrl (ctx: ResourceBuildContext) p =
    ResourceBuilder
      .Create(Person, getIdentifier p)
      .WithSelfUrl(getSelfUrl baseUrl p)
      .WithAttributes(fun () -> getAttributes ctx p)
      .WithRelationships(getRelationships baseUrl ctx p)
      .WithLinks(fun () -> getLinks ctx)



module Article =

  let private getIdentifier (a: Article) =
    ResourceIdentifier.create TypeNames.article (ArticleId.toApi a.Id)

  let private getSelfUrl baseUrl a =
    baseUrl |> Uri.addSegments [CollectionUrlSegments.article; (getIdentifier a).Id]

  let private getAttributes (ctx: ResourceBuildContext) (a: Article) =
    {
      title =
        ctx.GetAttribute(nameof <@ any<ArticleAttrs>.title @>,
          a.Title)
      body =
        ctx.GetAttribute(nameof <@ any<ArticleAttrs>.body @>,
          a.Body)
      ``type`` =
        ctx.GetAttribute(nameof <@ any<ArticleAttrs>.``type`` @>,
          a.Type, ArticleType.toApi)
      created =
        ctx.GetAttribute(nameof <@ any<ArticleAttrs>.created @>,
          a.Created)
      updated =
        ctx.GetAttribute(nameof <@ any<ArticleAttrs>.updated @>,
          a.Updated)
    }

  let private getRelationships baseUrl (ctx: ResourceBuildContext) (a: Article) =
    async {

      // We use Async.StartChild to fetch the included resources in parallel

      let! authorComp =
        ctx.IncludeToOne(nameof <@ any<ArticleRels>.author @>,
          a.Id, Db.Person.authorForArticle, Person.getBuilder baseUrl, relatedLink = true)
        |> Async.StartChild

      let! commentsComp =
        ctx.IncludeToMany(nameof <@ any<ArticleRels>.comments @>,
          a.Id, Db.Comment.allForArticle, Comment.getBuilder baseUrl, relatedLink = true)
        |> Async.StartChild

      // Actually wait for them to be fetched

      let! authorRel, authorBuilder = authorComp
      let! commentsRel, commentsBuilders = commentsComp

      let rels = {
        author = authorRel
        comments = commentsRel
      }

      let builders = [
        yield! authorBuilder |> Option.toList
        yield! commentsBuilders
      ]

      return rels, builders
    }

  let private getLinks (ctx: ResourceBuildContext) =
    ctx.CreateSelfLink ()

  let getBuilder baseUrl (ctx: ResourceBuildContext) a =
    ResourceBuilder
      .Create(Article, getIdentifier a)
      .WithSelfUrl(getSelfUrl baseUrl a)
      .WithAttributes(fun () -> getAttributes ctx a)
      .WithRelationships(getRelationships baseUrl ctx a)
      .WithLinks(fun () -> getLinks ctx)



module Comment =

  let private getIdentifier (c: Comment) =
    ResourceIdentifier.create TypeNames.comment (CommentId.toApi c.Id)

  let private getSelfUrl baseUrl c =
    baseUrl |> Uri.addSegments [CollectionUrlSegments.comment; (getIdentifier c).Id]

  let private getAttributes (ctx: ResourceBuildContext) (c: Comment) =
    {
      body =
        ctx.GetAttribute(nameof <@ any<ArticleAttrs>.body @>,
          c.Body)
    }

  let private getRelationships baseUrl (ctx: ResourceBuildContext) (c: Comment) =
    async {

      let! authorComp =
        ctx.IncludeToOne(nameof <@ any<CommentRels>.author @>,
          c.Id, Db.Person.authorForComment, Person.getBuilder baseUrl, relatedLink = true)
        |> Async.StartChild

      let! articleComp =
        ctx.IncludeToOne(nameof <@ any<CommentRels>.article @>,
          c.Id, Db.Article.forComment, Article.getBuilder baseUrl, relatedLink = true)
        |> Async.StartChild

      let! authorRel, authorBuilder = authorComp
      let! articleRel, articleBuilder = articleComp

      return
        { author = authorRel
          article = articleRel },
        [ yield! authorBuilder |> Option.toList
          yield! articleBuilder |> Option.toList ]
    }

  let private getLinks (ctx: ResourceBuildContext) =
    ctx.CreateSelfLink ()

  let getBuilder baseUrl (ctx: ResourceBuildContext) c =
    ResourceBuilder
      .Create(Comment, getIdentifier c)
      .WithSelfUrl(getSelfUrl baseUrl c)
      .WithAttributes(fun () -> getAttributes ctx c)
      .WithRelationships(getRelationships baseUrl ctx c)
      .WithLinks(fun () -> getLinks ctx)
