﻿module HttpHandlers

(*
This module contains the Giraffe HttpHandlers that contain the API-level logic
for each operation. Ideally, the HttpHandlers contain only code for parsing the
raw data from the request (query parameters, body, etc.) and writing the
response, and otherwise delegate further parsing (to domain entities) and
business functionality to lower layers.

This module makes extensive use of monadic/applicative error handling through
FsToolkit.ErrorHandling. All HttpHandlers are written not as task {} computation
expressions as in "vanilla" Giraffe, but as asyncResult {} computation
expressions returning Async<Result<HttpHandler, ApiError list>>. A helper
function (handleAsyncResult) is used at the end of each handler to "convert"
back a normal HttpHandler that calls the handler or returns the errors.

The above is not required by FSharp.JsonApi, but it plays nicely with it. You
might be able to write a Giraffe-friendly taskResult {} computation expression
if you wanted.
*)

open System
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Http.Extensions
open Serilog
open Giraffe
open FSharp.Control.Tasks.V2.ContextInsensitive
open FSharp.JsonApi
open FSharp.JsonSkippable
open FsToolkit.ErrorHandling
open FsToolkit.ErrorHandling.Operator.Validation

open Domain
open Resources
open Converters
open ResourceBuilders
open ErrorHandling


[<AutoOpen>]
module Helpers =

  /// Wraps a string in "filter[]".
  let wrapFilter = sprintf "filter[%s]"

  /// Takes an Option-returning parser and turns it into a Result-returning
  /// parser with an error message about the input not being a valid targetType.
  let withInvalidTypeMsg targetType f x =
    f x |> Result.requireSome (sprintf "The value '%O' is not a valid %s" x targetType)

  /// Parses the string as a DateTimeOffset or returns a suitable error message.
  let parseDateTimeOffset (str: string) =
    match DateTimeOffset.TryParse str with
    | true, d -> Ok d
    | false, _ -> Error (sprintf "Not a valid date-time: %s" str)


  type HttpRequest with

    member this.SafeUrl =
      this.GetEncodedUrl () |> Uri

    /// The API's base URL.
    // (In a production environment with reverse proxies and whatnot, you'd
    // might want to hardcode this to a known good value.)
    member this.BaseUrl =
      Uri(this.SafeUrl.GetLeftPart(UriPartial.Authority))


  // FSharp.JsonApi.Giraffe has an HttpHandler called jsonApi that writes a
  // response document. Here we wrap that handler, with two goals:
  //  1. Bake in the jsonApiCtx parameter so we don't have to write that
  //     everywhere
  //  2. Validate the response and log any errors. If we return invalid response
  //     bodies to clients (e.g. with invalid null values, or with fields marked
  //     as [<WriteOnly>] such as a user's password), it'd be nice to know.
  //
  // Validation can take a non-trivial amount of time for very large responses,
  // so since it's not relevant for the client, it's done on a background
  // thread.
  let jsonApi doc : HttpHandler =
    fun next ctx ->
      task {
        async {
          match jsonApiCtx.ValidateResponse doc with
          | Ok _ -> ()
          | Error errs -> errs |> List.iter (fun err -> Log.Warning("Response document contained error {Error}", sprintf "%A" err))
        } |> Async.Start
        return! jsonApi jsonApiCtx doc next ctx
      }


  // This is just an example of how to do logging of error responses. At the
  // very least, the Error.Id should be logged so you can look it up when an API
  // client comes to you with an error response they got. I use the code below
  // in production for several APIs.
  let private doHandleErrors mainStatus (statusesAndErrors: (int * Error) list) : HttpHandler =
    fun (next : HttpFunc) (ctx : HttpContext) ->
      match statusesAndErrors with
      | [] -> failwith "Error handler got empty error list"  // Should never happen. Will cause 500 if it does.
      | [_, err] ->
          // One error
          Log.Information(
            "Request failed with error {ErrorCode}: {ErrorTitle}: {ErrorMessage} (ID for this occurrence: {OccurrenceId})",
            err.Code |> Skippable.defaultValue "<none>",
            err.Title |> Skippable.defaultValue "<none>",
            err.Detail |> Skippable.defaultValue "<none>",
            err.Id |> Skippable.defaultValue "<none>")
      | errs ->
          // Several errors
          Log.Information("Request failed with {NumErrors} errors", errs.Length)
          for i, (_, err) in Seq.indexed errs do
            Log.Information(
              "[{ErrNum}/{NumErrs}] {ErrorCode}: {ErrorTitle}: {ErrorMessage} (ID for this occurrence: {OccurrenceId})",
              i + 1,
              errs.Length,
              err.Code |> Skippable.defaultValue "<none>",
              err.Title |> Skippable.defaultValue "<none>",
              err.Detail |> Skippable.defaultValue "<none>",
              err.Id |> Skippable.defaultValue "<none>")

      // Set the error's Status property based on the numeric status code.
      let errors =
        statusesAndErrors
        |> List.map (fun (status, err) -> err |> Error.setStatus status)

      // Get the status code for the whole response. If not specified, we
      // default to the most common error code across all returned errors, which
      // is likely good enough 99% of the time.
      let status =
        mainStatus
        |> Option.defaultValue (
             statusesAndErrors
             |> List.countBy fst
             |> List.maxBy snd
             |> fst)

      // Write the response
      (setStatusCode status >=> jsonApi (ErrorDocument.ofErrors errors)) next ctx


  /// Responds with the specified single error.
  let handleError err : HttpHandler =
    err |> getStatusAndError |> List.singleton |> doHandleErrors None


  /// Responds with the specified list of errors.
  let handleErrors errs : HttpHandler =
    errs |> List.distinct |> List.map getStatusAndError |> doHandleErrors None


  /// Responds with the specified list of errors, using the specified status
  /// code for the response.
  let handleErrorsWithMainStatus mainStatus errs : HttpHandler =
    errs |> List.distinct |> List.map getStatusAndError |> doHandleErrors (Some mainStatus)


  /// Calls the specified Ok handler or returns the specified errors.
  let handleResult next ctx (res: Result<HttpHandler, ApiError list>) : HttpFuncResult =
      match res with
      | Ok handler -> handler next ctx
      | Error errs -> handleErrors errs next ctx


  /// Calls the specified Ok handler or returns the specified errors.
  let handleAsyncResult next ctx (asyncRes: Async<Result<HttpHandler, ApiError list>>) : HttpFuncResult =
    task {
      let! res = asyncRes
      return! handleResult next ctx res
    }


  let unhandledExceptionHandler (ex : Exception) =
    Log.Error(ex, "Unhandled exception while executing request")
    handleError UnknownError


// This module contains functions to find resources by their API ID.
module Find =

  /// Finds a person by its API ID, returning ResourceNotFound if not found.
  let person apiUserId =
    asyncResult {
      let notFoundErr = [ResourceNotFound (TypeNames.person, apiUserId)]
      let! personId = PersonId.fromApi apiUserId |> Result.requireSome notFoundErr
      return! Db.Person.byId personId |> AsyncResult.requireSome notFoundErr
    }

  /// Finds an article by its API ID, returning ResourceNotFound if not found.
  let article apiArticleId =
    asyncResult {
      let notFoundErr = [ResourceNotFound (TypeNames.article, apiArticleId)]
      let! articleId = ArticleId.fromApi apiArticleId |> Result.requireSome notFoundErr
      return! Db.Article.byId articleId |> AsyncResult.requireSome notFoundErr
    }

  /// Finds a comment by its API ID, returning ResourceNotFound if not found.
  let comment apiCommentId =
    asyncResult {
      let notFoundErr = [ResourceNotFound (TypeNames.comment, apiCommentId)]
      let! commentId = CommentId.fromApi apiCommentId |> Result.requireSome notFoundErr
      return! Db.Comment.byId commentId |> AsyncResult.requireSome notFoundErr
    }


/// An HTTP handler that uses the specified function to find an entity by its
/// API ID, returning ResourceNotFound if not found. This is used in Routes.fs.
let find findFun (apiId: string) (routeHandler: 'entity -> HttpHandler) : HttpHandler =
  fun (next: HttpFunc) (ctx: HttpContext) ->
    findFun apiId |> AsyncResult.map routeHandler |> handleAsyncResult next ctx



/// In the modules below, we make use of applicative error handling using
/// FsToolkit.ErrorHandling's validation operators. This means that we collect
/// and return errors from multiple calls. We could also do it monadically,
/// using let! for each parameter. If so, we could only return errors from a
/// single let! call at a time.


module Person =

  // An excellent example of applicative error handling. A function to parse
  // search arguments from query parameters, returning at list of errors from
  // all parsed query parameters if any fail.

  let private parseSearchArgs (ctx: HttpContext) =
    PersonSearchArgs.create
    <!> Query.GetSingle(nameof <@ any<PersonAttrs>.firstName @> |> wrapFilter, ctx)
    <*> Query.GetSingle(nameof <@ any<PersonAttrs>.lastName @> |> wrapFilter, ctx)
    <*> Query.GetSingle(nameof <@ any<PersonAttrs>.twitter @> |> wrapFilter, ctx)
    // Note the use of Gender.fromApiMap. If we get a value not in the map, the
    // returned error will contain all allowed values. Nice! Had we used a
    // function to parse instead of a map, we'd have to supply a useful error
    // message ourselves in order to get the allowed values (and remember to
    // update it when adding new allowed values).
    <*> Query.GetList(nameof <@ any<PersonAttrs>.gender @>, Gender.fromApiMap, ctx)
    |> Result.mapError (List.map queryError)


  // Our first "normal" HttpHandler! Note that it's implemented using
  // asyncResult {} and ends with a call to handleAsyncResult. See earlier
  // comment. (Again, this is not strictly related to FSharp.JsonApi.)

  let search : HttpHandler =
    fun (next : HttpFunc) (ctx : HttpContext) ->
      asyncResult {
        let! searchArgs = parseSearchArgs ctx
        let! persons = Db.Person.search searchArgs
        let! resDoc =
          jsonApiCtx.BuildDocument(
            persons, Person.getBuilder ctx.Request.BaseUrl, ctx)
        return jsonApi resDoc
      }
      |> handleAsyncResult next ctx

  // Look at Routes.fs if you think it's weird that this and similar handlers
  // accept a full domain object, and not just an ID. At this point in the
  // pipeline, the person is already found (or a ResourceNotFound error
  // returned) using the find handler and the functions in the Find module. The
  // only thing left to do here is create and return the JSON-API document for
  // the person.
  let get person : HttpHandler =
    fun (next : HttpFunc) (ctx : HttpContext) ->
      asyncResult {
        let! resDoc =
          jsonApiCtx.BuildDocument(
            person, Person.getBuilder ctx.Request.BaseUrl, ctx)
        return jsonApi resDoc
      }
      |> handleAsyncResult next ctx

  let create : HttpHandler =
    fun (next : HttpFunc) (ctx : HttpContext) ->
      asyncResult {

        // Below we parse the requesty body as a Person resource. We use
        // ParseSimple, which returns a SimpleResource, which is a simplified
        // and "flattened" view or a resource where you are guaranteed to have
        // access to the attribute and relationship types you have defined. In
        // the attribute/relationship types, all members will be Skip if the
        // request document did not have /data, /data/attributes or
        // /data/relationships. Often this is sufficient, because you only care
        // about whether the attribute/relationship is there or not, not which
        // "level" is was missing at.

        let! res =
          jsonApiCtx
            .WithNoIdForPost()  // We don't support client-generated IDs
            .ParseSimple(Person, ctx)
          |> AsyncResult.mapError (List.map docError)

        let a = SimpleResource.attributes res

        // Again, applicative error handling, but this time to parse attributes.

        let! person =
          Person.create
          <!> Attribute.Require(nameof <@ a.firstName @>, a.firstName)
          <*> Attribute.Require(nameof <@ a.lastName @>, a.lastName)
          <*> Attribute.Get(a.twitter)
          <*> Attribute.Get(nameof <@ a.gender @>, a.gender, Gender.fromApiMap)
          |> Result.mapError (List.map docError)

        // One could argue that the responsibility of remembering to save
        // new/updated domain entities should be elsewhere, but let's not
        // over-complicate such a simple app.
        do! Db.Person.save person

        let! resDoc =
          jsonApiCtx.BuildDocument(
            person, Person.getBuilder ctx.Request.BaseUrl, ctx)
        return
          setStatusCode 201
          >=> setLocationHeaderFromMainSelfUrl resDoc
          >=> jsonApi resDoc
      }
      |> handleAsyncResult next ctx


  let update (person: Person) : HttpHandler =
    fun (next : HttpFunc) (ctx : HttpContext) ->
      asyncResult {
        let! res =
          jsonApiCtx
            .WithIdForPatch(person.Id |> PersonId.toApi)
            .ParseSimple(Person, ctx)
          |> AsyncResult.mapError (List.map docError)

        let a = SimpleResource.attributes res

        let! person =
          Person.update
          <!> Attribute.Get(a.firstName)
          <*> Attribute.Get(a.lastName)
          <*> Attribute.Get(a.twitter)
          <*> Attribute.Get(nameof <@ a.gender @>, a.gender, Gender.fromApiMap)
          <*> Ok person
          |> Result.mapError (List.map docError)

        do! Db.Person.save person

        let! resDoc =
          jsonApiCtx.BuildDocument(
            person, Person.getBuilder ctx.Request.BaseUrl, ctx)
        return jsonApi resDoc
      }
      |> handleAsyncResult next ctx


  let delete (person: Person) : HttpHandler =
    fun (next : HttpFunc) (ctx : HttpContext) ->
      asyncResult {
        do! Db.Person.delete person.Id
        return setStatusCode 204
      }
      |> handleAsyncResult next ctx


  let articles (person: Person) : HttpHandler =
    fun (next : HttpFunc) (ctx : HttpContext) ->
      asyncResult {
        let! articles = Db.Article.allByAuthor person.Id
        let! resDoc =
          jsonApiCtx.BuildDocument(
            articles, Article.getBuilder ctx.Request.BaseUrl, ctx)
        return jsonApi resDoc
      }
      |> handleAsyncResult next ctx



module Article =

  let private parseSearchArgs (ctx: HttpContext) =
    // You can create whichever query parameter names you want, in whatever
    // manner you want. The JSON-API spec is unopinionated about things like
    // "filter operators" ([ge] and [le] below), and FSharp.JsonApi just wants a
    // query parameter name to look for.
    let q_createdAfter = sprintf "filter[%s][ge]" (nameof <@ any<ArticleAttrs>.created @>)
    let q_createdBefore = sprintf "filter[%s][le]" (nameof <@ any<ArticleAttrs>.created @>)
    ArticleSearchArgs.create
    <!> Query.GetSingle(nameof <@ any<ArticleAttrs>.title @> |> wrapFilter, ctx)
    <*> Query.GetList(nameof <@ any<ArticleAttrs>.``type`` @> |> wrapFilter, ArticleType.fromApiMap, ctx)
    <*> Query.GetSingle(q_createdAfter, parseDateTimeOffset, ctx)
    <*> Query.GetSingle(q_createdBefore, parseDateTimeOffset, ctx)
    |> Result.mapError (List.map queryError)


  let search : HttpHandler =
    fun (next : HttpFunc) (ctx : HttpContext) ->
      asyncResult {
        let! searchArgs = parseSearchArgs ctx
        let! articles = Db.Article.search searchArgs
        let! resDoc =
          jsonApiCtx.BuildDocument(
            articles, Article.getBuilder ctx.Request.BaseUrl, ctx)
        return jsonApi resDoc
      }
      |> handleAsyncResult next ctx


  let get article : HttpHandler =
    fun (next : HttpFunc) (ctx : HttpContext) ->
      asyncResult {
        let! resDoc =
          jsonApiCtx.BuildDocument(
            article, Article.getBuilder ctx.Request.BaseUrl, ctx)
        return jsonApi resDoc
      }
      |> handleAsyncResult next ctx

  let create : HttpHandler =
    fun (next : HttpFunc) (ctx : HttpContext) ->
      asyncResult {

        // F# type inference and overload resolution isn't perfect, particularly
        // with overloaded computation expressions like asyncResult, so
        // sometimes it's necessary to add type annotations even though it
        // shouldn't be necessary (https://github.com/dotnet/fsharp/issues/4472)
        let! (res: SimpleResource<ArticleAttrs, ArticleRels>) =
          jsonApiCtx
            .WithNoIdForPost()
            .ParseSimple(Article, ctx)
          |> AsyncResult.mapError (List.map docError)

        let a = SimpleResource.attributes res
        let r = SimpleResource.relationships res

        // There are also some helpers for parsing relationships, similar to
        // attributes.
        let! (author: Person) =
          Relationship.RequireNonNull(nameof <@ r.author @>, r.author, PersonId.fromApi, Db.Person.byId)
          |> AsyncResult.mapError (List.map docError)

        let! article =
          Article.create author.Id
          <!> Attribute.Require(nameof <@ a.title @>, a.title)
          <*> Attribute.Require(nameof <@ a.body @>, a.body)
          <*> Attribute.Get(nameof <@ a.``type`` @>, a.``type``, ArticleType.fromApiMap)
          |> Result.mapError (List.map docError)

        do! Db.Article.save article

        let! resDoc =
          jsonApiCtx.BuildDocument(
            article, Article.getBuilder ctx.Request.BaseUrl, ctx)
        return
          setStatusCode 201
          >=> setLocationHeaderFromMainSelfUrl resDoc
          >=> jsonApi resDoc
      }
      |> handleAsyncResult next ctx


  let update (article: Article) : HttpHandler =
    fun (next : HttpFunc) (ctx : HttpContext) ->
      asyncResult {
        let! res =
          jsonApiCtx
            .WithIdForPatch(article.Id |> ArticleId.toApi)
            .ParseSimple(Article, ctx)
          |> AsyncResult.mapError (List.map docError)

        let a = SimpleResource.attributes res

        let! article =
          Article.update
          <!> Attribute.Get(a.title)
          <*> Attribute.Get(a.body)
          <*> Attribute.Get(nameof <@ a.``type`` @>, a.``type``, ArticleType.fromApiMap)
          <*> Ok article
          |> Result.mapError (List.map docError)

        let! resDoc =
          jsonApiCtx.BuildDocument(
            article, Article.getBuilder ctx.Request.BaseUrl, ctx)
        return jsonApi resDoc
      }
      |> handleAsyncResult next ctx


  let delete (article: Article) : HttpHandler =
    fun (next : HttpFunc) (ctx : HttpContext) ->
      asyncResult {
        do! Db.Article.delete article.Id
        return setStatusCode 204
      }
      |> handleAsyncResult next ctx


  let author (article: Article) : HttpHandler =
    fun (next : HttpFunc) (ctx : HttpContext) ->
      asyncResult {
        let! author = Db.Person.authorForArticle article.Id
        let! resDoc =
          jsonApiCtx.BuildDocument(
            author, Person.getBuilder ctx.Request.BaseUrl, ctx)
        return jsonApi resDoc
      }
      |> handleAsyncResult next ctx


  let comments (article: Article) : HttpHandler =
    fun (next : HttpFunc) (ctx : HttpContext) ->
      asyncResult {
        let! comments = Db.Comment.allForArticle article.Id
        let! resDoc =
          jsonApiCtx.BuildDocument(
            comments, Comment.getBuilder ctx.Request.BaseUrl, ctx)
        return jsonApi resDoc
      }
      |> handleAsyncResult next ctx



module Comment =

  let private parseSearchArgs (ctx: HttpContext) =
    // Again, the JSON-API spec is unopinionated about filter specifics, such as
    // filtering on related resource attributes. The below is just an intuitive
    // way of naming the parameter.
    let q_authorFirstName =
      sprintf "filter[%s.%s]"
        (nameof <@ any<CommentRels>.author @>)
        (nameof <@ any<PersonAttrs>.firstName @>)
    CommentSearchArgs.create
    <!> Query.GetSingle(nameof <@ any<CommentRels>.author @> |> wrapFilter, PersonId.fromApi |> withInvalidTypeMsg "person ID", ctx)
    <*> Query.GetSingle(q_authorFirstName, ctx)
    |> Result.mapError (List.map queryError)


  let search : HttpHandler =
    fun (next : HttpFunc) (ctx : HttpContext) ->
      asyncResult {
        let! searchArgs = parseSearchArgs ctx
        let! comments = Db.Comment.search searchArgs
        let! resDoc =
          jsonApiCtx.BuildDocument(
            comments, Comment.getBuilder ctx.Request.BaseUrl, ctx)
        return jsonApi resDoc
      }
      |> handleAsyncResult next ctx


  let get comment : HttpHandler =
    fun (next : HttpFunc) (ctx : HttpContext) ->
      asyncResult {
        let! resDoc =
          jsonApiCtx.BuildDocument(
            comment, Comment.getBuilder ctx.Request.BaseUrl, ctx)
        return jsonApi resDoc
      }
      |> handleAsyncResult next ctx


  let create : HttpHandler =
    fun (next : HttpFunc) (ctx : HttpContext) ->
      asyncResult {
        let! res =
          jsonApiCtx
            .WithNoIdForPost()
            .WithoutReadOnly(TypeNames.comment, nameof <@ any<CommentRels>.author @>)
            .ParseSimple(Comment, ctx)
          |> AsyncResult.mapError (List.map docError)

        let a = SimpleResource.attributes res
        let r = SimpleResource.relationships res

        let! (author: Person) =
          Relationship.RequireNonNull(nameof <@ r.author @>, r.author, PersonId.fromApi, Db.Person.byId)
          |> AsyncResult.mapError (List.map docError)

        let! (article: Article) =
          Relationship.RequireNonNull(nameof <@ r.article @>, r.article, ArticleId.fromApi, Db.Article.byId)
          |> AsyncResult.mapError (List.map docError)

        let! comment =
          Comment.create author.Id article.Id
          <!> Attribute.Require(nameof <@ a.body @>, a.body)
          |> Result.mapError (List.map docError)

        do! Db.Comment.save comment

        let! resDoc =
          jsonApiCtx.BuildDocument(
            comment, Comment.getBuilder ctx.Request.BaseUrl, ctx)
        return
          setStatusCode 201
          >=> setLocationHeaderFromMainSelfUrl resDoc
          >=> jsonApi resDoc
      }
      |> handleAsyncResult next ctx


  let update (comment: Comment) : HttpHandler =
    fun (next : HttpFunc) (ctx : HttpContext) ->
      asyncResult {
        let! res =
          jsonApiCtx
            .WithIdForPatch(comment.Id |> CommentId.toApi)
            .ParseSimple(Comment, ctx)
          |> AsyncResult.mapError (List.map docError)

        let a = SimpleResource.attributes res

        let! comment =
          Comment.update
          <!> Attribute.Get(a.body)
          <*> Ok comment
          |> Result.mapError (List.map docError)

        let! resDoc =
          jsonApiCtx.BuildDocument(
            comment, Comment.getBuilder ctx.Request.BaseUrl, ctx)
        return jsonApi resDoc
      }
      |> handleAsyncResult next ctx


  let delete (comment: Comment) : HttpHandler =
    fun (next : HttpFunc) (ctx : HttpContext) ->
      asyncResult {
        do! Db.Comment.delete comment.Id
        return setStatusCode 204
      }
      |> handleAsyncResult next ctx


  let author (comment: Comment) : HttpHandler =
    fun (next : HttpFunc) (ctx : HttpContext) ->
      asyncResult {
        let! author = Db.Person.authorForComment comment.Id
        let! resDoc =
          jsonApiCtx.BuildDocument(
            author, Person.getBuilder ctx.Request.BaseUrl, ctx)
        return jsonApi resDoc
      }
      |> handleAsyncResult next ctx


  let article (comment: Comment) : HttpHandler =
    fun (next : HttpFunc) (ctx : HttpContext) ->
      asyncResult {
        let! author = Db.Article.forComment comment.Id
        let! resDoc =
          jsonApiCtx.BuildDocument(
            author, Article.getBuilder ctx.Request.BaseUrl, ctx)
        return jsonApi resDoc
      }
      |> handleAsyncResult next ctx