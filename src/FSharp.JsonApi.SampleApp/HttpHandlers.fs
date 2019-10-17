module HttpHandlers

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


  // Instantiate FSharp.JsonApi's helper for chaining setters with applicative
  // error handling.
  let set = Setter(docError, queryError)


  type HttpRequest with

    member this.SafeUrl =
      this.GetEncodedUrl () |> Uri

    /// The API's base URL.
    // (In a production environment with reverse proxies and whatnot, you'd
    // might want to hardcode this to a known good value.)
    member this.BaseUrl =
      Uri(this.SafeUrl.GetLeftPart(UriPartial.Authority))


  // FSharp.JsonApi.Giraffe has several HttpHandlers for writing JSON-API
  // response documents. Here we wrap such a handler, with two goals:
  //  1. Bake in the jsonApiCtx parameter so we don't have to write that
  //     everywhere
  //  2. Decide once which handler to use (e.g. here we are using jsonApiETag to
  //     support ETag generation/checking for all responses).
  //  3. Validate the response and log any errors. If we return invalid response
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
        return! jsonApiETag jsonApiCtx doc next ctx
      }


  // This is just an example of how to do logging of error responses. At the
  // very least, the Error.Id should be logged so you can look it up when an API
  // client comes to you with an error response they got. I use the code below
  // in production for several APIs.
  let private doHandleErrors mainStatus (statusesAndErrors: (int * Error) list) : HttpHandler =
    fun (next : HttpFunc) (ctx : HttpContext) ->
      match statusesAndErrors with
      | [] ->
          // Should never happen. Since an error response without errors is
          // invalid according to the JSON-API specification, throw an exception
          // so a general 500 error is returned.
          failwith "Error handler got empty error list"
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

      // Set each error's Status property based on the corresponding numeric
      // status code.
      let errors =
        statusesAndErrors
        |> List.map (fun (status, err) -> err |> Error.setStatus status)

      // Get the status code for the whole response. If not specified, we
      // default to the most common error code across all returned errors, which
      // is probably good enough 99% of the time.
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
  /// code for the whole response.
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
      let notFoundErr = [ResourceNotFound]
      let! personId = PersonId.fromApi apiUserId |> Result.requireSome notFoundErr
      return! Db.Person.byId personId |> AsyncResult.requireSome notFoundErr
    }

  /// Finds an article by its API ID, returning ResourceNotFound if not found.
  let article apiArticleId =
    asyncResult {
      let notFoundErr = [ResourceNotFound]
      let! articleId = ArticleId.fromApi apiArticleId |> Result.requireSome notFoundErr
      return! Db.Article.byId articleId |> AsyncResult.requireSome notFoundErr
    }

  /// Finds a comment by its API ID, returning ResourceNotFound if not found.
  let comment apiCommentId =
    asyncResult {
      let notFoundErr = [ResourceNotFound]
      let! commentId = CommentId.fromApi apiCommentId |> Result.requireSome notFoundErr
      return! Db.Comment.byId commentId |> AsyncResult.requireSome notFoundErr
    }


/// An HTTP handler that uses the specified function to find an entity by its
/// API ID, returning ResourceNotFound if not found. This is used in Routes.fs.
let find findFun (apiId: string) (routeHandler: 'entity -> HttpHandler) : HttpHandler =
  fun (next: HttpFunc) (ctx: HttpContext) ->
    findFun apiId |> AsyncResult.map routeHandler |> handleAsyncResult next ctx



/// In the modules below, we make use of applicative error handling using
/// FsToolkit.ErrorHandling's validation operators and FSharp.JsonApi's Setter
/// type. "Applicative error handling" basically means that we collect and
/// return errors from multiple calls. We could also do it monadically, using
/// let! for each parameter. If so, we could only return errors from a single
/// let! call at a time.


module Person =

  // An excellent example of applicative error handling. A function to parse
  // search arguments from query parameters, returning at list of errors from
  // all parsed query parameters if any fail.
  let private parseSearchArgs (ctx: HttpContext) =

    let sortMap = Map.ofList [
      nameof <@ any<PersonAttrs>.firstName @>, PersonSort.FirstName
      nameof <@ any<PersonAttrs>.lastName @>, PersonSort.LastName
    ]

    let q = ctx.QueryParser

    // We use FsToolkit.ErrorHandling's operators for the parameters to
    // PersonSearchArgs.create. Then we map to the correct error type, and
    // continue chaining "setters" for the type we're building.
    PersonSearchArgs.create
    <!> q.GetSortSingle(sortMap, (PersonSort.FirstName, false))
    <*> q.GetDefaultBoundInt("page[offset]", 0, min=0)
    <*> q.GetDefaultBoundInt("page[limit]", 10, min=1)
    |> Result.mapError (List.map queryError)
    |> set.Optional(PersonSearchArgs.setFirstName, q.GetSingle(nameof <@ any<PersonAttrs>.firstName @> |> wrapFilter))
    |> set.Optional(PersonSearchArgs.setLastName, q.GetSingle(nameof <@ any<PersonAttrs>.lastName @> |> wrapFilter))
    |> set.Optional(PersonSearchArgs.setTwitter, q.GetSingle(nameof <@ any<PersonAttrs>.twitter @> |> wrapFilter))
    // Note the use of Gender.fromApiMap below. If we get a value not in the
    // map, the returned error will contain all allowed values. Had we used a
    // function to parse instead of a map, we'd have to supply a useful error
    // message ourselves in order to get the allowed values (and remembered to
    // update it when adding new allowed values).
    |> set.Optional(PersonSearchArgs.setGenders, q.GetList(nameof <@ any<PersonAttrs>.gender @>, Gender.fromApiMap))


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

        // Below we parse the requesty body as a Person resource.

        let! res =
          jsonApiCtx
            .WithNoIdForPost()  // We don't support client-generated IDs
            .ParseRequired(Person, ctx)
          |> AsyncResult.mapError (List.map docError)

        // Resource.attributesOrDefault returns the attributes if present, or a
        // default attributes instance if not. This presents a simplified and
        // "flattened" view of the attributes where you are guaranteed to have
        // access to the attribute and relationship types you have defined, and
        // all members will be Skip if the request document did not have /data,
        // /data/attributes or /data/relationships. Often this is sufficient,
        // because you only care about whether the attribute/relationship is
        // there or not, not which "level" is was missing at.
        let a = Resource.attributesOrDefault res

        // Again, applicative error handling, but this time to parse attributes.
        // Note again that we use the operators for the parameters to
        // Person.create, and then chain additional setters using the Setter
        // instance.
        let! person =
          Person.create
          <!> Attribute.Require(nameof <@ a.firstName @>, a.firstName)
          <*> Attribute.Require(nameof <@ a.lastName @>, a.lastName)
          |> Result.mapError (List.map docError)
          |> set.Optional(Person.setTwitter, Attribute.Get(a.twitter))
          |> set.Optional(Person.setGender, Attribute.Get(nameof <@ a.gender @>, a.gender, Gender.fromApiMap))

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
            .ParseRequired(Person, ctx)
          |> AsyncResult.mapError (List.map docError)

        let a = Resource.attributesOrDefault res

        let! person =
          Ok person
          |> set.Optional(Person.setFirstName, Attribute.Get(a.firstName))
          |> set.Optional(Person.setLastName, Attribute.Get(a.lastName))
          |> set.Optional(Person.setTwitter, Attribute.Get(a.twitter))
          |> set.Optional(Person.setGender, Attribute.Get(nameof <@ a.gender @>, a.gender, Gender.fromApiMap))

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

    let sortMap = Map.ofList [
      nameof <@ any<ArticleAttrs>.title @>, ArticleSort.Title
      nameof <@ any<ArticleAttrs>.created @>, ArticleSort.Created
    ]

    let q = ctx.QueryParser

    // You can create whichever query parameter names you want, in whatever
    // manner you want. The JSON-API spec is unopinionated about things like
    // "filter operators" (the [ge] and [le] suffixes below), and the below is
    // just an example of how the parameters might be named. FSharp.JsonApi has
    // no notion of "filter operators" and just needs a query parameter name.
    let createdAfter = sprintf "filter[%s][ge]" (nameof <@ any<ArticleAttrs>.created @>)
    let createdBefore = sprintf "filter[%s][le]" (nameof <@ any<ArticleAttrs>.created @>)
    ArticleSearchArgs.create
    <!> q.GetSortSingle(sortMap, (ArticleSort.Created, true))
    <*> q.GetDefaultBoundInt("page[offset]", 0, min=0)
    <*> q.GetDefaultBoundInt("page[limit]", 10, min=1)
    |> Result.mapError (List.map queryError)
    |> set.Optional(ArticleSearchArgs.setTitle, q.GetSingle(nameof <@ any<ArticleAttrs>.title @> |> wrapFilter))
    |> set.Optional(ArticleSearchArgs.setTypes, q.GetList(nameof <@ any<ArticleAttrs>.articleType @> |> wrapFilter, ArticleType.fromApiMap))
    |> set.Optional(ArticleSearchArgs.setCreatedAfter, q.GetSingle(createdAfter, parseDateTimeOffset))
    |> set.Optional(ArticleSearchArgs.setCreatedBefore, q.GetSingle(createdBefore, parseDateTimeOffset))


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

        let! res =
          jsonApiCtx
            .WithNoIdForPost()
            .ParseRequired(Article, ctx)
          |> AsyncResult.mapError (List.map docError)

        // F# type inference and overload resolution isn't perfect, particularly
        // with overloaded computation expressions like asyncResult, so
        // sometimes it's necessary to add type annotations even though it
        // shouldn't be necessary (https://github.com/dotnet/fsharp/issues/4472).
        // Note that the type annotation can also be added to res above, but
        // that will generally be more verbose, particularly with the
        // higher-arity parsing overloads.
        let (a: ArticleAttrs) = Resource.attributesOrDefault res
        let (r: ArticleRels) = Resource.relationshipsOrDefault res

        // There are also some helpers for parsing relationships, similar to
        // attributes.
        let! (author: Person) =
          Relationship.RequireNonNull(nameof <@ r.author @>, r.author, PersonId.fromApi, Db.Person.byId)
          |> AsyncResult.mapError (List.map docError)

        // Since the relationship parsers may be async, they are more difficult
        // to use directly along with the rest of the applicative parsing
        // (below). Making a good library for mixed sync/async applicative
        // parsing parsing would probably be stretching the F# type system, and
        // requiring some monadic parsing is a good tradeoff. After all,
        // returning multiple errors through the API is generally just a
        // (hopefully useful) courtesy to API client developers, not a critical
        // requirement. There's nothing stopping you from creating a JSON-API
        // API that only ever returns a single error.

        let! article =
          Article.create author.Id
          <!> Attribute.Require(nameof <@ a.title @>, a.title)
          <*> Attribute.Require(nameof <@ a.body @>, a.body)
          |> Result.mapError (List.map docError)
          |> set.Optional(Article.setArticleType, Attribute.Get(nameof <@ a.articleType @>, a.articleType, ArticleType.fromApiMap))

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
            .ParseRequired(Article, ctx)
          |> AsyncResult.mapError (List.map docError)

        let (a: ArticleAttrs) = Resource.attributesOrDefault res
        let (r: ArticleRels) = Resource.relationshipsOrDefault res

        let! (author: Person option) =
          Relationship.GetNonNull(nameof <@ r.author @>, r.author, PersonId.fromApi, Db.Person.byId)
          |> AsyncResult.mapError (List.map docError)

        let! article =
          Ok article
          |> set.Optional(Article.setAuthor, author |> Option.map (fun a -> a.Id))
          |> set.Optional(Article.setTitle, Attribute.Get(a.title))
          |> set.Optional(Article.setBody, Attribute.Get(a.body))
          |> set.Optional(Article.setArticleType, Attribute.Get(nameof <@ a.articleType @>, a.articleType, ArticleType.fromApiMap))
          |> set.Required(Article.setUpdated, Some DateTimeOffset.Now)

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

    let sortMap = Map.ofList [
      nameof <@ any<CommentAttrs>.created @>, CommentSort.Created
    ]

    let q = ctx.QueryParser

    // Again, the JSON-API spec is unopinionated about filter specifics, such as
    // filtering on related resource attributes. The below is just an example of
    // how to name filters for attributes on related resources.
    let authorFirstName =
      sprintf "filter[%s.%s]"
        (nameof <@ any<CommentRels>.author @>)
        (nameof <@ any<PersonAttrs>.firstName @>)
    CommentSearchArgs.create
    <!> q.GetSortSingle(sortMap, (CommentSort.Created, true))
    <*> q.GetDefaultBoundInt("page[offset]", 0, min=0)
    <*> q.GetDefaultBoundInt("page[limit]", 10, min=1)
    |> Result.mapError (List.map queryError)
    |> set.Optional(
        CommentSearchArgs.setAuthorId,
        q.GetSingle(
          nameof <@ any<CommentRels>.author @> |> wrapFilter,
          PersonId.fromApi |> withInvalidTypeMsg "person ID"))
    |> set.Optional(CommentSearchArgs.setAuthorFirstName, q.GetSingle(authorFirstName))


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
            .ParseRequired(Comment, ctx)
          |> AsyncResult.mapError (List.map docError)

        let a = Resource.attributesOrDefault res
        let r = Resource.relationshipsOrDefault res

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
            .ParseRequired(Comment, ctx)
          |> AsyncResult.mapError (List.map docError)

        let a = Resource.attributesOrDefault res

        let! comment =
          Ok comment
          |> set.Optional(Comment.setBody, Attribute.Get(a.body))
          |> set.Required(Comment.setUpdated, Some DateTimeOffset.Now)

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
