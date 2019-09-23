module Domain

open System

(*
This module contains all domain entities and logic. It has nothing directly
to do with FSharp.JsonApi.

In a large, non-trivial API, you might want to place everything not directly
API-related (i.e. Domain.fs and Db.fs in this case) in a separate project.
How you organize stuff is entirely up to you.

In the create/update functions defined here, the parameters are all wrapped
in option, and properties that are already wrapped in option will be doubly
option-wrapped. The outer option is whether to set/update that property at
all, and the inner option (if present) is part of the actual property value
(e.g. Person.Twitter - it can be specifically set to None, or not changed).
*)


type PersonId = PersonId of Guid


type Gender =
  | Male
  | Female
  | Other


type Person = {
  Id: PersonId
  FirstName: string
  LastName: string
  Twitter: string option
  Gender: Gender option
}


[<RequireQualifiedAccess>]
type PersonSort =
  | FirstName
  | LastName


// Arguments used for searching for persons (in GET /persons)
type PersonSearchArgs = {
  FirstName: string option
  LastName: string option
  Twitter: string option
  Genders: Gender list option
  SortBy: PersonSort
  SortDescending: bool
  Offset: int
  Limit: int
}


module Person =

  let create firstName lastName = {
    Id = Guid.NewGuid () |> PersonId
    FirstName = firstName
    LastName = lastName
    Twitter = None
    Gender = None
  }

  let setFirstName firstName (person: Person) =
    { person with FirstName = firstName }

  let setLastName lastName (person: Person) =
    { person with LastName = lastName }

  let setTwitter twitter (person: Person) =
    { person with Twitter = twitter }

  let setGender gender (person: Person) =
    { person with Gender = gender }


module PersonSearchArgs =

  let create (sortBy, sortDesc) offset limit = {
    FirstName = None
    LastName = None
    Twitter = None
    Genders = None
    SortBy = sortBy
    SortDescending = sortDesc
    Offset = offset
    Limit = limit
  }

  let setFirstName firstName (args: PersonSearchArgs) =
    { args with FirstName = Some firstName }

  let setLastName lastName (args: PersonSearchArgs) =
    { args with LastName = Some lastName }

  let setTwitter twitter (args: PersonSearchArgs) =
    { args with Twitter = Some twitter }

  let setGenders genders (args: PersonSearchArgs) =
    { args with Genders = Some genders }


type ArticleId = ArticleId of Guid


type ArticleType =
  | Personal
  | Commercial


type Article = {
  Id: ArticleId
  AuthorId: PersonId
  Title: string
  Body: string
  Type: ArticleType
  Created: DateTimeOffset
  Updated: DateTimeOffset option
}


[<RequireQualifiedAccess>]
type ArticleSort =
  | Title
  | Created


// Arguments used for searching for articles (in GET /articles)
type ArticleSearchArgs = {
  Title: string option
  Types: ArticleType list option
  CreatedAfter: DateTimeOffset option
  CreatedBefore: DateTimeOffset option
  SortBy: ArticleSort
  SortDescending: bool
  Offset: int
  Limit: int
}


module Article =

  let create authorId title body = {
    Id = Guid.NewGuid () |> ArticleId
    AuthorId = authorId
    Title = title
    Body = body
    Type = Personal
    Created = DateTimeOffset.Now
    Updated = None
  }

  let setAuthor authorId (article: Article) =
    { article with AuthorId = authorId }

  let setTitle title (article: Article) =
    { article with Title = title }

  let setBody body (article: Article) =
    { article with Body = body }

  let setArticleType articleType (article: Article) =
    { article with Type = articleType }

  let setUpdated updatedAt (article: Article) =
    { article with Updated = updatedAt }


module ArticleSearchArgs =

  let create (sortBy, sortDesc) offset limit = {
    Title = None
    Types = None
    CreatedAfter = None
    CreatedBefore = None
    SortBy = sortBy
    SortDescending = sortDesc
    Offset = offset
    Limit = limit
  }

  let setTitle title (args: ArticleSearchArgs) =
    { args with Title = Some title }

  let setTypes types (args: ArticleSearchArgs) =
    { args with Types = Some types }

  let setCreatedAfter createdAfter (args: ArticleSearchArgs) =
    { args with CreatedAfter = Some createdAfter }

  let setCreatedBefore createdBefore (args: ArticleSearchArgs) =
    { args with CreatedBefore = Some createdBefore }


type CommentId = CommentId of Guid


type Comment = {
  Id: CommentId
  AuthorId: PersonId
  ArticleId: ArticleId
  Body: string
  Created: DateTimeOffset
  Updated: DateTimeOffset option
}


[<RequireQualifiedAccess>]
type CommentSort =
  | Created


// Arguments used for searching for comments (in GET /comments)
type CommentSearchArgs = {
  Author: PersonId option
  AuthorFirstName: string option
  SortBy: CommentSort
  SortDescending: bool
  Offset: int
  Limit: int
}


module Comment =

  let create authorId articleId body = {
    Id = Guid.NewGuid () |> CommentId
    AuthorId = authorId
    ArticleId = articleId
    Body = body
    Created = DateTimeOffset.Now
    Updated = None
  }

  let setBody body (comment: Comment) =
    { comment with Body = body }

  let setUpdated updatedAt (comment: Comment) =
    { comment with Updated = updatedAt }


module CommentSearchArgs =

  let create (sortBy, sortDesc) offset limit = {
    Author = None
    AuthorFirstName = None
    SortBy = sortBy
    SortDescending = sortDesc
    Offset = offset
    Limit = limit
  }

  let setAuthorId authorId (args: CommentSearchArgs) =
    { args with Author = Some authorId }

  let setAuthorFirstName authorFirstName (args: CommentSearchArgs) =
    { args with AuthorFirstName = Some authorFirstName }
