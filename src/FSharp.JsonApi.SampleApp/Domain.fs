﻿module Domain

open System

// This module contains all domain entities and logic. In a large, non-trivial
// API, you might want to place everything not directly API-related (i.e.
// Domain.fs and Db.fs in this case) in a separate project. How you organize
// stuff is entirely up to you.

// In the create/update functions defined here, the parameters are all wrapped
// in option, and properties that are already wrapped in option will be doubly
// option-wrapped. The outer option is whether to set/update that property at
// all, and the inner option (if present) is part of the actual property value
// (e.g. Person.Twitter - it can be specifically set to None, or not changed).


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
  Sort: PersonSort
  SortDescending: bool
}


module Person =

  let create firstName lastName twitter gender = {
    Id = Guid.NewGuid () |> PersonId
    FirstName = firstName
    LastName = lastName
    Twitter = twitter |> Option.defaultValue None
    Gender = gender |> Option.defaultValue None
  }

  let update firstName lastName twitter gender person = {
    person with
      FirstName = firstName |> Option.defaultValue person.FirstName
      LastName = lastName |> Option.defaultValue person.LastName
      Twitter = twitter |> Option.defaultValue person.Twitter
      Gender = gender |> Option.defaultValue person.Gender
  }


module PersonSearchArgs =

  open FSharp.JsonApi

  let create firstName lastName twitter genders sort = {
    FirstName = firstName
    LastName = lastName
    Twitter = twitter
    Genders = genders
    Sort = fst sort
    SortDescending = snd sort = SortDir.Descending
  }


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
  Sort: ArticleSort
  SortDescending: bool
}


module Article =

  let create authorId title body articleType = {
    Id = Guid.NewGuid () |> ArticleId
    AuthorId = authorId
    Title = title
    Body = body
    Type = articleType |> Option.defaultValue Personal
    Created = DateTimeOffset.Now
    Updated = None
  }

  let update title body articleType article = {
    article with
      Title = title |> Option.defaultValue article.Title
      Body = body |> Option.defaultValue article.Body
      Type = articleType |> Option.defaultValue article.Type
      Updated = Some DateTimeOffset.Now
  }


module ArticleSearchArgs =

  open FSharp.JsonApi

  let create title types createdAfter createdBefore sort = {
    Title = title
    Types = types
    CreatedAfter = createdAfter
    CreatedBefore = createdBefore
    Sort = fst sort
    SortDescending = snd sort = SortDir.Descending
  }


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
  Sort: CommentSort
  SortDescending: bool
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

  let update body (comment: Comment) = {
    comment with
      Body = body |> Option.defaultValue comment.Body
      Updated = Some DateTimeOffset.Now
  }


module CommentSearchArgs =

  open FSharp.JsonApi

  let create authorId authorFirstName sort = {
    Author = authorId
    AuthorFirstName = authorFirstName
    Sort = fst sort
    SortDescending = snd sort = SortDir.Descending
  }
