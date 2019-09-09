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
  SortBy: PersonSort
  SortDescending: bool
  Offset: int
  Limit: int
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

  let create firstName lastName twitter genders (sortBy, sortDesc) offset limit = {
    FirstName = firstName
    LastName = lastName
    Twitter = twitter
    Genders = genders
    SortBy = sortBy
    SortDescending = sortDesc
    Offset = offset
    Limit = limit
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
  SortBy: ArticleSort
  SortDescending: bool
  Offset: int
  Limit: int
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

  let update authorId title body articleType article = {
    article with
      AuthorId = authorId |> Option.defaultValue article.AuthorId
      Title = title |> Option.defaultValue article.Title
      Body = body |> Option.defaultValue article.Body
      Type = articleType |> Option.defaultValue article.Type
      Updated = Some DateTimeOffset.Now
  }


module ArticleSearchArgs =

  let create title types createdAfter createdBefore (sortBy, sortDesc) offset limit = {
    Title = title
    Types = types
    CreatedAfter = createdAfter
    CreatedBefore = createdBefore
    SortBy = sortBy
    SortDescending = sortDesc
    Offset = offset
    Limit = limit
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

  let update body (comment: Comment) = {
    comment with
      Body = body |> Option.defaultValue comment.Body
      Updated = Some DateTimeOffset.Now
  }


module CommentSearchArgs =

  let create authorId authorFirstName (sortBy, sortDesc) offset limit = {
    Author = authorId
    AuthorFirstName = authorFirstName
    SortBy = sortBy
    SortDescending = sortDesc
    Offset = offset
    Limit = limit
  }
