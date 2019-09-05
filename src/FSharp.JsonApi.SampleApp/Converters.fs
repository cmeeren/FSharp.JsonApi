module Converters


// This module is the single, canonical place for transforming internal IDs and
// supporting types (e.g. enums) to/from the API representations.


open System
open Domain
open Resources


module PersonId =

  let toApi (PersonId personId) =
    personId.ToString()

  let fromApi (str: string) =
    match Guid.TryParse str with
    | false, _ -> None
    | true, guid -> guid |> PersonId |> Some


module ArticleId =

  let toApi (ArticleId articleId) =
    articleId.ToString()

  let fromApi (str: string) =
    match Guid.TryParse str with
    | false, _ -> None
    | true, guid -> guid |> ArticleId |> Some


module CommentId =

  let toApi (CommentId commentId) =
    commentId.ToString()

  let fromApi (str: string) =
    match Guid.TryParse str with
    | false, _ -> None
    | true, guid -> guid |> CommentId |> Some


module Gender =

  let toApi = function
    | Male -> Gender.male
    | Female -> Gender.female
    | Other -> Gender.other

  // We implement the reverse transformation as a map instead of a function, so
  // that when parsing fails, it's easy to return nice error messages with the
  // allowed values (FSharp.JsonApi has helpful overloads for this).

  let fromApiMap = Map.ofList [
    Gender.male, Male
    Gender.female, Female
    Gender.other, Other
  ]


module ArticleType =

  let toApi = function
    | Personal -> ArticleType.personal
    | Commercial -> ArticleType.commercial

  let fromApiMap = Map.ofList [
    ArticleType.personal, Personal
    ArticleType.commercial, Commercial
  ]


