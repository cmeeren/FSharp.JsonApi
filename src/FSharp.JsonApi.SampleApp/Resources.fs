module Resources

// This module contains the type definitions of everything that is sent over the
// wire.

open System
open FSharp.JsonApi
open FSharp.JsonSkippable


// It's nice to have a single place for the resource type names and collection
// URL segments since they're used in more than once place.

module TypeNames =

  let [<Literal>] person = "person"
  let [<Literal>] article = "article"
  let [<Literal>] comment = "comment"


module CollectionUrlSegments =

  let [<Literal>] person = "persons"
  let [<Literal>] article = "articles"
  let [<Literal>] comment = "comments"


(*
Below we define record types for attributes and relationships, as well as any
supporting types.

Discriminated unions can be represented by enums, which will be serialized as
string.

The following is required and will be checked at initialization:

 - Attribute and relationship types must use [<CLIMutable>]

 - All attributes and relationships (i.e. members of attribute and relationship
   types) must be wrapped in Skippable

Otherwise:

 - All property names will be serialized exactly as they are - no mucking about
   with configuring NamingConventions, casing rules etc. Use double backticks
   for special names as shown below.

 - For attributes, use [<ReadOnly>] and [<WriteOnly>] as needed (will be used
   when validating requests)

 - For relationships, use [<AllowedTypes>], [<NotNull>] (for to-one resources),
   [<ReadOnly>], and [<WriteOnly>] as needed (will be used when validating
   requests)
*)


type Gender =
  | male = 0
  | female = 1
  | other = 2

[<CLIMutable>]
type PersonAttrs = {
  firstName: string Skippable
  lastName: string Skippable
  twitter: string option Skippable
  gender: Gender option Skippable
}

[<CLIMutable>]
type PersonRels = {
  [<ReadOnly; AllowedTypes(TypeNames.article)>]
  articles: ToMany Skippable
}


type ArticleType =
  | personal = 0
  | commercial = 1

[<CLIMutable>]
type ArticleAttrs = {
  title: string Skippable
  body: string Skippable
  ``type``: ArticleType Skippable
  [<ReadOnly>] created: DateTimeOffset Skippable
  [<ReadOnly>] updated: DateTimeOffset option Skippable
}

[<CLIMutable>]
type ArticleRels = {
  [<NotNull; AllowedTypes(TypeNames.person)>]
  author: ToOne Skippable
  [<ReadOnly; NotNull; AllowedTypes(TypeNames.comment)>]
  comments: ToMany Skippable
}


[<CLIMutable>]
type CommentAttrs = {
  body: string Skippable
  [<ReadOnly>] created: DateTimeOffset Skippable
  [<ReadOnly>] updated: DateTimeOffset option Skippable
}

[<CLIMutable>]
type CommentRels = {
  // These are read-only because it shouldn't be possible to change them after
  // creation. However, they are mandatory when creating a comment using
  // POST /comments. We'll override the read-only status when in that operation.
  [<NotNull; ReadOnly; AllowedTypes(TypeNames.person)>]
  author: ToOne Skippable
  [<NotNull; ReadOnly; AllowedTypes(TypeNames.article)>]
  article: ToOne Skippable
}


// After all resources are defined, define the resource discriminator, which
//  1) provides FSharp.JsonApi with all necessary information about the
//     resources supported by the API, and
//  2) allows for a nice and friendly syntax for parsing resources.

type ResourceDiscriminator =
  | [<ResourceName(TypeNames.person)>]
    Person of Resource<PersonAttrs, PersonRels>

  | [<ResourceName(TypeNames.article)>]
    Article of Resource<ArticleAttrs, ArticleRels>

  | [<ResourceName(TypeNames.comment)>]
    Comment of Resource<CommentAttrs, CommentRels>


// The JsonApiContext contains all information about the resources supported by
// the API. Ut uses the resource discriminator and the types it references to
// get this information at startup. It is used to serialize, deserialize,
// create, parse, and validate documents.
//
// You want this to be called as early as possible to ensure errors are caught
// at startup and not at the first request, so we refer to jsonApiCtx from
// Program.fs to force module initialization at startup.

let jsonApiCtx = JsonApiContext.create<ResourceDiscriminator>
