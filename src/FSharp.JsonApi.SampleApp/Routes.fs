module Routes

// This module contains the API's routing. Nothing too surprising here if you
// know Giraffe.


open Giraffe
open FSharp.JsonApi

open Resources
open ErrorHandling
open HttpHandlers


let mainHandler : HttpHandler =
  choose [

    validateJsonApiRequest (List.map requestError >> handleErrors)
    >=> choose [


      // Person endpoints
      subRoute (sprintf "/%s" CollectionUrlSegments.person)
        (choose [

          // Person collection endpoints
          routex "/?" >=> GET_HEAD >=> Person.search
          routex "/?" >=> POST >=> Person.create

          // Specific person endpoints
          subRoutef "/%s" (fun apiId -> find Find.person apiId (fun person ->
            choose [

              routex "/?" >=> GET_HEAD >=> Person.get person
              routex "/?" >=> PATCH >=> Person.update person
              routex "/?" >=> DELETE >=> Person.delete person

              // Relationship "related" link endpoints
              routex (sprintf "/%s/?" (nameof <@ any<PersonRels>.articles @>))
                >=> GET_HEAD >=> Person.articles person

              // In ResourceBuilders.fs we added some custom test links ("foo"
              // and "bar") to the person resource - if actually implemented,
              // they would be placed here
            ]
          ))
        ])



      // Article endpoints
      subRoute (sprintf "/%s" CollectionUrlSegments.article)
        (choose [

          // Article collection endpoints
          routex "/?" >=> GET_HEAD >=> Article.search
          routex "/?" >=> POST >=> Article.create

          // Specific article endpoints
          subRoutef "/%s" (fun apiId -> find Find.article apiId (fun article ->
            choose [

              routex "/?" >=> GET_HEAD >=> Article.get article
              routex "/?" >=> PATCH >=> Article.update article
              routex "/?" >=> DELETE >=> Article.delete article

              // Relationship "related" link endpoints
              routex (sprintf "/%s/?" (nameof <@ any<ArticleRels>.author @>))
                >=> GET_HEAD >=> Article.author article
              routex (sprintf "/%s/?" (nameof <@ any<ArticleRels>.comments @>))
                >=> GET_HEAD >=> Article.comments article
            ]
          ))
        ])



      // Comment endpoints
      subRoute (sprintf "/%s" CollectionUrlSegments.comment)
        (choose [

          // Comment collection endpoints
          routex "/?" >=> GET_HEAD >=> Comment.search
          routex "/?" >=> POST >=> Comment.create

          // Specific comment endpoints
          subRoutef "/%s" (fun apiId -> find Find.comment apiId (fun comment ->
            choose [

              routex "/?" >=> GET_HEAD >=> Comment.get comment
              routex "/?" >=> PATCH >=> Comment.update comment
              routex "/?" >=> DELETE >=> Comment.delete comment

              // Relationship "related" link endpoints
              routex (sprintf "/%s/?" (nameof <@ any<CommentRels>.author @>))
                >=> GET_HEAD >=> Comment.author comment
              routex (sprintf "/%s/?" (nameof <@ any<CommentRels>.article @>))
                >=> GET_HEAD >=> Comment.article comment
            ]
          ))
        ])

      ]

    handleError NotFound
  ]
