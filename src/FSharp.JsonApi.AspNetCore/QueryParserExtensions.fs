namespace FSharp.JsonApi

open Microsoft.AspNetCore.Http


[<AutoOpen>]
module QueryParserExtensions =

  type QueryParser with

    /// Returns a parser for the query params in the specified HttpContext.
    static member FromHttpContext(ctx: HttpContext) =
      ctx.Request.Query
      |> Seq.map (fun kvp -> kvp.Key, string kvp.Value)
      |> Map.ofSeq
      |> QueryParser.FromQueryParamMap
