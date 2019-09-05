namespace FSharp.JsonApi

open Microsoft.AspNetCore.Http


// Must be in a separate file to give even lower priority than the other
// extensions
[<AutoOpen>]
module AspNetCoreQueryExtensions2 =

  type Query with

    /// Parses a comma-separated query parameter using the specified function.
    static member GetList
        ( paramName: string,
          parse: string -> 'a,
          ctx: HttpContext
        ) : Result<'a list option, QueryError list> =
      Query.GetList(paramName, parse, queryMap ctx)

    /// Parses a required, comma-separated query parameter using the specified
    /// function.
    static member RequireList
        ( paramName: string,
          parse: string -> 'a,
          ctx: HttpContext
        ) : Result<'a list, QueryError list> =
      Query.RequireList(paramName, parse, queryMap ctx)

    /// Parses a singular query parameter (not containing commas) using the
    /// specified function.
    static member GetSingle
        ( paramName: string,
          parse: string -> 'a,
          ctx: HttpContext
        ) : Result<'a option, QueryError list> =
      Query.GetSingle(paramName, parse, queryMap ctx)

    /// Parses a required, singular query parameter (not containing commas) using
    /// the specified function.
    static member RequireSingle
        ( paramName: string,
          parse: string -> 'a,
          ctx: HttpContext
        ) : Result<'a, QueryError list> =
      Query.RequireSingle(paramName, parse, queryMap ctx)
