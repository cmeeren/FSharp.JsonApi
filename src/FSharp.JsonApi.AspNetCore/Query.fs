namespace FSharp.JsonApi

open Microsoft.AspNetCore.Http


[<AutoOpen>]
module private Helpers =

  let queryMap (ctx: HttpContext) =
    ctx.Request.Query
    |> Seq.map (fun kvp -> kvp.Key, string kvp.Value)
    |> Map.ofSeq

[<AutoOpen>]
module AspNetCoreQueryExtensions =

  type Query with

    /// Parses sparse fieldset query parameters and returns a structure that can
    /// be used to determine whether to include a given field.
    static member GetFieldsets(ctx: HttpContext) : Fieldsets =
      Query.GetFieldsets(queryMap ctx)

    /// Parses the JSON-API (comma-separated) 'include' query parameter and
    /// returns a list of include paths where each path is a list of relationship
    /// names.
    static member GetIncludePaths(ctx: HttpContext) : IncludePath list =
      Query.GetIncludePaths(queryMap ctx)

    /// Parses a comma-separated query parameter according to the specified map.
    /// Values that do not exist as keys in the map will give
    /// QueryError.InvalidEnum where allowedValues will be the map keys.
    static member GetList
        ( paramName: string,
          valueMap: Map<string, 'a>, 
          ctx: HttpContext
        ) : Result<'a list option, QueryError list> =
          Query.GetList(paramName, valueMap, queryMap ctx)

    /// Parses a comma-separated query parameter according to the specified map.
    /// Values that do not exist as keys in the map will give
    /// QueryError.InvalidEnum where allowedValues will be the map keys. If the
    /// query parameter is missing, defaultValue will be used.
    static member GetList
        ( paramName: string,
          valueMap: Map<string, 'a>,
          defaultValue: 'a list,
          ctx: HttpContext
        ) : Result<'a list, QueryError list> =
          Query.GetList(paramName, valueMap, defaultValue, queryMap ctx)

    /// Parses a comma-separated query parameter according to the specified map.
    /// Values that do not exist as keys in the map will give
    /// QueryError.InvalidEnum where allowedValues will be the string values of
    /// the map keys.
    static member GetList
        ( paramName: string,
          valueMap: Map<'enum, 'a>,
          ctx: HttpContext
        ) : Result<'a list option, QueryError list> =
      Query.GetList(paramName, valueMap, queryMap ctx)

    /// Parses a comma-separated query parameter according to the specified map.
    /// Values that do not exist as keys in the map will give
    /// QueryError.InvalidEnum where allowedValues will be the string values of
    /// the map keys. If the query parameter is missing, defaultValue will be
    /// used.
    static member GetList
        ( paramName: string,
          valueMap: Map<'enum, 'a>,
          defaultValue: 'a list,
          ctx: HttpContext
        ) : Result<'a list, QueryError list> =
      Query.GetList(paramName, valueMap, defaultValue, queryMap ctx)

    /// Parses a comma-separated query parameter using the specified function.
    static member GetList
        ( paramName: string,
          tryParse: string -> 'a option,
          ctx: HttpContext
        ) : Result<'a list option, QueryError list> =
      Query.GetList(paramName, tryParse, queryMap ctx)

    /// Parses a comma-separated query parameter using the specified function.
    /// If the query parameter is missing, defaultValue will be used.
    static member GetList
        ( paramName: string,
          tryParse: string -> 'a option,
          defaultValue: 'a list,
          ctx: HttpContext
        ) : Result<'a list, QueryError list> =
      Query.GetList(paramName, tryParse, defaultValue, queryMap ctx)

    /// Parses a comma-separated query parameter using the specified function. The
    /// Error string will be available as errMsg in the returned
    /// QueryError.InvalidParsed.
    static member GetList
        ( paramName: string,
          tryParse: string -> Result<'a, string>,
          ctx: HttpContext
        ) : Result<'a list option, QueryError list> =
      Query.GetList(paramName, tryParse, queryMap ctx)

    /// Parses a comma-separated query parameter using the specified function.
    /// The Error string will be available as errMsg in the returned
    /// QueryError.InvalidParsed. If the query parameter is missing,
    /// defaultValue will be used.
    static member GetList
        ( paramName: string,
          tryParse: string -> Result<'a, string>,
          defaultValue: 'a list,
          ctx: HttpContext
        ) : Result<'a list, QueryError list> =
      Query.GetList(paramName, tryParse, defaultValue, queryMap ctx)

    /// Parses a comma-separated query parameter as a list of strings.
    static member GetList
        ( paramName: string,
          ctx: HttpContext
        ) : Result<string list option, QueryError list> =
      Query.GetList(paramName, queryMap ctx)

    /// Parses a comma-separated query parameter as a list of strings. If the
    /// query parameter is missing, defaultValue will be used.
    static member GetList
        ( paramName: string,
          defaultValue: string list,
          ctx: HttpContext
        ) : Result<string list, QueryError list> =
      Query.GetList(paramName, defaultValue, queryMap ctx)

    /// Parses a required, comma-separated query parameter according to the
    /// specified map. Values that do not exist as keys in the map will give
    /// QueryError.InvalidEnum where allowedValues will be the map keys.
    static member RequireList
        ( paramName: string,
          valueMap: Map<string, 'a>,
          ctx: HttpContext
        ) : Result<'a list, QueryError list> =
      Query.RequireList(paramName, valueMap, queryMap ctx)

    /// Parses a required, comma-separated query parameter according to the
    /// specified map. Values that do not exist as keys in the map will give
    /// QueryError.InvalidEnum where allowedValues will be the string values of
    /// the map keys.
    static member RequireList
        ( paramName: string,
          valueMap: Map<'enum, 'a>,
          ctx: HttpContext
        ) : Result<'a list, QueryError list> =
      Query.RequireList(paramName, valueMap, queryMap ctx)

    /// Parses a required, comma-separated query parameter using the specified
    /// function.
    static member RequireList
        ( paramName: string,
          tryParse: string -> 'a option,
          ctx: HttpContext
        ) : Result<'a list, QueryError list> =
      Query.RequireList(paramName, tryParse, queryMap ctx)

    /// Parses a required, comma-separated query parameter using the specified
    /// function. The Error string will be available as errMsg in the returned
    /// QueryError.InvalidParsed.
    static member RequireList
        ( paramName: string,
          tryParse: string -> Result<'a, string>,
          ctx: HttpContext
        ) : Result<'a list, QueryError list> =
      Query.RequireList(paramName, tryParse, queryMap ctx)

    /// Parses a required, comma-separated query parameter as a list of strings.
    static member RequireList
        ( paramName: string,
          ctx: HttpContext
        ) : Result<string list, QueryError list> =
      Query.RequireList(paramName, queryMap ctx)

    /// Parses a singular query parameter (not containing commas) according to the
    /// specified map. Values that do not exist as keys in the map will give
    /// QueryError.InvalidEnum where allowedValues will be the map keys.
    static member GetSingle
        ( paramName: string,
          valueMap: Map<string, 'a>,
          ctx: HttpContext
        ) : Result<'a option, QueryError list> =
      Query.GetSingle(paramName, valueMap, queryMap ctx)

    /// Parses a singular query parameter (not containing commas) according to
    /// the specified map. Values that do not exist as keys in the map will give
    /// QueryError.InvalidEnum where allowedValues will be the map keys. If the
    /// query parameter is missing, defaultValue will be used.
    static member GetSingle
        ( paramName: string,
          valueMap: Map<string, 'a>,
          defaultValue: 'a,
          ctx: HttpContext
        ) : Result<'a, QueryError list> =
      Query.GetSingle(paramName, valueMap, defaultValue, queryMap ctx)

    /// Parses a singular query parameter (not containing commas) according to the
    /// specified map. Values that do not exist as keys in the map will give
    /// QueryError.InvalidEnum where allowedValues will be the string values of
    /// the map keys.
    static member GetSingle
        ( paramName: string,
          valueMap: Map<'enum, 'a>,
          ctx: HttpContext
        ) : Result<'a option, QueryError list> =
      Query.GetSingle(paramName, valueMap, queryMap ctx)

    /// Parses a singular query parameter (not containing commas) according to
    /// the specified map. Values that do not exist as keys in the map will give
    /// QueryError.InvalidEnum where allowedValues will be the string values of
    /// the map keys. If the query parameter is missing, defaultValue will be
    /// used.
    static member GetSingle
        ( paramName: string,
          valueMap: Map<'enum, 'a>,
          defaultValue: 'a,
          ctx: HttpContext
        ) : Result<'a, QueryError list> =
      Query.GetSingle(paramName, valueMap, defaultValue, queryMap ctx)

    /// Parses a singular query parameter (not containing commas) using the
    /// specified function.
    static member GetSingle
        ( paramName: string,
          tryParse: string -> 'a option,
          ctx: HttpContext
        ) : Result<'a option, QueryError list> =
      Query.GetSingle(paramName, tryParse, queryMap ctx)

    /// Parses a singular query parameter (not containing commas) using the
    /// specified function. If the query parameter is missing, defaultValue will
    /// be used.
    static member GetSingle
        ( paramName: string,
          tryParse: string -> 'a option,
          defaultValue: 'a,
          ctx: HttpContext
        ) : Result<'a, QueryError list> =
      Query.GetSingle(paramName, tryParse, defaultValue, queryMap ctx)

    /// Parses a singular query parameter (not containing commas) using the
    /// specified function. The Error string will be available as errMsg in the
    /// returned QueryError.InvalidParsed.
    static member GetSingle
        ( paramName: string,
          tryParse: string -> Result<'a, string>,
          ctx: HttpContext
        ) : Result<'a option, QueryError list> =
      Query.GetSingle(paramName, tryParse, queryMap ctx)

    /// Parses a singular query parameter (not containing commas) using the
    /// specified function. The Error string will be available as errMsg in the
    /// returned QueryError.InvalidParsed. If the query parameter is missing,
    /// defaultValue will be used.
    static member GetSingle
        ( paramName: string,
          tryParse: string -> Result<'a, string>,
          defaultValue: 'a,
          ctx: HttpContext
        ) : Result<'a, QueryError list> =
      Query.GetSingle(paramName, tryParse, defaultValue, queryMap ctx)

    /// Parses a query parameter as single string (not containing commas).
    static member GetSingle
        ( paramName: string,
          ctx: HttpContext
        ) : Result<string option, QueryError list> =
      Query.GetSingle(paramName, queryMap ctx)

    /// Parses a query parameter as single string (not containing commas). If
    /// the query parameter is missing, defaultValue will be used.
    static member GetSingle
        ( paramName: string,
          defaultValue: string,
          ctx: HttpContext
        ) : Result<string, QueryError list> =
      Query.GetSingle(paramName, defaultValue, queryMap ctx)

    /// Parses a required, singular query parameter (not containing commas)
    /// according to the specified map. Values that do not exist as keys in the
    /// map will give QueryError.InvalidEnum where allowedValues will be the map
    /// keys.
    static member RequireSingle
        ( paramName: string,
          valueMap: Map<string, 'a>,
          ctx: HttpContext
        ) : Result<'a, QueryError list> =
      Query.RequireSingle(paramName, valueMap, queryMap ctx)

    /// Parses a required, singular query parameter (not containing commas)
    /// according to the specified map. Values that do not exist as keys in the
    /// map will give QueryError.InvalidEnum where allowedValues will be the
    /// string values of the map keys.
    static member RequireSingle
        ( paramName: string,
          valueMap: Map<'enum, 'a>,
          ctx: HttpContext
        ) : Result<'a, QueryError list> =
      Query.RequireSingle(paramName, valueMap, queryMap ctx)

    /// Parses a required, singular query parameter (not containing commas) using
    /// the specified function.
    static member RequireSingle
        ( paramName: string,
          tryParse: string -> 'a option,
          ctx: HttpContext
        ) : Result<'a, QueryError list> =
      Query.RequireSingle(paramName, tryParse, queryMap ctx)

    /// Parses a required, singular query parameter (not containing commas) using
    /// the specified function. The Error string will be available as errMsg in
    /// the returned QueryError.InvalidParsed.
    static member RequireSingle
        ( paramName: string,
          tryParse: string -> Result<'a, string>,
          ctx: HttpContext
        ) : Result<'a, QueryError list> =
      Query.RequireSingle(paramName, tryParse, queryMap ctx)

    /// Parses a required query parameter as single string (not containing
    /// commas).
    static member RequireSingle
        ( paramName: string,
          ctx: HttpContext
        ) : Result<string, QueryError list> =
      Query.RequireSingle(paramName, queryMap ctx)

    /// Parses a query parameter as a single boolean ("true"/"false").
    static member GetBool
        ( paramName: string,
          ctx: HttpContext
        ) : Result<bool option, QueryError list> =
      Query.GetBool(paramName, queryMap ctx)

    /// Parses a query parameter as a single boolean ("true"/"false"). If the
    /// query parameter is missing, defaultValue will be used.
    static member GetBool
        ( paramName: string,
          defaultValue: bool,
          ctx: HttpContext
        ) : Result<bool, QueryError list> =
      Query.GetBool(paramName, defaultValue, queryMap ctx)

    /// Parses a required query parameter as a single boolean ("true"/"false").
    static member RequireBool
        ( paramName: string,
          ctx: HttpContext
        ) : Result<bool, QueryError list> =
      Query.RequireBool(paramName, queryMap ctx)

    /// Parses the given query parameter as an integer between optional min and
    /// max values.
    static member GetBoundInt
        ( paramName: string,
          ctx: HttpContext,
          ?min: int,
          ?max: int
        ) : Result<int option, QueryError list> =
      Query.GetBoundInt(paramName, queryMap ctx, ?min = min, ?max = max)

    /// Parses the given query parameter as an integer between optional min and
    /// max values. If the query parameter is missing, defaultValue will be
    /// used.
    static member GetBoundInt
        ( paramName: string,
          defaultValue: int,
          ctx: HttpContext,
          ?min: int,
          ?max: int
        ) : Result<int, QueryError list> =
      Query.GetBoundInt(paramName, defaultValue, queryMap ctx, ?min = min, ?max = max)

    /// Parses the given required query parameter as an integer between optional
    /// min and max values.
    static member RequireBoundInt
        ( paramName: string,
          ctx: HttpContext,
          ?min: int,
          ?max: int
        ) : Result<int, QueryError list> =
      Query.RequireBoundInt(paramName, queryMap ctx, ?min = min, ?max = max)

    /// Parses the JSON-API 'sort' query parameter according to the specified
    /// map. Values that do not exist as keys in the map will give
    /// QueryErr.InvalidEnum where allowedValues will be the map keys. The
    /// boolean indicates whether to sort descending (true) or ascending
    /// (false).
    static member GetSortList
        ( valueMap: Map<string, 'a>,
          ctx: HttpContext
        ) : Result<('a * bool) list option, QueryError list> =
      Query.GetSortList(valueMap, queryMap ctx)

    /// Parses the JSON-API 'sort' query parameter according to the specified
    /// map. Values that do not exist as keys in the map will give
    /// QueryErr.InvalidEnum where allowedValues will be the map keys. If the
    /// query parameter is missing, defaultValue will be used. The boolean
    /// indicates whether to sort descending (true) or ascending (false).
    static member GetSortList
        ( valueMap: Map<string, 'a>,
          defaultValue: ('a * bool) list,
          ctx: HttpContext
        ) : Result<('a * bool) list, QueryError list> =
      Query.GetSortList(valueMap, defaultValue, queryMap ctx)

    /// Parses the JSON-API 'sort' query parameter according to the specified
    /// map. Values that do not exist as keys in the map will give
    /// QueryErr.InvalidEnum where allowedValues will be the string values of
    /// the map keys. The boolean indicates whether to sort descending (true) or
    /// ascending (false).
    static member GetSortList
        ( valueMap: Map<'enum, 'a>,
          ctx: HttpContext
        ) : Result<('a * bool) list option, QueryError list> =
      Query.GetSortList(valueMap, queryMap ctx)

    /// Parses the JSON-API 'sort' query parameter according to the specified
    /// map. Values that do not exist as keys in the map will give
    /// QueryErr.InvalidEnum where allowedValues will be the string values of
    /// the map keys. If the query parameter is missing, defaultValue will be
    /// used. The boolean indicates whether to sort descending (true) or
    /// ascending (false).
    static member GetSortList
        ( valueMap: Map<'enum, 'a>,
          defaultValue: ('a * bool) list,
          ctx: HttpContext
        ) : Result<('a * bool) list, QueryError list> =
      Query.GetSortList(valueMap, defaultValue, queryMap ctx)
  
    /// Parses the JSON-API 'sort' query parameter according to the specified
    /// map. Will return an error if the query parameter is not present. Values
    /// that do not exist as keys in the map will give QueryErr.InvalidEnum
    /// where allowedValues will be the map keys. The boolean indicates whether
    /// to sort descending (true) or ascending (false).
    static member RequireSortList
        ( valueMap: Map<string, 'a>,
          ctx: HttpContext
        ) : Result<('a * bool) list, QueryError list> =
      Query.RequireSortList(valueMap, queryMap ctx)
  
    /// Parses the JSON-API 'sort' query parameter according to the specified
    /// map. Will return an error if the query parameter is not present. Values
    /// that do not exist as keys in the map will give QueryErr.InvalidEnum
    /// where allowedValues will be the string values of map keys. The boolean
    /// indicates whether to sort descending (true) or ascending (false).
    static member RequireSortList
        ( valueMap: Map<'enum, 'a>,
          ctx: HttpContext
        ) : Result<('a * bool) list, QueryError list> =
      Query.RequireSortList(valueMap, queryMap ctx)

    /// Parses the JSON-API 'sort' query parameter according to the specified
    /// map. Only a single value is supported (not containing commas). Values
    /// that do not exist as keys in the map will give QueryErr.InvalidEnum
    /// where allowedValues will be the map keys. The boolean indicates whether
    /// to sort descending (true) or ascending (false).
    static member GetSortSingle
        ( valueMap: Map<string, 'a>,
          ctx: HttpContext
        ) : Result<('a * bool) option, QueryError list> =
      Query.GetSortSingle(valueMap, queryMap ctx)

    /// Parses the JSON-API 'sort' query parameter according to the specified
    /// map. Only a single value is supported (not containing commas). Values
    /// that do not exist as keys in the map will give QueryErr.InvalidEnum
    /// where allowedValues will be the map keys. If the query parameter is
    /// missing, defaultValue will be used. The boolean indicates whether to
    /// sort descending (true) or ascending (false).
    static member GetSortSingle
        ( valueMap: Map<string, 'a>,
          defaultValue: 'a * bool,
          ctx: HttpContext
        ) : Result<'a * bool, QueryError list> =
      Query.GetSortSingle(valueMap, defaultValue, queryMap ctx)
  
    /// Parses the JSON-API 'sort' query parameter according to the specified
    /// map. Only a single value is supported (not containing commas). Values
    /// that do not exist as keys in the map will give QueryErr.InvalidEnum
    /// where allowedValues will be the string values of the map keys. The
    /// boolean indicates whether to sort descending (true) or ascending
    /// (false).
    static member GetSortSingle
        ( valueMap: Map<'enum, 'a>,
          ctx: HttpContext
        ) : Result<('a * bool) option, QueryError list> =
      Query.GetSortSingle(valueMap, queryMap ctx)

    /// Parses the JSON-API 'sort' query parameter according to the specified
    /// map. Only a single value is supported (not containing commas). Values
    /// that do not exist as keys in the map will give QueryErr.InvalidEnum
    /// where allowedValues will be the string values of the map keys. If the
    /// query parameter is missing, defaultValue will be used. The boolean
    /// indicates whether to sort descending (true) or ascending (false).
    static member GetSortSingle
        ( valueMap: Map<'enum, 'a>,
          defaultValue: 'a * bool,
          ctx: HttpContext
        ) : Result<'a * bool, QueryError list> =
      Query.GetSortSingle(valueMap, defaultValue, queryMap ctx)

    /// Parses the JSON-API 'sort' query parameter according to the specified
    /// map. Only a single value is supported (not containing commas). Will
    /// return an error if the query parameter is not present. Values that do
    /// not exist as keys in the map will give QueryErr.InvalidEnum where
    /// allowedValues will be the map keys. The boolean indicates whether to
    /// sort descending (true) or ascending (false).
    static member RequireSortSingle
        ( valueMap: Map<string, 'a>,
          ctx: HttpContext
        ) : Result<'a * bool, QueryError list> =
      Query.RequireSortSingle(valueMap, queryMap ctx)
  
    /// Parses the JSON-API 'sort' query parameter according to the specified
    /// map. Only a single value is supported (not containing commas). Will
    /// return an error if the query parameter is not present. Values that do
    /// not exist as keys in the map will give QueryErr.InvalidEnum where
    /// allowedValues will be the string values of the map keys. The boolean
    /// indicates whether to sort descending (true) or ascending (false).
    static member RequireSortSingle
        ( valueMap: Map<'enum, 'a>,
          ctx: HttpContext
        ) : Result<'a * bool, QueryError list> =
      Query.RequireSortSingle(valueMap, queryMap ctx)
