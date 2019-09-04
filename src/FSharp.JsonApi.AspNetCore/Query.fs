namespace FSharp.JsonApi

open System
open Microsoft.AspNetCore.Http


/// Represents errors during query parameter parsing and validation.
[<RequireQualifiedAccess>]
type QueryError =
  /// A query parameter value was not allowed.
  | InvalidEnum of paramName: string * illegalValue: string * allowedValues: string list
  /// A query parameter's value could not be parsed and resulted in an error
  /// message.
  | InvalidParsed of paramName: string * invalidValue: string * errMsg: string option
  /// The query parameter value was too large.
  | TooLarge of paramName: string * invalidValue: int * max: int
  /// The query parameter value was too small.
  | TooSmall of paramName: string * invalidValue: int * min: int
  /// A required query parameter was missing.
  | Missing of paramName: string
  /// A query parameter that only supports a single value got multiple values.
  | NotSingular of paramName: string * numProvidedValues: int


[<AutoOpen>]
module private Helpers =

  open System.Collections.Concurrent

  let withStringKeys<'a, 'b, 'c when 'a : enum<'c> and 'a : comparison> =
    let d = ConcurrentDictionary<_,_>(HashIdentity.Reference)
    fun (valueMap: Map<'a, 'b>) ->
      d.GetOrAdd(valueMap, valueMap |> Map.mapKv (box >> string) id)

  let boolMap =
    [ "true", true
      "false", false ]
    |> Map.ofList


type Query =

  /// Parses sparse fieldset query parameters and returns a structure that can
  /// be used to determine whether to include a given field.
  static member GetFieldsets(ctx: HttpContext) : Fieldsets =
    ctx.Request.Query
    |> Seq.filter (fun kvp -> kvp.Key.StartsWith "fields[" && kvp.Key.EndsWith "]")
    |> Seq.map (fun kvp ->
          let resourceType = kvp.Key |> String.removePrefix "fields[" |> String.removeSuffix "]"
          let fields =
            (string kvp.Value).Split ','
            |> Set.ofArray
            |> Set.map String.trim
          (resourceType, fields)
        )
    |> Map.ofSeq

  /// Parses the JSON-API (comma-separated) 'include' query parameter and
  /// returns a list of include paths where each path is a list of relationship
  /// names.
  static member GetIncludePaths(ctx: HttpContext) : IncludePath list =
    match ctx.Request.Query.TryGetValue "include" with
    | false, _ -> []
    | true, rawValue ->
        (string rawValue).Split ','
        |> Array.toList
        |> List.map (fun s ->
            s.Split '.'
            |> Array.toList
            |> List.map String.trim
            |> List.filter ((<>) "")
        )

  /// Parses a comma-separated query parameter according to the specified map.
  /// Values that do not exist as keys in the map will give
  /// QueryError.InvalidEnum where allowedValues will be the map keys.
  static member GetList
      ( paramName: string,
        valueMap: Map<string, 'a>, 
        ctx: HttpContext
      ) : Result<'a list option, QueryError list> =
    match ctx.Request.Query.TryGetValue paramName with
    | false, _ -> Ok None
    | true, rawValue ->
        (string rawValue).Split ','
        |> Array.toList
        |> List.map String.trim
        |> List.map (fun value ->
            match valueMap |> Map.tryFind value with
            | Some x -> Ok [x]
            | None ->
                let allowedValues = valueMap |> Map.toList |> List.map fst
                Error [ QueryError.InvalidEnum (paramName, value, allowedValues) ]
        )
        |> List.reduce Result.combine
        |> Result.map Some

  /// Parses a comma-separated query parameter according to the specified map.
  /// Values that do not exist as keys in the map will give
  /// QueryError.InvalidEnum where allowedValues will be the string values of
  /// the map keys.
  static member GetList
      ( paramName: string,
        valueMap: Map<'enum, 'a>,
        ctx: HttpContext
      ) : Result<'a list option, QueryError list> =
    Query.GetList(paramName, withStringKeys valueMap, ctx)

  /// Parses a comma-separated query parameter using the specified function.
  static member GetList
      ( paramName: string,
        tryParse: string -> 'a option,
        ctx: HttpContext
      ) : Result<'a list option, QueryError list> =
    match ctx.Request.Query.TryGetValue paramName with
    | false, _ -> Ok None
    | true, rawValue ->
        (string rawValue).Split ','
        |> Array.toList
        |> List.map String.trim
        |> List.map (fun value ->
            match tryParse value with
            | Some x -> Ok [x]
            | None -> Error [ QueryError.InvalidParsed (paramName, value, None) ]
        )
        |> List.reduce Result.combine
        |> Result.map Some

  /// Parses a comma-separated query parameter using the specified function. The
  /// Error string will be available as errMsg in the returned
  /// QueryError.InvalidParsedMsg.
  static member GetList
      ( paramName: string,
        tryParse: string -> Result<'a, string>,
        ctx: HttpContext
      ) : Result<'a list option, QueryError list> =
    match ctx.Request.Query.TryGetValue paramName with
    | false, _ -> Ok None
    | true, rawValue ->
        (string rawValue).Split ','
        |> Array.toList
        |> List.map String.trim
        |> List.map (fun value ->
            match tryParse value with
            | Ok x -> Ok [x]
            | Error msg -> Error [ QueryError.InvalidParsed (paramName, value, Some msg) ]
        )
        |> List.reduce Result.combine
        |> Result.map Some

  /// Parses a comma-separated query parameter as a list of strings.
  static member GetList
      ( paramName: string,
        ctx: HttpContext
      ) : Result<string list option, QueryError list> =
    Query.GetList(paramName, Some, ctx)

  /// Parses a required, comma-separated query parameter according to the
  /// specified map. Values that do not exist as keys in the map will give
  /// QueryError.InvalidEnum where allowedValues will be the map keys.
  static member RequireList
      ( paramName: string,
        valueMap: Map<string, 'a>,
        ctx: HttpContext
      ) : Result<'a list, QueryError list> =
    let errIfNone = [ QueryError.Missing paramName ]
    Query.GetList(paramName, valueMap, ctx)
    |> Result.bind (Result.requireSome errIfNone)

  /// Parses a required, comma-separated query parameter according to the
  /// specified map. Values that do not exist as keys in the map will give
  /// QueryError.InvalidEnum where allowedValues will be the string values of
  /// the map keys.
  static member RequireList
      ( paramName: string,
        valueMap: Map<'enum, 'a>,
        ctx: HttpContext
      ) : Result<'a list, QueryError list> =
    Query.RequireList(paramName, withStringKeys valueMap, ctx)

  /// Parses a required, comma-separated query parameter using the specified
  /// function.
  static member RequireList
      ( paramName: string,
        tryParse: string -> 'a option,
        ctx: HttpContext
      ) : Result<'a list, QueryError list> =
    let errIfNone = [ QueryError.Missing paramName ]
    Query.GetList(paramName, tryParse, ctx)
    |> Result.bind (Result.requireSome errIfNone)

  /// Parses a required, comma-separated query parameter using the specified
  /// function. The Error string will be available as errMsg in the returned
  /// QueryError.InvalidParsedMsg.
  static member RequireList
      ( paramName: string,
        tryParse: string -> Result<'a, string>,
        ctx: HttpContext
      ) : Result<'a list, QueryError list> =
    let errIfNone = [ QueryError.Missing paramName ]
    Query.GetList(paramName, tryParse, ctx)
    |> Result.bind (Result.requireSome errIfNone)

  /// Parses a required, comma-separated query parameter as a list of strings.
  static member RequireList
      ( paramName: string,
        ctx: HttpContext
      ) : Result<string list, QueryError list> =
    Query.RequireList(paramName, Some, ctx)

  /// Parses a singular query parameter (not containing commas) according to the
  /// specified map. Values that do not exist as keys in the map will give
  /// QueryError.InvalidEnum where allowedValues will be the map keys.
  static member GetSingle
      ( paramName: string,
        valueMap: Map<string, 'a>,
        ctx: HttpContext
      ) : Result<'a option, QueryError list> =
    match Query.GetList(paramName, valueMap, ctx) with
    | Ok None -> Ok None
    | Ok (Some [x]) -> Ok (Some x)
    | Ok (Some xs) -> Error [ QueryError.NotSingular (paramName, xs.Length) ]
    | Error [x] -> Error [x]
    | Error xs -> Error <| QueryError.NotSingular (paramName, xs.Length) :: xs

  /// Parses a singular query parameter (not containing commas) according to the
  /// specified map. Values that do not exist as keys in the map will give
  /// QueryError.InvalidEnum where allowedValues will be the string values of
  /// the map keys.
  static member GetSingle
      ( paramName: string,
        valueMap: Map<'enum, 'a>,
        ctx: HttpContext
      ) : Result<'a option, QueryError list> =
    Query.GetSingle(paramName, withStringKeys valueMap, ctx)

  /// Parses a singular query parameter (not containing commas) using the
  /// specified function.
  static member GetSingle
      ( paramName: string,
        tryParse: string -> 'a option,
        ctx: HttpContext
      ) : Result<'a option, QueryError list> =
    match Query.GetList(paramName, tryParse, ctx) with
    | Ok None -> Ok None
    | Ok (Some [x]) -> Ok (Some x)
    | Ok (Some xs) -> Error [ QueryError.NotSingular (paramName, xs.Length) ]
    | Error [x] -> Error [x]
    | Error xs -> Error <| QueryError.NotSingular (paramName, xs.Length) :: xs

  /// Parses a singular query parameter (not containing commas) using the
  /// specified function. The Error string will be available as errMsg in the
  /// returned QueryError.InvalidParsedMsg.
  static member GetSingle
      ( paramName: string,
        tryParse: string -> Result<'a, string>,
        ctx: HttpContext
      ) : Result<'a option, QueryError list> =
    match Query.GetList(paramName, tryParse, ctx) with
    | Ok None -> Ok None
    | Ok (Some [x]) -> Ok (Some x)
    | Ok (Some xs) -> Error [ QueryError.NotSingular (paramName, xs.Length) ]
    | Error [x] -> Error [x]
    | Error xs -> Error <| QueryError.NotSingular (paramName, xs.Length) :: xs

  /// Parses a query parameter as single string (not containing commas).
  static member GetSingle
      ( paramName: string,
        ctx: HttpContext
      ) : Result<string option, QueryError list> =
    Query.GetSingle(paramName, Some, ctx)

  /// Parses a required, singular query parameter (not containing commas)
  /// according to the specified map. Values that do not exist as keys in the
  /// map will give QueryError.InvalidEnum where allowedValues will be the map
  /// keys.
  static member RequireSingle
      ( paramName: string,
        valueMap: Map<string, 'a>,
        ctx: HttpContext
      ) : Result<'a, QueryError list> =
    match Query.GetList(paramName, valueMap, ctx) with
    | Ok None -> Error [ QueryError.Missing paramName ]
    | Ok (Some [x]) -> Ok x
    | Ok (Some xs) -> Error [ QueryError.NotSingular (paramName, xs.Length) ]
    | Error [x] -> Error [x]
    | Error xs -> Error <| QueryError.NotSingular (paramName, xs.Length) :: xs

  /// Parses a required, singular query parameter (not containing commas)
  /// according to the specified map. Values that do not exist as keys in the
  /// map will give QueryError.InvalidEnum where allowedValues will be the
  /// string values of the map keys.
  static member RequireSingle
      ( paramName: string,
        valueMap: Map<'enum, 'a>,
        ctx: HttpContext
      ) : Result<'a, QueryError list> =
    Query.RequireSingle(paramName, withStringKeys valueMap, ctx)

  /// Parses a required, singular query parameter (not containing commas) using
  /// the specified function.
  static member RequireSingle
      ( paramName: string,
        tryParse: string -> 'a option,
        ctx: HttpContext
      ) : Result<'a, QueryError list> =
    match Query.GetList(paramName, tryParse, ctx) with
    | Ok None -> Error [ QueryError.Missing paramName ]
    | Ok (Some [x]) -> Ok x
    | Ok (Some xs) -> Error [ QueryError.NotSingular (paramName, xs.Length) ]
    | Error [x] -> Error [x]
    | Error xs -> Error <| QueryError.NotSingular (paramName, xs.Length) :: xs

  /// Parses a required, singular query parameter (not containing commas) using
  /// the specified function. The Error string will be available as errMsg in
  /// the returned QueryError.InvalidParsedMsg.
  static member RequireSingle
      ( paramName: string,
        tryParse: string -> Result<'a, string>,
        ctx: HttpContext
      ) : Result<'a, QueryError list> =
    match Query.GetList(paramName, tryParse, ctx) with
    | Ok None -> Error [ QueryError.Missing paramName ]
    | Ok (Some [x]) -> Ok x
    | Ok (Some xs) -> Error [ QueryError.NotSingular (paramName, xs.Length) ]
    | Error [x] -> Error [x]
    | Error xs -> Error <| QueryError.NotSingular (paramName, xs.Length) :: xs

  /// Parses a required query parameter as single string (not containing
  /// commas).
  static member RequireSingle
      ( paramName: string,
        ctx: HttpContext
      ) : Result<string, QueryError list> =
    Query.RequireSingle(paramName, Some, ctx)

  /// Parses a query parameter as a single boolean ("true"/"false").
  static member GetBool
      ( paramName: string,
        ctx: HttpContext
      ) : Result<bool option, QueryError list> =
    Query.GetSingle(paramName, boolMap, ctx)

  /// Parses a required query parameter as a single boolean ("true"/"false").
  static member RequireBool
      ( paramName: string,
        ctx: HttpContext
      ) : Result<bool, QueryError list> =
    Query.RequireSingle(paramName, boolMap, ctx)

  /// Parses the given query parameter as an integer between optional min and
  /// max values.
  static member GetBoundInt
      ( paramName: string,
        ctx: HttpContext,
        ?min: int,
        ?max: int
      ) : Result<int option, QueryError list> =
    match ctx.Request.Query.TryGetValue paramName with
    | false, _ -> Ok None
    | true, rawValue ->
        let value = string rawValue
        match Int32.TryParse value, min, max with
        | (true, i), Some min, _ when i < min ->
            Error [ QueryError.TooSmall (paramName, i, min) ]
        | (true, i), _, Some max when i > max ->
            Error [ QueryError.TooLarge (paramName, i, max) ]
        | (true, i), _, _ -> Ok (Some i)
        | (false, _), _, _ ->
            Error [ QueryError.InvalidParsed (paramName, value, None) ]

  /// Parses the given required query parameter as an integer between optional
  /// min and max values.
  static member RequireBoundInt
      ( paramName: string,
        ctx: HttpContext,
        ?min: int,
        ?max: int
      ) : Result<int, QueryError list> =
    let errIfNone = [ QueryError.Missing paramName ]
    Query.GetBoundInt(paramName, ctx, ?min=min, ?max=max)
    |> Result.bind (Result.requireSome errIfNone)

  /// Parses the JSON-API 'sort' query parameter according to the specified map.
  /// Values that do not exist as keys in the map will give QueryErr.InvalidEnum
  /// where allowedValues will be the map keys.
  static member GetSortList
      ( valueMap: Map<string, 'a>,
        ctx: HttpContext
      ) : Result<('a * QuerySort) list option, QueryError list> =
    match ctx.Request.Query.TryGetValue "sort" with
    | false, _ -> Ok None
    | true, rawValue ->
        (string rawValue).Split ','
        |> Array.toList
        |> List.map (fun s -> s.Trim())
        |> List.map (fun value ->
            let sortField = if value.StartsWith "-" then value.Substring 1 else value
            let sortDir = if value.StartsWith "-" then QuerySort.Descending else QuerySort.Ascending
            match valueMap |> Map.tryFind sortField with
            | Some x -> Ok [ (x, sortDir) ]
            | None ->
                let allowedValues = valueMap |> Map.toList |> List.map fst
                Error [ QueryError.InvalidEnum ("sort", value, allowedValues) ]
        )
        |> List.reduce Result.combine
        |> Result.map Some

  /// Parses the JSON-API 'sort' query parameter according to the specified map.
  /// Values that do not exist as keys in the map will give QueryErr.InvalidEnum
  /// where allowedValues will be the string values of the map keys.
  static member GetSortList
      ( valueMap: Map<'enum, 'a>,
        ctx: HttpContext
      ) : Result<('a * QuerySort) list option, QueryError list> =
    Query.GetSortList(withStringKeys valueMap, ctx)
  
  /// Parses the JSON-API 'sort' query parameter according to the specified map.
  /// Will return an error if the query parameter is not present. Values that do
  /// not exist as keys in the map will give QueryErr.InvalidEnum where
  /// allowedValues will be the map keys.
  static member RequireSortList
      ( valueMap: Map<string, 'a>,
        ctx: HttpContext
      ) : Result<('a * QuerySort) list, QueryError list> =
    let errIfNone = [ QueryError.Missing "sort" ]
    Query.GetSortList(valueMap, ctx)
    |> Result.bind (Result.requireSome errIfNone)
  
  /// Parses the JSON-API 'sort' query parameter according to the specified map.
  /// Will return an error if the query parameter is not present. Values that do
  /// not exist as keys in the map will give QueryErr.InvalidEnum where
  /// allowedValues will be the string values of map keys.
  static member RequireSortList
      ( valueMap: Map<'enum, 'a>,
        ctx: HttpContext
      ) : Result<('a * QuerySort) list, QueryError list> =
    Query.RequireSortList(withStringKeys valueMap, ctx)

  /// Parses the JSON-API 'sort' query parameter according to the specified map.
  /// Only a single value is supported (not containing commas). Values that do
  /// not exist as keys in the map will give QueryErr.InvalidEnum where
  /// allowedValues will be the map keys.
  static member GetSortSingle
      ( valueMap: Map<string, 'a>,
        ctx: HttpContext
      ) : Result<('a * QuerySort) option, QueryError list> =
    match Query.GetSortList(valueMap, ctx) with
    | Ok None -> Ok None
    | Ok (Some [x]) -> Ok (Some x)
    | Ok (Some xs) -> Error [ QueryError.NotSingular ("sort", xs.Length) ]
    | Error [x] -> Error [x]
    | Error xs -> Error <| QueryError.NotSingular ("sort", xs.Length) :: xs
  
  /// Parses the JSON-API 'sort' query parameter according to the specified map.
  /// Only a single value is supported (not containing commas). Values that do
  /// not exist as keys in the map will give QueryErr.InvalidEnum where
  /// allowedValues will be the string values of the map keys.
  static member GetSortSingle
      ( valueMap: Map<'enum, 'a>,
        ctx: HttpContext
      ) : Result<('a * QuerySort) option, QueryError list> =
    Query.GetSortSingle(withStringKeys valueMap, ctx)

  /// Parses the JSON-API 'sort' query parameter according to the specified map.
  /// Only a single value is supported (not containing commas). Will return an
  /// error if the query parameter is not present. Values that do not exist as
  /// keys in the map will give QueryErr.InvalidEnum where allowedValues will be
  /// the map keys.
  static member RequireSortSingle
      ( valueMap: Map<string, 'a>,
        ctx: HttpContext
      ) : Result<'a * QuerySort, QueryError list> =
    match Query.GetSortList(valueMap, ctx) with
    | Ok None -> Error [ QueryError.Missing "sort" ]
    | Ok (Some [x]) -> Ok x
    | Ok (Some xs) -> Error [ QueryError.NotSingular ("sort", xs.Length) ]
    | Error [x] -> Error [x]
    | Error xs -> Error <| QueryError.NotSingular ("sort", xs.Length) :: xs
  
  /// Parses the JSON-API 'sort' query parameter according to the specified map.
  /// Only a single value is supported (not containing commas). Will return an
  /// error if the query parameter is not present. Values that do not exist as
  /// keys in the map will give QueryErr.InvalidEnum where allowedValues will be
  /// the string values of the map keys.
  static member RequireSortSingle
      ( valueMap: Map<'enum, 'a>,
        ctx: HttpContext
      ) : Result<'a * QuerySort, QueryError list> =
    Query.RequireSortSingle(withStringKeys valueMap, ctx)


[<AutoOpen>]
module Extensions =

  type Query with

    /// Parses a comma-separated query parameter using the specified function.
    static member GetList
        ( paramName: string,
          parse: string -> 'a,
          ctx: HttpContext
        ) : Result<'a list option, QueryError list> =
      Query.GetList(paramName, parse >> Some, ctx)

    /// Parses a required, comma-separated query parameter using the specified
    /// function.
    static member RequireList
        ( paramName: string,
          parse: string -> 'a,
          ctx: HttpContext
        ) : Result<'a list, QueryError list> =
      Query.RequireList(paramName, parse >> Some, ctx)

    /// Parses a singular query parameter (not containing commas) using the
    /// specified function.
    static member GetSingle
        ( paramName: string,
          parse: string -> 'a,
          ctx: HttpContext
        ) : Result<'a option, QueryError list> =
      Query.GetSingle(paramName, parse >> Some, ctx)

    /// Parses a required, singular query parameter (not containing commas) using
    /// the specified function.
    static member RequireSingle
        ( paramName: string,
          parse: string -> 'a,
          ctx: HttpContext
        ) : Result<'a, QueryError list> =
      Query.RequireSingle(paramName, parse >> Some, ctx)
