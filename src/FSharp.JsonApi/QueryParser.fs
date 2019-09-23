namespace FSharp.JsonApi

open System
open System.Text.RegularExpressions


/// Represents errors during query parameter parsing and validation.
[<RequireQualifiedAccess>]
type QueryError =
  /// A query parameter value was not allowed.
  | InvalidEnum of paramName: string * invalidValue: string * allowedValues: string list
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


  module QueryParamName =

    let private globallyAllowed = "[a-zA-Z0-9\u0080-\uFFFF]"
    let private allowedInside = "[a-zA-Z0-9\u0080-\uFFFF\-_ ]"

    let private isKnownJsonApiName =
      let r = Regex("^sort$|^include$|^page\[.+?\]$|^filter\[.+?\]$|^fields\[.+?\]$", RegexOptions.Compiled)
      fun s -> r.IsMatch s

    let private startsWithGloballyAllowed =
      let r = Regex("^" + globallyAllowed, RegexOptions.Compiled)
      fun s -> r.IsMatch s

    let private endsWithGloballyAllowed =
      let r = Regex(globallyAllowed + "$", RegexOptions.Compiled)
      fun s -> r.IsMatch s

    let private containsOnlyAllowed =
      let r = Regex(allowedInside + "+", RegexOptions.Compiled)
      fun s -> r.IsMatch s

    let private containsNonLowercase =
      let r = Regex("[^a-z]", RegexOptions.Compiled)
      fun s -> r.IsMatch s

    let isValid s =
      isKnownJsonApiName s
      || (startsWithGloballyAllowed s
          && endsWithGloballyAllowed s
          && containsOnlyAllowed s
          && containsNonLowercase s)


type QueryParser private (queryParams: Map<string, string>) =

  /// Returns a parser for the query params in the specified map.
  static member FromQueryParamMap (queryParams: Map<string, string>) =
    QueryParser(queryParams)

  /// Indicates if a query parameter name is illegal according to the JSON-API
  /// specification. A custom list of regex patterns can be supplied in order to
  /// whitelist custom parameter names.
  static member IsLegalName(paramName: string, ?customWhitelist: string list) =
    let isCustomWhitelisted () =
      match customWhitelist with
      | None -> false
      | Some list -> list |> List.exists (fun pattern -> Regex.IsMatch(paramName, pattern))
    QueryParamName.isValid paramName || isCustomWhitelisted ()

  /// Parses sparse fieldset query parameters and returns a structure that can
  /// be used to determine whether to include a given field.
  member __.GetFieldsets() : Fieldsets =
    queryParams
    |> Map.filter (fun key _ -> key.StartsWith "fields[" && key.EndsWith "]")
    |> Map.mapKv
        (String.removePrefix "fields[" >> String.removeSuffix "]")
        (String.split "," >> Set.ofList >> Set.map String.trim)

  /// Parses the JSON-API (comma-separated) 'include' query parameter and
  /// returns a list of include paths where each path is a list of relationship
  /// names.
  member __.GetIncludePaths() : IncludePath list =
    match queryParams.TryFind "include" with
    | None -> []
    | Some rawValue ->
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
  member __.GetList
      ( paramName: string,
        valueMap: Map<string, 'a>
      ) : Result<'a list option, QueryError list> =
    match queryParams.TryFind paramName with
    | None -> Ok None
    | Some rawValue ->
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
  /// QueryError.InvalidEnum where allowedValues will be the map keys. If the
  /// query parameter is missing, defaultValue will be used.
  member this.GetList
      ( paramName: string,
        valueMap: Map<string, 'a>,
        defaultValue: 'a list
      ) : Result<'a list, QueryError list> =
    this.GetList(paramName, valueMap)
    |> Result.map (Option.defaultValue defaultValue)

  /// Parses a comma-separated query parameter according to the specified map.
  /// Values that do not exist as keys in the map will give
  /// QueryError.InvalidEnum where allowedValues will be the string values of
  /// the map keys.
  member this.GetList
      ( paramName: string,
        valueMap: Map<'enum, 'a>
      ) : Result<'a list option, QueryError list> =
    this.GetList(paramName, withStringKeys valueMap)

  /// Parses a comma-separated query parameter according to the specified map.
  /// Values that do not exist as keys in the map will give
  /// QueryError.InvalidEnum where allowedValues will be the string values of
  /// the map keys. If the query parameter is missing, defaultValue will be
  /// used.
  member this.GetList
      ( paramName: string,
        valueMap: Map<'enum, 'a>,
        defaultValue: 'a list
      ) : Result<'a list, QueryError list> =
    this.GetList(paramName, withStringKeys valueMap)
    |> Result.map (Option.defaultValue defaultValue)

  /// Parses a comma-separated query parameter using the specified function.
  member __.GetList
      ( paramName: string,
        tryParse: string -> 'a option
      ) : Result<'a list option, QueryError list> =
    match queryParams.TryFind paramName with
    | None -> Ok None
    | Some rawValue ->
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

  /// Parses a comma-separated query parameter using the specified function. If
  /// the query parameter is missing, defaultValue will be used.
  member this.GetList
      ( paramName: string,
        tryParse: string -> 'a option,
        defaultValue: 'a list
      ) : Result<'a list, QueryError list> =
    this.GetList(paramName, tryParse)
    |> Result.map (Option.defaultValue defaultValue)

  /// Parses a comma-separated query parameter using the specified function. The
  /// Error string will be available as errMsg in the returned
  /// QueryError.InvalidParsed.
  member __.GetList
      ( paramName: string,
        tryParse: string -> Result<'a, string>
      ) : Result<'a list option, QueryError list> =
    match queryParams.TryFind paramName with
    | None -> Ok None
    | Some rawValue ->
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

  /// Parses a comma-separated query parameter using the specified function. The
  /// Error string will be available as errMsg in the returned
  /// QueryError.InvalidParsed. If the query parameter is missing, defaultValue
  /// will be used.
  member this.GetList
      ( paramName: string,
        tryParse: string -> Result<'a, string>,
        defaultValue: 'a list
      ) : Result<'a list, QueryError list> =
    this.GetList(paramName, tryParse)
    |> Result.map (Option.defaultValue defaultValue)

  /// Parses a comma-separated query parameter as a list of strings.
  member this.GetList
      ( paramName: string
      ) : Result<string list option, QueryError list> =
    this.GetList(paramName, Some)

  /// Parses a comma-separated query parameter as a list of strings. If the
  /// query parameter is missing, defaultValue will be used.
  member this.GetList
      ( paramName: string,
        defaultValue: string list
      ) : Result<string list, QueryError list> =
    this.GetList(paramName, Some)
    |> Result.map (Option.defaultValue defaultValue)

  /// Parses a required, comma-separated query parameter according to the
  /// specified map. Values that do not exist as keys in the map will give
  /// QueryError.InvalidEnum where allowedValues will be the map keys.
  member this.RequireList
      ( paramName: string,
        valueMap: Map<string, 'a>
      ) : Result<'a list, QueryError list> =
    let errIfNone = [QueryError.Missing paramName]
    this.GetList(paramName, valueMap)
    |> Result.bind (Result.requireSome errIfNone)

  /// Parses a required, comma-separated query parameter according to the
  /// specified map. Values that do not exist as keys in the map will give
  /// QueryError.InvalidEnum where allowedValues will be the string values of
  /// the map keys.
  member this.RequireList
      ( paramName: string,
        valueMap: Map<'enum, 'a>
      ) : Result<'a list, QueryError list> =
    this.RequireList(paramName, withStringKeys valueMap)

  /// Parses a required, comma-separated query parameter using the specified
  /// function.
  member this.RequireList
      ( paramName: string,
        tryParse: string -> 'a option
      ) : Result<'a list, QueryError list> =
    let errIfNone = [QueryError.Missing paramName]
    this.GetList(paramName, tryParse)
    |> Result.bind (Result.requireSome errIfNone)

  /// Parses a required, comma-separated query parameter using the specified
  /// function. The Error string will be available as errMsg in the returned
  /// QueryError.InvalidParsed.
  member this.RequireList
      ( paramName: string,
        tryParse: string -> Result<'a, string>
      ) : Result<'a list, QueryError list> =
    let errIfNone = [QueryError.Missing paramName]
    this.GetList(paramName, tryParse)
    |> Result.bind (Result.requireSome errIfNone)

  /// Parses a required, comma-separated query parameter as a list of strings.
  member this.RequireList
      ( paramName: string
      ) : Result<string list, QueryError list> =
    this.RequireList(paramName, Some)

  /// Parses a singular query parameter (not containing commas) according to the
  /// specified map. Values that do not exist as keys in the map will give
  /// QueryError.InvalidEnum where allowedValues will be the map keys.
  member this.GetSingle
      ( paramName: string,
        valueMap: Map<string, 'a>
      ) : Result<'a option, QueryError list> =
    match this.GetList(paramName, valueMap) with
    | Ok None -> Ok None
    | Ok (Some [x]) -> Ok (Some x)
    | Ok (Some xs) -> Error [ QueryError.NotSingular (paramName, xs.Length) ]
    | Error [x] -> Error [x]
    | Error xs -> Error <| QueryError.NotSingular (paramName, xs.Length) :: xs

  /// Parses a singular query parameter (not containing commas) according to the
  /// specified map. Values that do not exist as keys in the map will give
  /// QueryError.InvalidEnum where allowedValues will be the map keys. If the
  /// query parameter is missing, defaultValue will be used.
  member this.GetSingle
      ( paramName: string,
        valueMap: Map<string, 'a>,
        defaultValue: 'a
      ) : Result<'a, QueryError list> =
    this.GetSingle(paramName, valueMap)
    |> Result.map (Option.defaultValue defaultValue)

  /// Parses a singular query parameter (not containing commas) according to the
  /// specified map. Values that do not exist as keys in the map will give
  /// QueryError.InvalidEnum where allowedValues will be the string values of
  /// the map keys.
  member this.GetSingle
      ( paramName: string,
        valueMap: Map<'enum, 'a>
      ) : Result<'a option, QueryError list> =
    this.GetSingle(paramName, withStringKeys valueMap)

  /// Parses a singular query parameter (not containing commas) according to the
  /// specified map. Values that do not exist as keys in the map will give
  /// QueryError.InvalidEnum where allowedValues will be the string values of
  /// the map keys. If the query parameter is missing, defaultValue will be
  /// used.
  member this.GetSingle
      ( paramName: string,
        valueMap: Map<'enum, 'a>,
        defaultValue: 'a
      ) : Result<'a, QueryError list> =
    this.GetSingle(paramName, withStringKeys valueMap)
    |> Result.map (Option.defaultValue defaultValue)

  /// Parses a singular query parameter (not containing commas) using the
  /// specified function.
  member this.GetSingle
      ( paramName: string,
        tryParse: string -> 'a option
      ) : Result<'a option, QueryError list> =
    match this.GetList(paramName, tryParse) with
    | Ok None -> Ok None
    | Ok (Some [x]) -> Ok (Some x)
    | Ok (Some xs) -> Error [ QueryError.NotSingular (paramName, xs.Length) ]
    | Error [x] -> Error [x]
    | Error xs -> Error <| QueryError.NotSingular (paramName, xs.Length) :: xs

  /// Parses a singular query parameter (not containing commas) using the
  /// specified function. If the query parameter is missing, defaultValue will
  /// be used.
  member this.GetSingle
      ( paramName: string,
        tryParse: string -> 'a option,
        defaultValue: 'a
      ) : Result<'a, QueryError list> =
    this.GetSingle(paramName, tryParse)
    |> Result.map (Option.defaultValue defaultValue)

  /// Parses a singular query parameter (not containing commas) using the
  /// specified function. The Error string will be available as errMsg in the
  /// returned QueryError.InvalidParsed.
  member this.GetSingle
      ( paramName: string,
        tryParse: string -> Result<'a, string>
      ) : Result<'a option, QueryError list> =
    match this.GetList(paramName, tryParse) with
    | Ok None -> Ok None
    | Ok (Some [x]) -> Ok (Some x)
    | Ok (Some xs) -> Error [ QueryError.NotSingular (paramName, xs.Length) ]
    | Error [x] -> Error [x]
    | Error xs -> Error <| QueryError.NotSingular (paramName, xs.Length) :: xs

  /// Parses a singular query parameter (not containing commas) using the
  /// specified function. The Error string will be available as errMsg in the
  /// returned QueryError.InvalidParsed. If the query parameter is missing,
  /// defaultValue will be used.
  member this.GetSingle
      ( paramName: string,
        tryParse: string -> Result<'a, string>,
        defaultValue: 'a
      ) : Result<'a, QueryError list> =
    this.GetSingle(paramName, tryParse)
    |> Result.map (Option.defaultValue defaultValue)

  /// Parses a query parameter as single string (not containing commas).
  member this.GetSingle(paramName: string) : Result<string option, QueryError list> =
    this.GetSingle(paramName, Some)

  /// Parses a query parameter as single string (not containing commas). If the
  /// query parameter is missing, defaultValue will be used.
  member this.GetSingle
      ( paramName: string,
        defaultValue: string
      ) : Result<string, QueryError list> =
    this.GetSingle(paramName, Some)
    |> Result.map (Option.defaultValue defaultValue)

  /// Parses a required, singular query parameter (not containing commas)
  /// according to the specified map. Values that do not exist as keys in the
  /// map will give QueryError.InvalidEnum where allowedValues will be the map
  /// keys.
  member this.RequireSingle
      ( paramName: string,
        valueMap: Map<string, 'a>
      ) : Result<'a, QueryError list> =
    match this.GetList(paramName, valueMap) with
    | Ok None -> Error [QueryError.Missing paramName]
    | Ok (Some [x]) -> Ok x
    | Ok (Some xs) -> Error [ QueryError.NotSingular (paramName, xs.Length) ]
    | Error [x] -> Error [x]
    | Error xs -> Error <| QueryError.NotSingular (paramName, xs.Length) :: xs

  /// Parses a required, singular query parameter (not containing commas)
  /// according to the specified map. Values that do not exist as keys in the
  /// map will give QueryError.InvalidEnum where allowedValues will be the
  /// string values of the map keys.
  member this.RequireSingle
      ( paramName: string,
        valueMap: Map<'enum, 'a>
      ) : Result<'a, QueryError list> =
    this.RequireSingle(paramName, withStringKeys valueMap)

  /// Parses a required, singular query parameter (not containing commas) using
  /// the specified function.
  member this.RequireSingle
      ( paramName: string,
        tryParse: string -> 'a option
      ) : Result<'a, QueryError list> =
    match this.GetList(paramName, tryParse) with
    | Ok None -> Error [QueryError.Missing paramName]
    | Ok (Some [x]) -> Ok x
    | Ok (Some xs) -> Error [ QueryError.NotSingular (paramName, xs.Length) ]
    | Error [x] -> Error [x]
    | Error xs -> Error <| QueryError.NotSingular (paramName, xs.Length) :: xs

  /// Parses a required, singular query parameter (not containing commas) using
  /// the specified function. The Error string will be available as errMsg in
  /// the returned QueryError.InvalidParsed.
  member this.RequireSingle
      ( paramName: string,
        tryParse: string -> Result<'a, string>
      ) : Result<'a, QueryError list> =
    match this.GetList(paramName, tryParse) with
    | Ok None -> Error [QueryError.Missing paramName]
    | Ok (Some [x]) -> Ok x
    | Ok (Some xs) -> Error [ QueryError.NotSingular (paramName, xs.Length) ]
    | Error [x] -> Error [x]
    | Error xs -> Error <| QueryError.NotSingular (paramName, xs.Length) :: xs

  /// Parses a required query parameter as single string (not containing
  /// commas).
  member this.RequireSingle(paramName: string) : Result<string, QueryError list> =
    this.RequireSingle(paramName, Some)

  /// Parses a query parameter as a single boolean ("true"/"false").
  member this.GetBool(paramName: string) : Result<bool option, QueryError list> =
    this.GetSingle(paramName, boolMap)

  /// Parses a query parameter as a single boolean ("true"/"false"). If the
  /// query parameter is missing, defaultValue will be used.
  member this.GetBool
      ( paramName: string,
        defaultValue: bool
      ) : Result<bool, QueryError list> =
    this.GetSingle(paramName, boolMap)
    |> Result.map (Option.defaultValue defaultValue)

  /// Parses a required query parameter as a single boolean ("true"/"false").
  member this.RequireBool
      ( paramName: string
      ) : Result<bool, QueryError list> =
    this.RequireSingle(paramName, boolMap)

  /// Parses the given query parameter as an integer between optional min and
  /// max values.
  member __.GetBoundInt
      ( paramName: string,
        ?min: int,
        ?max: int
      ) : Result<int option, QueryError list> =
    match queryParams.TryFind paramName with
    | None -> Ok None
    | Some rawValue ->
        let value = string rawValue
        match Int32.TryParse value, min, max with
        | (true, i), Some min, _ when i < min ->
            Error [ QueryError.TooSmall (paramName, i, min) ]
        | (true, i), _, Some max when i > max ->
            Error [ QueryError.TooLarge (paramName, i, max) ]
        | (true, i), _, _ -> Ok (Some i)
        | (false, _), _, _ ->
            Error [ QueryError.InvalidParsed (paramName, value, None) ]

  /// Parses the given query parameter as an integer between optional min and
  /// max values. If the query parameter is missing, defaultValue will be used.
  member this.GetDefaultBoundInt
      ( paramName: string,
        defaultValue: int,
        ?min: int,
        ?max: int
      ) : Result<int, QueryError list> =
    this.GetBoundInt(paramName, ?min=min, ?max=max)
    |> Result.map (Option.defaultValue defaultValue)

  /// Parses the given required query parameter as an integer between optional
  /// min and max values.
  member this.RequireBoundInt
      ( paramName: string,
        ?min: int,
        ?max: int
      ) : Result<int, QueryError list> =
    let errIfNone = [QueryError.Missing paramName]
    this.GetBoundInt(paramName, ?min=min, ?max=max)
    |> Result.bind (Result.requireSome errIfNone)

  /// Parses the JSON-API 'sort' query parameter according to the specified map.
  /// Values that do not exist as keys in the map will give QueryErr.InvalidEnum
  /// where allowedValues will be the map keys. The boolean indicates
  /// whether to sort descending (true) or ascending (false).
  member __.GetSortList
      ( valueMap: Map<string, 'a>
      ) : Result<('a * bool) list option, QueryError list> =
    match queryParams.TryFind "sort" with
    | None -> Ok None
    | Some rawValue ->
        (string rawValue).Split ','
        |> Array.toList
        |> List.map (fun s -> s.Trim())
        |> List.map (fun value ->
            let sortField = if value.StartsWith "-" then value.Substring 1 else value
            let sortDir = if value.StartsWith "-" then true else false
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
  /// where allowedValues will be the map keys. If the query parameter is
  /// missing, defaultValue will be used.The boolean indicates whether
  /// to sort descending (true) or ascending (false).
  member this.GetSortList
      ( valueMap: Map<string, 'a>,
        defaultValue: ('a * bool) list
      ) : Result<('a * bool) list, QueryError list> =
    this.GetSortList(valueMap)
    |> Result.map (Option.defaultValue defaultValue)

  /// Parses the JSON-API 'sort' query parameter according to the specified map.
  /// Values that do not exist as keys in the map will give QueryErr.InvalidEnum
  /// where allowedValues will be the string values of the map keys. The
  /// boolean indicates whether to sort descending (true) or ascending
  /// (false).
  member this.GetSortList
      ( valueMap: Map<'enum, 'a>
      ) : Result<('a * bool) list option, QueryError list> =
    this.GetSortList(withStringKeys valueMap)

  /// Parses the JSON-API 'sort' query parameter according to the specified map.
  /// Values that do not exist as keys in the map will give QueryErr.InvalidEnum
  /// where allowedValues will be the string values of the map keys. If the
  /// query parameter is missing, defaultValue will be used. The boolean
  /// indicates whether to sort descending (true) or ascending (false).
  member this.GetSortList
      ( valueMap: Map<'enum, 'a>,
        defaultValue: ('a * bool) list
      ) : Result<('a * bool) list, QueryError list> =
    this.GetSortList(withStringKeys valueMap)
    |> Result.map (Option.defaultValue defaultValue)
  
  /// Parses the JSON-API 'sort' query parameter according to the specified map.
  /// Will return an error if the query parameter is not present. Values that do
  /// not exist as keys in the map will give QueryErr.InvalidEnum where
  /// allowedValues will be the map keys. The boolean indicates whether
  /// to sort descending (true) or ascending (false).
  member this.RequireSortList
      ( valueMap: Map<string, 'a>
      ) : Result<('a * bool) list, QueryError list> =
    let errIfNone = [ QueryError.Missing "sort" ]
    this.GetSortList(valueMap)
    |> Result.bind (Result.requireSome errIfNone)
  
  /// Parses the JSON-API 'sort' query parameter according to the specified map.
  /// Will return an error if the query parameter is not present. Values that do
  /// not exist as keys in the map will give QueryErr.InvalidEnum where
  /// allowedValues will be the string values of map keys. The booleans
  /// indicates whether to sort descending (true) or ascending (false).
  member this.RequireSortList
      ( valueMap: Map<'enum, 'a>
      ) : Result<('a * bool) list, QueryError list> =
    this.RequireSortList(withStringKeys valueMap)

  /// Parses the JSON-API 'sort' query parameter according to the specified map.
  /// Only a single value is supported (not containing commas). Values that do
  /// not exist as keys in the map will give QueryErr.InvalidEnum where
  /// allowedValues will be the map keys. The boolean indicates whether
  /// to sort descending (true) or ascending (false).
  member this.GetSortSingle
      ( valueMap: Map<string, 'a>
      ) : Result<('a * bool) option, QueryError list> =
    match this.GetSortList(valueMap) with
    | Ok None -> Ok None
    | Ok (Some [x]) -> Ok (Some x)
    | Ok (Some xs) -> Error [ QueryError.NotSingular ("sort", xs.Length) ]
    | Error [x] -> Error [x]
    | Error xs -> Error <| QueryError.NotSingular ("sort", xs.Length) :: xs

  /// Parses the JSON-API 'sort' query parameter according to the specified map.
  /// Only a single value is supported (not containing commas). Values that do
  /// not exist as keys in the map will give QueryErr.InvalidEnum where
  /// allowedValues will be the map keys. If the query parameter is missing,
  /// defaultValue will be used. The boolean indicates whether to sort
  /// descending (true) or ascending (false).
  member this.GetSortSingle
      ( valueMap: Map<string, 'a>,
        defaultValue: 'a * bool
      ) : Result<'a * bool, QueryError list> =
    this.GetSortSingle(valueMap)
    |> Result.map (Option.defaultValue defaultValue)
  
  /// Parses the JSON-API 'sort' query parameter according to the specified map.
  /// Only a single value is supported (not containing commas). Values that do
  /// not exist as keys in the map will give QueryErr.InvalidEnum where
  /// allowedValues will be the string values of the map keys. The boolean
  /// indicates whether to sort descending (true) or ascending (false).
  member this.GetSortSingle
      ( valueMap: Map<'enum, 'a>
      ) : Result<('a * bool) option, QueryError list> =
    this.GetSortSingle(withStringKeys valueMap)

  /// Parses the JSON-API 'sort' query parameter according to the specified map.
  /// Only a single value is supported (not containing commas). Values that do
  /// not exist as keys in the map will give QueryErr.InvalidEnum where
  /// allowedValues will be the string values of the map keys. If the query
  /// parameter is missing, defaultValue will be used. The boolean indicates
  /// whether to sort descending (true) or ascending (false).
  member this.GetSortSingle
      ( valueMap: Map<'enum, 'a>,
        defaultValue: 'a * bool
      ) : Result<'a * bool, QueryError list> =
    this.GetSortSingle(withStringKeys valueMap)
    |> Result.map (Option.defaultValue defaultValue)

  /// Parses the JSON-API 'sort' query parameter according to the specified map.
  /// Only a single value is supported (not containing commas). Will return an
  /// error if the query parameter is not present. Values that do not exist as
  /// keys in the map will give QueryErr.InvalidEnum where allowedValues will be
  /// the map keys. The boolean indicates whether to sort descending
  /// (true) or ascending (false).
  member this.RequireSortSingle
      ( valueMap: Map<string, 'a>
      ) : Result<'a * bool, QueryError list> =
    match this.GetSortList(valueMap) with
    | Ok None -> Error [ QueryError.Missing "sort" ]
    | Ok (Some [x]) -> Ok x
    | Ok (Some xs) -> Error [ QueryError.NotSingular ("sort", xs.Length) ]
    | Error [x] -> Error [x]
    | Error xs -> Error <| QueryError.NotSingular ("sort", xs.Length) :: xs
  
  /// Parses the JSON-API 'sort' query parameter according to the specified map.
  /// Only a single value is supported (not containing commas). Will return an
  /// error if the query parameter is not present. Values that do not exist as
  /// keys in the map will give QueryErr.InvalidEnum where allowedValues will be
  /// the string values of the map keys. The boolean indicates whether
  /// to sort descending (true) or ascending (false).
  member this.RequireSortSingle
      ( valueMap: Map<'enum, 'a>
      ) : Result<'a * bool, QueryError list> =
    this.RequireSortSingle(withStringKeys valueMap)


[<AutoOpen>]
module QueryExtensions =

  type QueryParser with

    /// Parses a comma-separated query parameter using the specified function.
    member this.GetList
        ( paramName: string,
          parse: string -> 'a
        ) : Result<'a list option, QueryError list> =
      this.GetList(paramName, parse >> Some)

    /// Parses a comma-separated query parameter using the specified function.
    /// If the query parameter is missing, defaultValue will be used.
    member this.GetList
        ( paramName: string,
          parse: string -> 'a,
          defaultValue: 'a list
        ) : Result<'a list, QueryError list> =
      this.GetList(paramName, parse >> Some)
      |> Result.map (Option.defaultValue defaultValue)

    /// Parses a required, comma-separated query parameter using the specified
    /// function.
    member this.RequireList
        ( paramName: string,
          parse: string -> 'a
        ) : Result<'a list, QueryError list> =
      this.RequireList(paramName, parse >> Some)

    /// Parses a singular query parameter (not containing commas) using the
    /// specified function.
    member this.GetSingle
        ( paramName: string,
          parse: string -> 'a
        ) : Result<'a option, QueryError list> =
      this.GetSingle(paramName, parse >> Some)

    /// Parses a singular query parameter (not containing commas) using the
    /// specified function. If the query parameter is missing, defaultValue will
    /// be used.
    member this.GetSingle
        ( paramName: string,
          parse: string -> 'a,
          defaultValue: 'a
        ) : Result<'a, QueryError list> =
      this.GetSingle(paramName, parse >> Some)
      |> Result.map (Option.defaultValue defaultValue)

    /// Parses a required, singular query parameter (not containing commas) using
    /// the specified function.
    member this.RequireSingle
        ( paramName: string,
          parse: string -> 'a
        ) : Result<'a, QueryError list> =
      this.RequireSingle(paramName, parse >> Some)
