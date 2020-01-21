namespace FSharp.JsonApi

open System

[<RequireQualifiedAccess>]
module Uri =

  /// Adds a path segment to a URI, adding a slash if needed. Does not change
  /// query parameters or fragment.
  let addSegment (segment: string) (uri: Uri) =
    let b = UriBuilder(uri)
    b.Path <- (b.Path.TrimEnd '/') + "/" + (segment.TrimStart '/')
    b.Uri

  /// Adds several path segments to a URI, adding slashes as needed. Does not
  /// change query parameters or fragment.
  let addSegments (segments: #seq<string>) (uri: Uri) =
    segments |> Seq.fold (fun uri segment -> addSegment segment uri) uri

  /// Adds a name-value pair to a URI's query string. A name may be added multiple
  /// times.
  let addQuery (key: string) (value: string) (uri: Uri) =
    let b = UriBuilder(uri)
    b.Query <-
      b.Query.TrimStart('?')
      |> String.split "&"
      |> List.map (String.split "=")
      |> fun kvs -> kvs @ [[key; value]]
      |> List.map (String.join "=")
      |> String.join "&"
      |> fun s -> if s = "" then s else "?" + s
    b.Uri

  /// Sets a name-value pair in the URI's query string, overwriting any existing
  /// values for that name.
  let setQuery (key: string) (value: string) (uri: Uri) =
    let b = UriBuilder(uri)
    b.Query <-
      b.Query.TrimStart('?')
      |> String.split "&"
      |> List.map (String.split "=")
      |> List.filter (fun kv -> kv |> List.tryItem 0 |> (<>) (Some key))
      |> fun kvs -> kvs @ [[key; value]]
      |> List.map (String.join "=")
      |> String.join "&"
      |> fun s -> if s = "" then s else "?" + s
    b.Uri


[<AutoOpen>]
module UriExtensions =

  type Uri with

    /// Adds a path segment to a URI, adding a slash if needed. Does not change
    /// query parameters or fragment.
    member this.AddSegment segment =
      this |> Uri.addSegment segment

    /// Adds several path segments to a URI, adding slashes as needed. Does not
    /// change query parameters or fragment.
    member this.AddSegments segments =
      this |> Uri.addSegments segments

    /// Adds a name-value pair to a URI's query string. A name may be added
    /// multiple times.
    member this.AddQuery (key, value) =
      this |> Uri.addQuery key value

    /// Sets a name-value pair in the URI's query string, overwriting any
    /// existing values for that name.
    member this.SetQuery (key, value) =
      this |> Uri.setQuery key value
