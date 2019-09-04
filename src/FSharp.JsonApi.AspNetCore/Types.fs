namespace FSharp.JsonApi


/// Indicates the sort direction of a JSON-API sort field.
[<RequireQualifiedAccess>]
type QuerySort =
  | Ascending
  | Descending
