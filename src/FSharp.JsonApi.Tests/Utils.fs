[<AutoOpen>]
module FSharp.JsonApi.Tests.Utils

open FSharp.JsonSkippable

type Skippable<'T> with
  member this.Value =
    match this with
    | Skip -> failwith "No value"
    | Include x -> x

module Map =

  let addOrUpdate (key: 'k) (valueIfMissing: 'v) (update: 'v -> 'v) map =
    match map |> Map.tryFind key with
    | None -> map.Add(key, valueIfMissing)
    | Some x -> map.Add(key, update x)


module Result =

  let error = function
    | Error err -> err
    | Ok _ -> failwith "Expected Error but got Ok"
