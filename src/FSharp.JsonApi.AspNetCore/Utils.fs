[<AutoOpen>]
module internal FSharp.JsonApi.AspNetCoreUtils


module Result =

  let combine res1 res2 =
    match res1, res2 with
    | Ok l1, Ok l2 -> Ok (l1 @ l2)
    | Ok _, Error errs | Error errs, Ok _ -> Error errs
    | Error errs1, Error errs2 -> Error (errs1 @ errs2)

  let requireSome errIfNone = function
    | Some x -> Ok x
    | None -> Error errIfNone


module Map =

  /// Transforms both keys and values in a map. If the new keys collide, only one
  /// (arbitrarily selected) will be present in the output map.
  let mapKv (fKey: 'k1 -> 'k2) (fVal: 'v1 -> 'v2) map =
    map |> Map.toSeq |> Seq.map (fun (k, v) -> fKey k, fVal v) |> Map.ofSeq


module String =

  /// Removes the given prefix from the string if the string starts with the
  /// prefix. If not, the string is returned unmodified.
  let removePrefix prefix (str: string) =
    if str.StartsWith prefix then str.Substring prefix.Length else str

  /// Removes the given suffix from the string if the string ends with the suffix.
  /// If not, the string is returned unmodified.
  let removeSuffix suffix (str: string) =
    if str.EndsWith suffix then str.Substring(0, str.Length - suffix.Length) else str

  /// Trims whitespace from both ends of a string.
  let trim (str: string) =
    str.Trim ()
