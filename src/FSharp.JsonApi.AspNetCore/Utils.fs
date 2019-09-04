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
