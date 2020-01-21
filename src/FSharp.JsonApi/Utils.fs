[<AutoOpen>]
module internal FSharp.JsonApi.Utils

open System
open System.Reflection
open System.Collections.Concurrent
open System.Linq.Expressions
open FSharp.JsonSkippable

/// Indicates if the value is null. Boxes the value before checking so even types
/// that cannot normally be null can be checked. Note that this will return true
/// if the value is None.
let isBoxedNull x =
  x |> box |> isNull

/// Indicates if the Skippable-wrapped value is null. Returns false if Skip.
/// Boxes the inner value before checking so even types that cannot normally
/// be null can be checked. Note that this will return true if the inner value
/// is None.
let isIncludedNull = function
  | Include x -> isBoxedNull x
  | _ -> false

/// Matches null values. Boxes the value before checking so even types that
/// cannot normally be null can be checked. Note that this will return true
/// if the value is None.
let (|BoxedNull|_|) x =
  if isBoxedNull x then Some () else None


let ignoreUnitArray (us: unit []) = ()


/// A simple wrapper that allows null keys when memoizing.
[<Struct>] type K<'a> = K of 'a

/// Permanently memoizes computed values.
let inline memoize (f: 'a -> 'b) =
  let d = ConcurrentDictionary<K<'a>, 'b>()
  fun (x: 'a) -> d.GetOrAdd(K x, fun _ -> f x)


/// Returns a fast, untyped getter for the property specified by the PropertyInfo.
/// The getter takes an instance and returns a property value.
let buildUntypedGetter (propertyInfo: PropertyInfo) : obj -> obj =
  let method = propertyInfo.GetMethod
  let objExpr = Expression.Parameter(typeof<obj>, "o")
  let expr =
    Expression.Lambda<Func<obj, obj>>(
      Expression.Convert(
        Expression.Call(
          Expression.Convert(objExpr, method.DeclaringType), method),
          typeof<obj>),
      objExpr)
  let action = expr.Compile()
  fun target -> action.Invoke(target)

/// Returns a fast, untyped setter for the property specified by the PropertyInfo.
/// The setter parameters are an instance and a property value.
let buildUntypedSetter (propertyInfo: PropertyInfo) : obj -> obj -> unit =
  let method = propertyInfo.SetMethod
  let objExpr = Expression.Parameter(typeof<obj>, "o")
  let value = Expression.Parameter(typeof<obj>)
  let expr =
    Expression.Lambda<Action<obj, obj>>(
      Expression.Call(
        Expression.Convert(objExpr, method.DeclaringType),
        method,
        Expression.Convert(value, method.GetParameters().[0].ParameterType)),
      objExpr,
      value)
  let action = expr.Compile()
  fun target value -> action.Invoke(target, value)



module Result =

  /// Combines two results, returning the Error case if at least one of the
  /// inputs is Error.
  let combine res1 res2 =
    match res1, res2 with
    | Ok l1, Ok l2 -> Ok (l1 @ l2)
    | Ok _, Error errs | Error errs, Ok _ -> Error errs
    | Error errs1, Error errs2 -> Error (errs1 @ errs2)


  let requireSome errIfNone = function
    | Some x -> Ok x
    | None -> Error errIfNone



module Async =

  let map f asnc =
    async {
      let! x = asnc
      return f x
    }

  let map2 f asnc1 asnc2 =
    async {
      let! x1 = asnc1
      let! x2 = asnc2
      return f x1 x2
    }



module AsyncResult =

  let map f = Async.map (Result.map f)

  let mapError f = Async.map (Result.mapError f)

  let requireSome errIfNone = Async.map (Result.requireSome errIfNone)

  let bindResult f = Async.map (Result.bind f)



module Option =

  /// Same as ofObj, but boxes before checking for null to avoid type check
  /// for allowed null value.
  let ofObjBoxed x =
    if isNull (box x) then None else Some x


  let traverseResult f opt =
    match opt with
    | None -> Ok None
    | Some v -> f v |> Result.map Some

  let traverseAsyncResult f opt =
    match opt with
    | None -> async.Return (Ok None)
    | Some v -> f v |> AsyncResult.map Some



module ResultOption =

  let map f x =
    match x with
    | Ok (Some x) -> f x |> Some |> Ok
    | Ok None -> Ok None
    | Error err -> Error err

  let bind f x =
    match x with
    | Ok (Some x) -> f x
    | Ok None -> Ok None
    | Error err -> Error err

  let bindResult f x =
    match x with
    | Ok (Some x) -> f x |> Result.map Some
    | Ok None -> Ok None
    | Error err -> Error err


module Array =

  /// Executes the function on each item on the array and returns the input
  /// array.
  let tee f xs =
    xs |> Array.iter f
    xs


module Map =

  /// Merges two maps. On key collision, will arbitrarily use one of the values.
  let merge map1 map2 =
    [Map.toSeq map1; Map.toSeq map2] |> Seq.concat |> Map.ofSeq

  /// Transforms both keys and values in a map. If the new keys collide, only one
  /// (arbitrarily selected) will be present in the output map.
  let mapKv (fKey: 'k1 -> 'k2) (fVal: 'v1 -> 'v2) map =
    map |> Map.toSeq |> Seq.map (fun (k, v) -> fKey k, fVal v) |> Map.ofSeq

  let box map =
    map |> Map.map (fun _ v -> box v)


module Tuple2 =

  let map fst snd (a, b) =
    (fst a, snd b)


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

  /// Splits a string by the given separator.
  let split (separator: string) (str: string) =
    str.Split([| separator |], StringSplitOptions.None) |> List.ofArray

  /// Joins a sequence of strings using the specified separator.
  let join (separator: string) (strings: seq<string>) =
    String.Join(separator, strings)


module ReflectionHelpers =

  open Microsoft.FSharp.Reflection

  let getUnionCases : Type -> UnionCaseInfo [] =
    memoize FSharpType.GetUnionCases

  let getUnionCaseFields =
    memoize FSharpValue.PreComputeUnionReader

  let getUnionTag =
    memoize FSharpValue.PreComputeUnionTagReader

  let getUnionCasesByTag =
    memoize (fun t -> FSharpType.GetUnionCases(t) |> Array.map (fun x -> x.Tag, x) |> dict)

  let getUnionTagOfValue v =
    let t = v.GetType()
    getUnionTag t v

  let inline getUnionFields v =
    let cases = getUnionCasesByTag (v.GetType())
    let tag = getUnionTagOfValue v
    let case = cases.[tag]
    let unionReader = getUnionCaseFields case
    (case, unionReader v)

  let getUnionCaseProperyInfoFields =
    memoize (fun (case: UnionCaseInfo) -> case.GetFields())
