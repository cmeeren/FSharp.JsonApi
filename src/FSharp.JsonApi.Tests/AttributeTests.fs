module FSharp.JsonApi.Tests.AttributeTests

open System
open Xunit
open Hedgehog
open Swensen.Unquote
open FSharp.JsonSkippable
open FSharp.JsonApi


let pointer attrName =
  sprintf "/data/attributes/%s" attrName


let hasInvalidNullWith pointer isOverride = function
  | Ok _ -> false
  | Error errs -> errs |> List.contains (RequestDocumentError.InvalidNull (pointer, isOverride))

let hasInvalidNull = function
  | Ok _ -> false
  | Error errs -> errs |> List.exists (function RequestDocumentError.InvalidNull _ -> true | _ -> false)


module GetNonNull_raw =

  [<Fact>]
  let ``returns InvalidNull with correct pointer and override when value is Include None`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let result = Attribute.GetNonNull(name, Include None)
      test <@ result |> hasInvalidNullWith (pointer name) true @>
    }

  [<Fact>]
  let ``does not return InvalidNull when value is Skip`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let result = Attribute.GetNonNull(name, Skip)
      test <@ result |> hasInvalidNull |> not @>
    }

  [<Fact>]
  let ``does not return InvalidNull when value is Include Some`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let! value = GenX.auto<int>
      let result = Attribute.GetNonNull(name, Include (Some value))
      test <@ result |> hasInvalidNull |> not @>
    }


module GetNonNull_stringMap =

  [<Fact>]
  let ``returns InvalidNull with correct pointer and override when value is Include None`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let! map = GenX.auto<Map<string, int>>
      let result = Attribute.GetNonNull(name, Include None, map)
      test <@ result |> hasInvalidNullWith (pointer name) true @>
    }

  [<Fact>]
  let ``does not return InvalidNull when value is Skip`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let! map = GenX.auto<Map<string, int>>
      let result = Attribute.GetNonNull(name, Skip, map)
      test <@ result |> hasInvalidNull |> not @>
    }

  [<Fact>]
  let ``does not return InvalidNull when value is Include Some`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let! value = GenX.auto<string>
      let! map = GenX.auto<Map<string, int>>
      let result = Attribute.GetNonNull(name, Include (Some value), map)
      test <@ result |> hasInvalidNull |> not @>
    }


module GetNonNull_enumMap =

  type Enum =
    | A = 0
    | B = 1

  [<Fact>]
  let ``returns InvalidNull with correct pointer and override when value is Include None`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let! map = GenX.auto<Map<Enum, int>>
      let result = Attribute.GetNonNull(name, Include None, map)
      test <@ result |> hasInvalidNullWith (pointer name) true @>
    }

  [<Fact>]
  let ``does not return InvalidNull when value is Skip`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let! map = GenX.auto<Map<Enum, int>>
      let result = Attribute.GetNonNull(name, Skip, map)
      test <@ result |> hasInvalidNull |> not @>
    }

  [<Fact>]
  let ``does not return InvalidNull when value is Include Some`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let! value = GenX.auto<Enum>
      let! map = GenX.auto<Map<Enum, int>>
      let result = Attribute.GetNonNull(name, Include (Some value), map)
      test <@ result |> hasInvalidNull |> not @>
    }


module GetNonNull_parseResult =

  [<Fact>]
  let ``returns InvalidNull with correct pointer and override when value is Include None`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let tryParse _ : Result<_, _> = failwith ""
      let result = Attribute.GetNonNull(name, Include None, tryParse)
      test <@ result |> hasInvalidNullWith (pointer name) true @>
    }

  [<Fact>]
  let ``does not return InvalidNull when value is Skip`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let tryParse _ : Result<_, _> = failwith ""
      let result = Attribute.GetNonNull(name, Skip, tryParse)
      test <@ result |> hasInvalidNull |> not @>
    }

  [<Fact>]
  let ``does not return InvalidNull when value is Include Some`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let! value = GenX.auto<int>
      let tryParse x : Result<_, _> = Ok x
      let result = Attribute.GetNonNull(name, Include (Some value), tryParse)
      test <@ result |> hasInvalidNull |> not @>
    }


module GetNonNull_parseOption =

  [<Fact>]
  let ``returns InvalidNull with correct pointer and override when value is Include None`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let tryParse _ : _ option = failwith ""
      let result = Attribute.GetNonNull(name, Include None, tryParse)
      test <@ result |> hasInvalidNullWith (pointer name) true @>
    }

  [<Fact>]
  let ``does not return InvalidNull when value is Skip`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let tryParse _ : _ option = failwith ""
      let result = Attribute.GetNonNull(name, Skip, tryParse)
      test <@ result |> hasInvalidNull |> not @>
    }

  [<Fact>]
  let ``does not return InvalidNull when value is Include Some`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let! value = GenX.auto<int>
      let tryParse x : _ option = Some x
      let result = Attribute.GetNonNull(name, Include (Some value), tryParse)
      test <@ result |> hasInvalidNull |> not @>
    }
