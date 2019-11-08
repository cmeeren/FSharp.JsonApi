module FSharp.JsonApi.Tests.AttributeTests

open Xunit
open Hedgehog
open Swensen.Unquote
open FSharp.JsonSkippable
open FSharp.JsonApi


let pointer attrName =
  sprintf "/data/attributes/%s" attrName


module Get_value =

  [<Fact>]
  let ``returns Ok None if value is Skip`` () =
    Property.check <| property {
      let (value: Skippable<char>) = Skip
      let result = Attribute.Get(value)
      test <@ result = Ok None @>
    }

  [<Fact>]
  let ``returns Ok Some with the Include value if value is Include`` () =
    Property.check <| property {
      let! value = GenX.auto<char>
      let result = Attribute.Get(Include value)
      test <@ result = Ok (Some value) @>
    }


module Get_nonOption_stringMap =

  [<Fact>]
  let ``returns Ok None if value is Skip`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let (value: Skippable<string>) = Skip
      let! valueMap = GenX.auto<Map<string, int>>

      let result = Attribute.Get(name, value, valueMap)

      test <@ result = Ok None @>
    }

  [<Fact>]
  let ``returns Ok Some with mapped value if value is Include and present in map`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let! value = GenX.auto<string>
      let! mapped = GenX.auto<int>
      let! valueMap = GenX.auto<Map<string, int>> |> Gen.map (Map.add value mapped)

      let result = Attribute.Get(name, Include value, valueMap)

      test <@ result = Ok (Some mapped) @>
    }

  [<Fact>]
  let ``returns InvalidEnum with correct data when Include value is not in map`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let! value = GenX.auto<string>
      let! valueMap = GenX.auto<Map<string, int>> |> Gen.map (Map.remove value)

      let allowedValues = valueMap |> Map.toList |> List.map fst
      let expectedErr = RequestDocumentError.AttributeInvalidEnum (pointer name, value, allowedValues)

      let result = Attribute.Get(name, Include value, valueMap)

      test <@ result |> Result.error |> List.contains expectedErr @>
    }


module Get_nonOption_enumMap =

  type Enum =
    | A = 0
    | B = 1

  [<Fact>]
  let ``returns Ok None if value is Skip`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let (value: Skippable<Enum>) = Skip
      let! valueMap = GenX.auto<Map<Enum, int>>

      let result = Attribute.Get(name, value, valueMap)

      test <@ result = Ok None @>
    }

  [<Fact>]
  let ``returns Ok Some with mapped value if value is Include and present in map`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let! value = GenX.auto<Enum>
      let! mapped = GenX.auto<int>
      let! valueMap = GenX.auto<Map<Enum, int>> |> Gen.map (Map.add value mapped)

      let result = Attribute.Get(name, Include value, valueMap)

      test <@ result = Ok (Some mapped) @>
    }

  [<Fact>]
  let ``returns InvalidEnum with correct data when Include value is not in map`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let! value = GenX.auto<Enum>
      let! valueMap = GenX.auto<Map<Enum, int>> |> Gen.map (Map.remove value)

      let allowedValues = valueMap |> Map.toList |> List.map (fst >> string)
      let expectedErr = RequestDocumentError.AttributeInvalidEnum (pointer name, string value, allowedValues)

      let result = Attribute.Get(name, Include value, valueMap)

      test <@ result |> Result.error |> List.contains expectedErr @>
    }


module Get_nonOption_parseDirect =

  [<Fact>]
  let ``returns Ok None if value is Skip`` () =
    Property.check <| property {
      let (value: Skippable<char>) = Skip
      let tryParse (_: char) : int = failwith "Should not be called"

      let result = Attribute.Get(value, tryParse)

      test <@ result = Ok None @>
    }

  [<Fact>]
  let ``returns Ok Some with parsed value if value is Include`` () =
    Property.check <| property {
      let! value = GenX.auto<char>
      let! parsed = GenX.auto<int>
      let tryParse (_: char) : int = parsed

      let result = Attribute.Get(Include value, tryParse)

      test <@ result = Ok (Some parsed) @>
    }


module Get_nonOption_parseAsyncDirect =

  [<Fact>]
  let ``returns Ok None if value is Skip`` () =
    Property.check <| property {
      let (value: Skippable<char>) = Skip
      let tryParse (_: char) : Async<int> = failwith "Should not be called"

      let result = Attribute.Get(value, tryParse) |> Async.RunSynchronously

      test <@ result = Ok None @>
    }

  [<Fact>]
  let ``returns Ok Some with parsed value if value is Include`` () =
    Property.check <| property {
      let! value = GenX.auto<char>
      let! parsed = GenX.auto<int>
      let tryParse (_: char) : Async<int> = async.Return parsed

      let result = Attribute.Get(Include value, tryParse) |> Async.RunSynchronously

      test <@ result = Ok (Some parsed) @>
    }


module Get_nonOption_tryParseResult =

  [<Fact>]
  let ``returns Ok None if value is Skip`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let (value: Skippable<char>) = Skip
      let tryParse (_: char) : Result<int, string> = failwith "Should not be called"

      let result = Attribute.Get(name, value, tryParse)

      test <@ result = Ok None @>
    }

  [<Fact>]
  let ``returns Ok Some with parsed value if value is Include and parse returns Ok`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let! value = GenX.auto<char>
      let! parsed = GenX.auto<int>
      let tryParse (_: char) : Result<int, string> = Ok parsed

      let result = Attribute.Get(name, Include value, tryParse)

      test <@ result = Ok (Some parsed) @>
    }

  [<Fact>]
  let ``returns InvalidParsed with correct data if value is Include and parse returns Error`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let! value = GenX.auto<char>
      let! errMsg = GenX.auto<string>
      let tryParse (_: char) : Result<int, string> = Error errMsg

      let expectedErr = RequestDocumentError.AttributeInvalidParsed (pointer name, Some errMsg)

      let result = Attribute.Get(name, Include value, tryParse)

      test <@ result |> Result.error |> List.contains expectedErr @>
    }


module Get_nonOption_tryParseAsyncResult =

  [<Fact>]
  let ``returns Ok None if value is Skip`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let (value: Skippable<char>) = Skip
      let tryParse (_: char) : Async<Result<int, string>> = failwith "Should not be called"

      let result = Attribute.Get(name, value, tryParse) |> Async.RunSynchronously

      test <@ result = Ok None @>
    }

  [<Fact>]
  let ``returns Ok Some with parsed value if value is Include and parse returns Ok`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let! value = GenX.auto<char>
      let! parsed = GenX.auto<int>
      let tryParse (_: char) : Async<Result<int, string>> = async.Return (Ok parsed)

      let result = Attribute.Get(name, Include value, tryParse) |> Async.RunSynchronously

      test <@ result = Ok (Some parsed) @>
    }

  [<Fact>]
  let ``returns InvalidParsed with correct data if value is Include and parse returns Error`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let! value = GenX.auto<char>
      let! errMsg = GenX.auto<string>
      let tryParse (_: char) : Async<Result<int, string>> = async.Return (Error errMsg)

      let expectedErr = RequestDocumentError.AttributeInvalidParsed (pointer name, Some errMsg)

      let result = Attribute.Get(name, Include value, tryParse) |> Async.RunSynchronously

      test <@ result |> Result.error |> List.contains expectedErr @>
    }


module Get_nonOption_tryParseOption =

  [<Fact>]
  let ``returns Ok None if value is Skip`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let (value: Skippable<char>) = Skip
      let tryParse (_: char) : int option = failwith "Should not be called"

      let result = Attribute.Get(name, value, tryParse)

      test <@ result = Ok None @>
    }

  [<Fact>]
  let ``returns Ok Some with parsed value if value is Include and parse returns Some`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let! value = GenX.auto<char>
      let! parsed = GenX.auto<int>
      let tryParse (_: char) : int option = Some parsed

      let result = Attribute.Get(name, Include value, tryParse)

      test <@ result = Ok (Some parsed) @>
    }

  [<Fact>]
  let ``returns InvalidParsed with correct data if value is Include and parse returns None`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let! value = GenX.auto<char>
      let tryParse (_: char) : int option = None

      let expectedErr = RequestDocumentError.AttributeInvalidParsed (pointer name, None)

      let result = Attribute.Get(name, Include value, tryParse)

      test <@ result |> Result.error |> List.contains expectedErr @>
    }


module Get_nonOption_tryParseAsyncOption =

  [<Fact>]
  let ``returns Ok None if value is Skip`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let (value: Skippable<char>) = Skip
      let tryParse (_: char) : Async<int option> = failwith "Should not be called"

      let result = Attribute.Get(name, value, tryParse) |> Async.RunSynchronously

      test <@ result = Ok None @>
    }

  [<Fact>]
  let ``returns Ok Some with parsed value if value is Include and parse returns Some`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let! value = GenX.auto<char>
      let! parsed = GenX.auto<int>
      let tryParse (_: char) : Async<int option> = async.Return (Some parsed)

      let result = Attribute.Get(name, Include value, tryParse) |> Async.RunSynchronously

      test <@ result = Ok (Some parsed) @>
    }

  [<Fact>]
  let ``returns InvalidParsed with correct data if value is Include and parse returns None`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let! value = GenX.auto<char>
      let tryParse (_: char) : Async<int option> = async.Return None

      let expectedErr = RequestDocumentError.AttributeInvalidParsed (pointer name, None)

      let result = Attribute.Get(name, Include value, tryParse) |> Async.RunSynchronously

      test <@ result |> Result.error |> List.contains expectedErr @>
    }


module Get_option_stringMap =

  [<Fact>]
  let ``returns Ok None if value is Skip`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let (value: Skippable<string option>) = Skip
      let! valueMap = GenX.auto<Map<string, int>>

      let result = Attribute.Get(name, value, valueMap)

      test <@ result = Ok None @>
    }

  [<Fact>]
  let ``returns Ok (Some None) if value is Include None`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let (value: Skippable<string option>) = Include None
      let! valueMap = GenX.auto<Map<string, int>>

      let result = Attribute.Get(name, value, valueMap)

      test <@ result = Ok (Some None) @>
    }

  [<Fact>]
  let ``returns Ok Some Some with mapped value if value is Include Some and present in map`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let! value = GenX.auto<string>
      let! mapped = GenX.auto<int>
      let! valueMap = GenX.auto<Map<string, int>> |> Gen.map (Map.add value mapped)

      let result = Attribute.Get(name, Include (Some value), valueMap)

      test <@ result = Ok (Some (Some mapped)) @>
    }

  [<Fact>]
  let ``returns InvalidEnum with correct data when Include Some value is not in map`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let! value = GenX.auto<string>
      let! valueMap = GenX.auto<Map<string, int>> |> Gen.map (Map.remove value)

      let allowedValues = valueMap |> Map.toList |> List.map fst
      let expectedErr = RequestDocumentError.AttributeInvalidEnum (pointer name, value, allowedValues)

      let result = Attribute.Get(name, Include (Some value), valueMap)

      test <@ result |> Result.error |> List.contains expectedErr @>
    }


module Get_option_enumMap =

  type Enum =
    | A = 0
    | B = 1

  [<Fact>]
  let ``returns Ok None if value is Skip`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let (value: Skippable<Enum option>) = Skip
      let! valueMap = GenX.auto<Map<Enum, int>>

      let result = Attribute.Get(name, value, valueMap)

      test <@ result = Ok None @>
    }

  [<Fact>]
  let ``returns Ok (Some None) if value is Include None`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let (value: Skippable<Enum option>) = Include None
      let! valueMap = GenX.auto<Map<Enum, int>>

      let result = Attribute.Get(name, value, valueMap)

      test <@ result = Ok (Some None) @>
    }

  [<Fact>]
  let ``returns Ok Some Some with mapped value if value is Include Some and present in map`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let! value = GenX.auto<Enum>
      let! mapped = GenX.auto<int>
      let! valueMap = GenX.auto<Map<Enum, int>> |> Gen.map (Map.add value mapped)

      let result = Attribute.Get(name, Include (Some value), valueMap)

      test <@ result = Ok (Some (Some mapped)) @>
    }

  [<Fact>]
  let ``returns InvalidEnum with correct data when Include Some value is not in map`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let! value = GenX.auto<Enum>
      let! valueMap = GenX.auto<Map<Enum, int>> |> Gen.map (Map.remove value)

      let allowedValues = valueMap |> Map.toList |> List.map (fst >> string)
      let expectedErr = RequestDocumentError.AttributeInvalidEnum (pointer name, string value, allowedValues)

      let result = Attribute.Get(name, Include (Some value), valueMap)

      test <@ result |> Result.error |> List.contains expectedErr @>
    }


module Get_option_parseDirect =

  [<Fact>]
  let ``returns Ok None if value is Skip`` () =
    Property.check <| property {
      let (value: Skippable<char option>) = Skip
      let tryParse (_: char) : int = failwith "Should not be called"

      let result = Attribute.Get(value, tryParse)

      test <@ result = Ok None @>
    }

  [<Fact>]
  let ``returns Ok (Some None) if value is Include None`` () =
    Property.check <| property {
      let (value: Skippable<char option>) = Include None
      let tryParse (_: char) : int = failwith "Should not be called"

      let result = Attribute.Get(value, tryParse)

      test <@ result = Ok (Some None) @>
    }

  [<Fact>]
  let ``returns Ok Some Some with parsed value if value is Include Some`` () =
    Property.check <| property {
      let! value = GenX.auto<char>
      let! parsed = GenX.auto<int>
      let tryParse (_: char) : int = parsed

      let result = Attribute.Get(Include (Some value), tryParse)

      test <@ result = Ok (Some (Some parsed)) @>
    }


module Get_option_parseAsyncDirect =

  [<Fact>]
  let ``returns Ok None if value is Skip`` () =
    Property.check <| property {
      let (value: Skippable<char option>) = Skip
      let tryParse (_: char) : Async<int> = failwith "Should not be called"

      let result = Attribute.Get(value, tryParse) |> Async.RunSynchronously

      test <@ result = Ok None @>
    }

  [<Fact>]
  let ``returns Ok (Some None) if value is Include None`` () =
    Property.check <| property {
      let (value: Skippable<char option>) = Include None
      let tryParse (_: char) : Async<int> = failwith "Should not be called"

      let result = Attribute.Get(value, tryParse) |> Async.RunSynchronously

      test <@ result = Ok (Some None) @>
    }

  [<Fact>]
  let ``returns Ok Some Some with parsed value if value is Include Some`` () =
    Property.check <| property {
      let! value = GenX.auto<char>
      let! parsed = GenX.auto<int>
      let tryParse (_: char) : Async<int> = async.Return parsed

      let result = Attribute.Get(Include (Some value), tryParse) |> Async.RunSynchronously

      test <@ result = Ok (Some (Some parsed)) @>
    }


module Get_option_tryParseResult =

  [<Fact>]
  let ``returns Ok None if value is Skip`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let (value: Skippable<char option>) = Skip
      let tryParse (_: char) : Result<int, string> = failwith "Should not be called"

      let result = Attribute.Get(name, value, tryParse)

      test <@ result = Ok None @>
    }

  [<Fact>]
  let ``returns Ok (Some None) if value is Include None`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let (value: Skippable<char option>) = Include None
      let tryParse (_: char) : Result<int, string> = failwith "Should not be called"

      let result = Attribute.Get(name, value, tryParse)

      test <@ result = Ok (Some None) @>
    }

  [<Fact>]
  let ``returns Ok Some Some with parsed value if value is Include Some and parse returns Ok`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let! value = GenX.auto<char>
      let! parsed = GenX.auto<int>
      let tryParse (_: char) : Result<int, string> = Ok parsed

      let result = Attribute.Get(name, Include (Some value), tryParse)

      test <@ result = Ok (Some (Some parsed)) @>
    }

  [<Fact>]
  let ``returns InvalidParsed with correct data if value is Include Some and parse returns Error`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let! value = GenX.auto<char>
      let! errMsg = GenX.auto<string>
      let tryParse (_: char) : Result<int, string> = Error errMsg

      let expectedErr = RequestDocumentError.AttributeInvalidParsed (pointer name, Some errMsg)

      let result = Attribute.Get(name, Include (Some value), tryParse)

      test <@ result |> Result.error |> List.contains expectedErr @>
    }


module Get_option_tryParseAsyncResult =

  [<Fact>]
  let ``returns Ok None if value is Skip`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let (value: Skippable<char option>) = Skip
      let tryParse (_: char) : Async<Result<int, string>> = failwith "Should not be called"

      let result = Attribute.Get(name, value, tryParse) |> Async.RunSynchronously

      test <@ result = Ok None @>
    }

  [<Fact>]
  let ``returns Ok (Some None) if value is Include None`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let (value: Skippable<char option>) = Include None
      let tryParse (_: char) : Async<Result<int, string>> = failwith "Should not be called"

      let result = Attribute.Get(name, value, tryParse) |> Async.RunSynchronously

      test <@ result = Ok (Some None) @>
    }

  [<Fact>]
  let ``returns Ok Some Some with parsed value if value is Include Some and parse returns Ok`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let! value = GenX.auto<char>
      let! parsed = GenX.auto<int>
      let tryParse (_: char) : Async<Result<int, string>> = async.Return (Ok parsed)

      let result = Attribute.Get(name, Include (Some value), tryParse) |> Async.RunSynchronously

      test <@ result = Ok (Some (Some parsed)) @>
    }

  [<Fact>]
  let ``returns InvalidParsed with correct data if value is Include Some and parse returns Error`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let! value = GenX.auto<char>
      let! errMsg = GenX.auto<string>
      let tryParse (_: char) : Async<Result<int, string>> = async.Return (Error errMsg)

      let expectedErr = RequestDocumentError.AttributeInvalidParsed (pointer name, Some errMsg)

      let result = Attribute.Get(name, Include (Some value), tryParse) |> Async.RunSynchronously

      test <@ result |> Result.error |> List.contains expectedErr @>
    }


module Get_option_tryParseOption =

  [<Fact>]
  let ``returns Ok None if value is Skip`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let (value: Skippable<char option>) = Skip
      let tryParse (_: char) : int option = failwith "Should not be called"

      let result = Attribute.Get(name, value, tryParse)

      test <@ result = Ok None @>
    }

  [<Fact>]
  let ``returns Ok (Some None) if value is Include None`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let (value: Skippable<char option>) = Include None
      let tryParse (_: char) : int option = failwith "Should not be called"

      let result = Attribute.Get(name, value, tryParse)

      test <@ result = Ok (Some None) @>
    }

  [<Fact>]
  let ``returns Ok Some Some with parsed value if value is Include Some and parse returns Some`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let! value = GenX.auto<char>
      let! parsed = GenX.auto<int>
      let tryParse (_: char) : int option = Some parsed

      let result = Attribute.Get(name, Include (Some value), tryParse)

      test <@ result = Ok (Some (Some parsed)) @>
    }

  [<Fact>]
  let ``returns InvalidParsed with correct data if value is Include Some and parse returns None`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let! value = GenX.auto<char>
      let tryParse (_: char) : int option = None

      let expectedErr = RequestDocumentError.AttributeInvalidParsed (pointer name, None)

      let result = Attribute.Get(name, Include (Some value), tryParse)

      test <@ result |> Result.error |> List.contains expectedErr @>
    }


module Get_option_tryParseAsyncOption =

  [<Fact>]
  let ``returns Ok None if value is Skip`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let (value: Skippable<char option>) = Skip
      let tryParse (_: char) : Async<int option> = failwith "Should not be called"

      let result = Attribute.Get(name, value, tryParse) |> Async.RunSynchronously

      test <@ result = Ok None @>
    }

  [<Fact>]
  let ``returns Ok (Some None) if value is Include None`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let (value: Skippable<char option>) = Include None
      let tryParse (_: char) : Async<int option> = failwith "Should not be called"

      let result = Attribute.Get(name, value, tryParse) |> Async.RunSynchronously

      test <@ result = Ok (Some None) @>
    }

  [<Fact>]
  let ``returns Ok Some Some with parsed value if value is Include Some and parse returns Some`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let! value = GenX.auto<char>
      let! parsed = GenX.auto<int>
      let tryParse (_: char) : Async<int option> = async.Return (Some parsed)

      let result = Attribute.Get(name, Include (Some value), tryParse) |> Async.RunSynchronously

      test <@ result = Ok (Some (Some parsed)) @>
    }

  [<Fact>]
  let ``returns InvalidParsed with correct data if value is Include Some and parse returns None`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let! value = GenX.auto<char>
      let tryParse (_: char) : Async<int option> = async.Return None

      let expectedErr = RequestDocumentError.AttributeInvalidParsed (pointer name, None)

      let result = Attribute.Get(name, Include (Some value), tryParse) |> Async.RunSynchronously

      test <@ result |> Result.error |> List.contains expectedErr @>
    }


module GetNonNull_value =

  [<Fact>]
  let ``returns Ok None if value is Skip`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let (value: Skippable<char option>) = Skip
      let result = Attribute.GetNonNull(name, value)
      test <@ result = Ok None @>
    }

  [<Fact>]
  let ``returns Ok Some if value is Include Some`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let! value = GenX.auto<char>
      let result = Attribute.GetNonNull(name, Include (Some value))
      test <@ result = Ok (Some value) @>
    }

  [<Fact>]
  let ``returns InvalidNull with correct data if value is Include None`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let (value: Skippable<char option>) = Include None

      let expectedErr = RequestDocumentError.InvalidNull (pointer name, true)

      let result = Attribute.GetNonNull(name, value)

      test <@ result |> Result.error |> List.contains expectedErr @>
    }


module GetNonNull_stringMap =

  [<Fact>]
  let ``returns Ok None if value is Skip`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let (value: Skippable<string option>) = Skip
      let! valueMap = GenX.auto<Map<string, int>>

      let result = Attribute.GetNonNull(name, value, valueMap)

      test <@ result = Ok None @>
    }

  [<Fact>]
  let ``returns InvalidNull with correct data if value is Include None`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let (value: Skippable<string option>) = Include None
      let! valueMap = GenX.auto<Map<string, int>>

      let expectedErr = RequestDocumentError.InvalidNull (pointer name, true)

      let result = Attribute.GetNonNull(name, value, valueMap)

      test <@ result |> Result.error |> List.contains expectedErr @>
    }

  [<Fact>]
  let ``returns Ok Some with mapped value if value is Include Some and present in map`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let! value = GenX.auto<string>
      let! mapped = GenX.auto<int>
      let! valueMap = GenX.auto<Map<string, int>> |> Gen.map (Map.add value mapped)

      let result = Attribute.GetNonNull(name, Include (Some value), valueMap)

      test <@ result = Ok (Some mapped) @>
    }

  [<Fact>]
  let ``returns InvalidEnum with correct data when Include Some value is not in map`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let! value = GenX.auto<string>
      let! valueMap = GenX.auto<Map<string, int>> |> Gen.map (Map.remove value)

      let allowedValues = valueMap |> Map.toList |> List.map fst
      let expectedErr = RequestDocumentError.AttributeInvalidEnum (pointer name, value, allowedValues)

      let result = Attribute.GetNonNull(name, Include (Some value), valueMap)

      test <@ result |> Result.error |> List.contains expectedErr @>
    }


module GetNonNull_enumMap =

  type Enum =
    | A = 0
    | B = 1

  [<Fact>]
  let ``returns Ok None if value is Skip`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let (value: Skippable<Enum option>) = Skip
      let! valueMap = GenX.auto<Map<Enum, int>>

      let result = Attribute.GetNonNull(name, value, valueMap)

      test <@ result = Ok None @>
    }

  [<Fact>]
  let ``returns InvalidNull with correct data if value is Include None`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let (value: Skippable<Enum option>) = Include None
      let! valueMap = GenX.auto<Map<Enum, int>>

      let expectedErr = RequestDocumentError.InvalidNull (pointer name, true)
      
      let result = Attribute.GetNonNull(name, value, valueMap)
      
      test <@ result |> Result.error |> List.contains expectedErr @>
    }

  [<Fact>]
  let ``returns Ok Some with mapped value if value is Include Some and present in map`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let! value = GenX.auto<Enum>
      let! mapped = GenX.auto<int>
      let! valueMap = GenX.auto<Map<Enum, int>> |> Gen.map (Map.add value mapped)

      let result = Attribute.GetNonNull(name, Include (Some value), valueMap)

      test <@ result = Ok (Some mapped) @>
    }

  [<Fact>]
  let ``returns InvalidEnum with correct data when Include Some value is not in map`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let! value = GenX.auto<Enum>
      let! valueMap = GenX.auto<Map<Enum, int>> |> Gen.map (Map.remove value)

      let allowedValues = valueMap |> Map.toList |> List.map (fst >> string)
      let expectedErr = RequestDocumentError.AttributeInvalidEnum (pointer name, string value, allowedValues)

      let result = Attribute.GetNonNull(name, Include (Some value), valueMap)

      test <@ result |> Result.error |> List.contains expectedErr @>
    }


module GetNonNull_tryParseResult =

  [<Fact>]
  let ``returns Ok None if value is Skip`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let (value: Skippable<char option>) = Skip
      let tryParse (_: char) : Result<int, string> = failwith "Should not be called"

      let result = Attribute.GetNonNull(name, value, tryParse)

      test <@ result = Ok None @>
    }

  [<Fact>]
  let ``returns InvalidNull with correct data if value is Include None`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let (value: Skippable<char option>) = Include None
      let tryParse (_: char) : Result<int, string> = failwith "Should not be called"

      let expectedErr = RequestDocumentError.InvalidNull (pointer name, true)
      
      let result = Attribute.GetNonNull(name, value, tryParse)
      
      test <@ result |> Result.error |> List.contains expectedErr @>
    }

  [<Fact>]
  let ``returns Ok Some with parsed value if value is Include Some and parse returns Ok`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let! value = GenX.auto<char>
      let! parsed = GenX.auto<int>
      let tryParse (_: char) : Result<int, string> = Ok parsed

      let result = Attribute.GetNonNull(name, Include (Some value), tryParse)

      test <@ result = Ok (Some parsed) @>
    }

  [<Fact>]
  let ``returns InvalidParsed with correct data if value is Include Some and parse returns Error`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let! value = GenX.auto<char>
      let! errMsg = GenX.auto<string>
      let tryParse (_: char) : Result<int, string> = Error errMsg

      let expectedErr = RequestDocumentError.AttributeInvalidParsed (pointer name, Some errMsg)

      let result = Attribute.GetNonNull(name, Include (Some value), tryParse)

      test <@ result |> Result.error |> List.contains expectedErr @>
    }


module GetNonNull_parseDirect =

  [<Fact>]
  let ``returns Ok None if value is Skip`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let (value: Skippable<char option>) = Skip
      let tryParse (_: char) : int = failwith "Should not be called"

      let result = Attribute.GetNonNull(name, value, tryParse)

      test <@ result = Ok None @>
    }

  [<Fact>]
  let ``returns InvalidNull with correct data if value is Include None`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let (value: Skippable<char option>) = Include None
      let tryParse (_: char) : int = failwith "Should not be called"

      let expectedErr = RequestDocumentError.InvalidNull (pointer name, true)
      
      let result = Attribute.GetNonNull(name, value, tryParse)
      
      test <@ result |> Result.error |> List.contains expectedErr @>
    }

  [<Fact>]
  let ``returns Ok Some with parsed value if value is Include Some`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let! value = GenX.auto<char>
      let! parsed = GenX.auto<int>
      let tryParse (_: char) : int = parsed

      let result = Attribute.GetNonNull(name, Include (Some value), tryParse)

      test <@ result = Ok (Some parsed) @>
    }


module GetNonNull_parseAsyncDirect =

  [<Fact>]
  let ``returns Ok None if value is Skip`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let (value: Skippable<char option>) = Skip
      let tryParse (_: char) : Async<int> = failwith "Should not be called"

      let result = Attribute.GetNonNull(name, value, tryParse) |> Async.RunSynchronously

      test <@ result = Ok None @>
    }

  [<Fact>]
  let ``returns InvalidNull with correct data if value is Include None`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let (value: Skippable<char option>) = Include None
      let tryParse (_: char) : Async<int> = failwith "Should not be called"

      let expectedErr = RequestDocumentError.InvalidNull (pointer name, true)
      
      let result = Attribute.GetNonNull(name, value, tryParse) |> Async.RunSynchronously
      
      test <@ result |> Result.error |> List.contains expectedErr @>
    }

  [<Fact>]
  let ``returns Ok Some with parsed value if value is Include Some`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let! value = GenX.auto<char>
      let! parsed = GenX.auto<int>
      let tryParse (_: char) : Async<int> = async.Return parsed

      let result = Attribute.GetNonNull(name, Include (Some value), tryParse) |> Async.RunSynchronously

      test <@ result = Ok (Some parsed) @>
    }


module GetNonNull_tryParseAsyncResult =

  [<Fact>]
  let ``returns Ok None if value is Skip`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let (value: Skippable<char option>) = Skip
      let tryParse (_: char) : Async<Result<int, string>> = failwith "Should not be called"

      let result = Attribute.GetNonNull(name, value, tryParse) |> Async.RunSynchronously

      test <@ result = Ok None @>
    }

  [<Fact>]
  let ``returns InvalidNull with correct data if value is Include None`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let (value: Skippable<char option>) = Include None
      let tryParse (_: char) : Async<Result<int, string>> = failwith "Should not be called"

      let expectedErr = RequestDocumentError.InvalidNull (pointer name, true)
      
      let result = Attribute.GetNonNull(name, value, tryParse) |> Async.RunSynchronously
      
      test <@ result |> Result.error |> List.contains expectedErr @>
    }

  [<Fact>]
  let ``returns Ok Some with parsed value if value is Include Some and parse returns Ok`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let! value = GenX.auto<char>
      let! parsed = GenX.auto<int>
      let tryParse (_: char) : Async<Result<int, string>> = async.Return (Ok parsed)

      let result = Attribute.GetNonNull(name, Include (Some value), tryParse) |> Async.RunSynchronously

      test <@ result = Ok (Some parsed) @>
    }

  [<Fact>]
  let ``returns InvalidParsed with correct data if value is Include Some and parse returns Error`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let! value = GenX.auto<char>
      let! errMsg = GenX.auto<string>
      let tryParse (_: char) : Async<Result<int, string>> = async.Return (Error errMsg)

      let expectedErr = RequestDocumentError.AttributeInvalidParsed (pointer name, Some errMsg)

      let result = Attribute.GetNonNull(name, Include (Some value), tryParse) |> Async.RunSynchronously

      test <@ result |> Result.error |> List.contains expectedErr @>
    }


module GetNonNull_tryParseOption =

  [<Fact>]
  let ``returns Ok None if value is Skip`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let (value: Skippable<char option>) = Skip
      let tryParse (_: char) : int option = failwith "Should not be called"

      let result = Attribute.GetNonNull(name, value, tryParse)

      test <@ result = Ok None @>
    }

  [<Fact>]
  let ``returns InvalidNull with correct data if value is Include None`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let (value: Skippable<char option>) = Include None
      let tryParse (_: char) : int option = failwith "Should not be called"

      let expectedErr = RequestDocumentError.InvalidNull (pointer name, true)
      
      let result = Attribute.GetNonNull(name, value, tryParse)
      
      test <@ result |> Result.error |> List.contains expectedErr @>
    }

  [<Fact>]
  let ``returns Ok Some with parsed value if value is Include Some and parse returns Some`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let! value = GenX.auto<char>
      let! parsed = GenX.auto<int>
      let tryParse (_: char) : int option = Some parsed

      let result = Attribute.GetNonNull(name, Include (Some value), tryParse)

      test <@ result = Ok (Some parsed) @>
    }

  [<Fact>]
  let ``returns InvalidParsed with correct data if value is Include Some and parse returns None`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let! value = GenX.auto<char>
      let tryParse (_: char) : int option = None

      let expectedErr = RequestDocumentError.AttributeInvalidParsed (pointer name, None)

      let result = Attribute.GetNonNull(name, Include (Some value), tryParse)

      test <@ result |> Result.error |> List.contains expectedErr @>
    }


module GetNonNull_tryParseAsyncOption =

  [<Fact>]
  let ``returns Ok None if value is Skip`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let (value: Skippable<char option>) = Skip
      let tryParse (_: char) : Async<int option> = failwith "Should not be called"

      let result = Attribute.GetNonNull(name, value, tryParse) |> Async.RunSynchronously

      test <@ result = Ok None @>
    }

  [<Fact>]
  let ``returns InvalidNull with correct data if value is Include None`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let (value: Skippable<char option>) = Include None
      let tryParse (_: char) : Async<int option> = failwith "Should not be called"

      let expectedErr = RequestDocumentError.InvalidNull (pointer name, true)
      
      let result = Attribute.GetNonNull(name, value, tryParse) |> Async.RunSynchronously
      
      test <@ result |> Result.error |> List.contains expectedErr @>
    }

  [<Fact>]
  let ``returns Ok Some with parsed value if value is Include Some and parse returns Some`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let! value = GenX.auto<char>
      let! parsed = GenX.auto<int>
      let tryParse (_: char) : Async<int option> = async.Return (Some parsed)

      let result = Attribute.GetNonNull(name, Include (Some value), tryParse) |> Async.RunSynchronously

      test <@ result = Ok (Some parsed) @>
    }

  [<Fact>]
  let ``returns InvalidParsed with correct data if value is Include Some and parse returns None`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let! value = GenX.auto<char>
      let tryParse (_: char) : Async<int option> = async.Return None

      let expectedErr = RequestDocumentError.AttributeInvalidParsed (pointer name, None)

      let result = Attribute.GetNonNull(name, Include (Some value), tryParse) |> Async.RunSynchronously

      test <@ result |> Result.error |> List.contains expectedErr @>
    }


module Require_value =

  [<Fact>]
  let ``returns RequiredFieldMissing with correct data if value is Skip`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let (value: Skippable<char>) = Skip

      let expectedErr = RequestDocumentError.RequiredFieldMissing (pointer name)

      let result = Attribute.Require(name, value)

      test <@ result |> Result.error |> List.contains expectedErr @>
    }

  [<Fact>]
  let ``returns Ok with the Include value if value is Include`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let! value = GenX.auto<char>
      let result = Attribute.Require(name, Include value)
      test <@ result = Ok value @>
    }


module Require_nonOption_stringMap =

  [<Fact>]
  let ``returns RequiredFieldMissing with correct data if value is Skip`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let (value: Skippable<string>) = Skip
      let! valueMap = GenX.auto<Map<string, int>>

      let expectedErr = RequestDocumentError.RequiredFieldMissing (pointer name)

      let result = Attribute.Require(name, value, valueMap)

      test <@ result |> Result.error |> List.contains expectedErr @>
    }

  [<Fact>]
  let ``returns Ok with mapped value if value is Include and present in map`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let! value = GenX.auto<string>
      let! mapped = GenX.auto<int>
      let! valueMap = GenX.auto<Map<string, int>> |> Gen.map (Map.add value mapped)

      let result = Attribute.Require(name, Include value, valueMap)

      test <@ result = Ok mapped @>
    }

  [<Fact>]
  let ``returns InvalidEnum with correct data when Include value is not in map`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let! value = GenX.auto<string>
      let! valueMap = GenX.auto<Map<string, int>> |> Gen.map (Map.remove value)

      let allowedValues = valueMap |> Map.toList |> List.map fst
      let expectedErr = RequestDocumentError.AttributeInvalidEnum (pointer name, value, allowedValues)

      let result = Attribute.Require(name, Include value, valueMap)

      test <@ result |> Result.error |> List.contains expectedErr @>
    }


module Require_nonOption_enumMap =

  type Enum =
    | A = 0
    | B = 1

  [<Fact>]
  let ``returns RequiredFieldMissing with correct data if value is Skip`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let (value: Skippable<Enum>) = Skip
      let! valueMap = GenX.auto<Map<Enum, int>>

      let expectedErr = RequestDocumentError.RequiredFieldMissing (pointer name)

      let result = Attribute.Require(name, value, valueMap)

      test <@ result |> Result.error |> List.contains expectedErr @>
    }

  [<Fact>]
  let ``returns Ok with mapped value if value is Include and present in map`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let! value = GenX.auto<Enum>
      let! mapped = GenX.auto<int>
      let! valueMap = GenX.auto<Map<Enum, int>> |> Gen.map (Map.add value mapped)

      let result = Attribute.Require(name, Include value, valueMap)

      test <@ result = Ok mapped @>
    }

  [<Fact>]
  let ``returns InvalidEnum with correct data when Include value is not in map`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let! value = GenX.auto<Enum>
      let! valueMap = GenX.auto<Map<Enum, int>> |> Gen.map (Map.remove value)

      let allowedValues = valueMap |> Map.toList |> List.map (fst >> string)
      let expectedErr = RequestDocumentError.AttributeInvalidEnum (pointer name, string value, allowedValues)

      let result = Attribute.Require(name, Include value, valueMap)

      test <@ result |> Result.error |> List.contains expectedErr @>
    }


module Require_nonOption_parseDirect =

  [<Fact>]
  let ``returns RequiredFieldMissing with correct data if value is Skip`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let (value: Skippable<char>) = Skip
      let tryParse (_: char) : int = failwith "Should not be called"

      let expectedErr = RequestDocumentError.RequiredFieldMissing (pointer name)

      let result = Attribute.Require(name, value, tryParse)

      test <@ result |> Result.error |> List.contains expectedErr @>
    }

  [<Fact>]
  let ``returns Ok with parsed value if value is Include`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let! value = GenX.auto<char>
      let! parsed = GenX.auto<int>
      let tryParse (_: char) : int = parsed

      let result = Attribute.Require(name, Include value, tryParse)

      test <@ result = Ok parsed @>
    }


module Require_nonOption_parseAsyncDirect =

  [<Fact>]
  let ``returns RequiredFieldMissing with correct data if value is Skip`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let (value: Skippable<char>) = Skip
      let tryParse (_: char) : Async<int> = failwith "Should not be called"

      let expectedErr = RequestDocumentError.RequiredFieldMissing (pointer name)

      let result = Attribute.Require(name, value, tryParse) |> Async.RunSynchronously

      test <@ result |> Result.error |> List.contains expectedErr @>
    }

  [<Fact>]
  let ``returns Ok with parsed value if value is Include`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let! value = GenX.auto<char>
      let! parsed = GenX.auto<int>
      let tryParse (_: char) : Async<int> = async.Return parsed

      let result = Attribute.Require(name, Include value, tryParse) |> Async.RunSynchronously

      test <@ result = Ok parsed @>
    }


module Require_nonOption_tryParseResult =

  [<Fact>]
  let ``returns RequiredFieldMissing with correct data if value is Skip`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let (value: Skippable<char>) = Skip
      let tryParse (_: char) : Result<int, string> = failwith "Should not be called"

      let expectedErr = RequestDocumentError.RequiredFieldMissing (pointer name)

      let result = Attribute.Require(name, value, tryParse)

      test <@ result |> Result.error |> List.contains expectedErr @>
    }

  [<Fact>]
  let ``returns Ok with parsed value if value is Include and parse returns Ok`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let! value = GenX.auto<char>
      let! parsed = GenX.auto<int>
      let tryParse (_: char) : Result<int, string> = Ok parsed

      let result = Attribute.Require(name, Include value, tryParse)

      test <@ result = Ok parsed @>
    }

  [<Fact>]
  let ``returns InvalidParsed with correct data if value is Include and parse returns Error`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let! value = GenX.auto<char>
      let! errMsg = GenX.auto<string>
      let tryParse (_: char) : Result<int, string> = Error errMsg

      let expectedErr = RequestDocumentError.AttributeInvalidParsed (pointer name, Some errMsg)

      let result = Attribute.Require(name, Include value, tryParse)

      test <@ result |> Result.error |> List.contains expectedErr @>
    }


module Require_nonOption_tryParseAsyncResult =

  [<Fact>]
  let ``returns RequiredFieldMissing with correct data if value is Skip`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let (value: Skippable<char>) = Skip
      let tryParse (_: char) : Async<Result<int, string>> = failwith "Should not be called"

      let expectedErr = RequestDocumentError.RequiredFieldMissing (pointer name)

      let result = Attribute.Require(name, value, tryParse) |> Async.RunSynchronously

      test <@ result |> Result.error |> List.contains expectedErr @>
    }

  [<Fact>]
  let ``returns Ok with parsed value if value is Include and parse returns Ok`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let! value = GenX.auto<char>
      let! parsed = GenX.auto<int>
      let tryParse (_: char) : Async<Result<int, string>> = async.Return (Ok parsed)

      let result = Attribute.Require(name, Include value, tryParse) |> Async.RunSynchronously

      test <@ result = Ok parsed @>
    }

  [<Fact>]
  let ``returns InvalidParsed with correct data if value is Include and parse returns Error`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let! value = GenX.auto<char>
      let! errMsg = GenX.auto<string>
      let tryParse (_: char) : Async<Result<int, string>> = async.Return (Error errMsg)

      let expectedErr = RequestDocumentError.AttributeInvalidParsed (pointer name, Some errMsg)

      let result = Attribute.Require(name, Include value, tryParse) |> Async.RunSynchronously

      test <@ result |> Result.error |> List.contains expectedErr @>
    }


module Require_nonOption_tryParseOption =

  [<Fact>]
  let ``returns RequiredFieldMissing with correct data if value is Skip`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let (value: Skippable<char>) = Skip
      let tryParse (_: char) : int option = failwith "Should not be called"

      let expectedErr = RequestDocumentError.RequiredFieldMissing (pointer name)

      let result = Attribute.Require(name, value, tryParse)

      test <@ result |> Result.error |> List.contains expectedErr @>
    }

  [<Fact>]
  let ``returns Ok with parsed value if value is Include and parse returns Some`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let! value = GenX.auto<char>
      let! parsed = GenX.auto<int>
      let tryParse (_: char) : int option = Some parsed

      let result = Attribute.Require(name, Include value, tryParse)

      test <@ result = Ok parsed @>
    }

  [<Fact>]
  let ``returns InvalidParsed with correct data if value is Include and parse returns None`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let! value = GenX.auto<char>
      let tryParse (_: char) : int option = None

      let expectedErr = RequestDocumentError.AttributeInvalidParsed (pointer name, None)

      let result = Attribute.Require(name, Include value, tryParse)

      test <@ result |> Result.error |> List.contains expectedErr @>
    }


module Require_nonOption_tryParseAsyncOption =

  [<Fact>]
  let ``returns RequiredFieldMissing with correct data if value is Skip`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let (value: Skippable<char>) = Skip
      let tryParse (_: char) : Async<int option> = failwith "Should not be called"

      let expectedErr = RequestDocumentError.RequiredFieldMissing (pointer name)

      let result = Attribute.Require(name, value, tryParse) |> Async.RunSynchronously

      test <@ result |> Result.error |> List.contains expectedErr @>
    }

  [<Fact>]
  let ``returns Ok with parsed value if value is Include and parse returns Some`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let! value = GenX.auto<char>
      let! parsed = GenX.auto<int>
      let tryParse (_: char) : Async<int option> = async.Return (Some parsed)

      let result = Attribute.Require(name, Include value, tryParse) |> Async.RunSynchronously

      test <@ result = Ok parsed @>
    }

  [<Fact>]
  let ``returns InvalidParsed with correct data if value is Include and parse returns None`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let! value = GenX.auto<char>
      let tryParse (_: char) : Async<int option> = async.Return None

      let expectedErr = RequestDocumentError.AttributeInvalidParsed (pointer name, None)

      let result = Attribute.Require(name, Include value, tryParse) |> Async.RunSynchronously

      test <@ result |> Result.error |> List.contains expectedErr @>
    }


module Require_option_stringMap =

  [<Fact>]
  let ``returns RequiredFieldMissing with correct data if value is Skip`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let (value: Skippable<string option>) = Skip
      let! valueMap = GenX.auto<Map<string, int>>

      let expectedErr = RequestDocumentError.RequiredFieldMissing (pointer name)

      let result = Attribute.Require(name, value, valueMap)

      test <@ result |> Result.error |> List.contains expectedErr @>
    }

  [<Fact>]
  let ``returns Ok None if value is Include None`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let (value: Skippable<string option>) = Include None
      let! valueMap = GenX.auto<Map<string, int>>

      let result = Attribute.Require(name, value, valueMap)

      test <@ result = Ok None @>
    }

  [<Fact>]
  let ``returns Ok Some with mapped value if value is Include Some and present in map`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let! value = GenX.auto<string>
      let! mapped = GenX.auto<int>
      let! valueMap = GenX.auto<Map<string, int>> |> Gen.map (Map.add value mapped)

      let result = Attribute.Require(name, Include (Some value), valueMap)

      test <@ result = Ok (Some mapped) @>
    }

  [<Fact>]
  let ``returns InvalidEnum with correct data when Include Some value is not in map`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let! value = GenX.auto<string>
      let! valueMap = GenX.auto<Map<string, int>> |> Gen.map (Map.remove value)

      let allowedValues = valueMap |> Map.toList |> List.map fst
      let expectedErr = RequestDocumentError.AttributeInvalidEnum (pointer name, value, allowedValues)

      let result = Attribute.Require(name, Include (Some value), valueMap)

      test <@ result |> Result.error |> List.contains expectedErr @>
    }


module Require_option_enumMap =

  type Enum =
    | A = 0
    | B = 1

  [<Fact>]
  let ``returns RequiredFieldMissing with correct data if value is Skip`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let (value: Skippable<Enum option>) = Skip
      let! valueMap = GenX.auto<Map<Enum, int>>

      let expectedErr = RequestDocumentError.RequiredFieldMissing (pointer name)

      let result = Attribute.Require(name, value, valueMap)

      test <@ result |> Result.error |> List.contains expectedErr @>
    }

  [<Fact>]
  let ``returns Ok None if value is Include None`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let (value: Skippable<Enum option>) = Include None
      let! valueMap = GenX.auto<Map<Enum, int>>

      let result = Attribute.Require(name, value, valueMap)

      test <@ result = Ok None @>
    }

  [<Fact>]
  let ``returns Ok Some with mapped value if value is Include Some and present in map`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let! value = GenX.auto<Enum>
      let! mapped = GenX.auto<int>
      let! valueMap = GenX.auto<Map<Enum, int>> |> Gen.map (Map.add value mapped)

      let result = Attribute.Require(name, Include (Some value), valueMap)

      test <@ result = Ok (Some mapped) @>
    }

  [<Fact>]
  let ``returns InvalidEnum with correct data when Include Some value is not in map`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let! value = GenX.auto<Enum>
      let! valueMap = GenX.auto<Map<Enum, int>> |> Gen.map (Map.remove value)

      let allowedValues = valueMap |> Map.toList |> List.map (fst >> string)
      let expectedErr = RequestDocumentError.AttributeInvalidEnum (pointer name, string value, allowedValues)

      let result = Attribute.Require(name, Include (Some value), valueMap)

      test <@ result |> Result.error |> List.contains expectedErr @>
    }


module Require_option_parseDirect =

  [<Fact>]
  let ``returns RequiredFieldMissing with correct data if value is Skip`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let (value: Skippable<char option>) = Skip
      let tryParse (_: char) : int = failwith "Should not be called"

      let expectedErr = RequestDocumentError.RequiredFieldMissing (pointer name)

      let result = Attribute.Require(name, value, tryParse)

      test <@ result |> Result.error |> List.contains expectedErr @>
    }

  [<Fact>]
  let ``returns Ok None if value is Include None`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let (value: Skippable<char option>) = Include None
      let tryParse (_: char) : int = failwith "Should not be called"

      let result = Attribute.Require(name, value, tryParse)

      test <@ result = Ok None @>
    }

  [<Fact>]
  let ``returns Ok Some with parsed value if value is Include Some`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let! value = GenX.auto<char>
      let! parsed = GenX.auto<int>
      let tryParse (_: char) : int = parsed

      let result = Attribute.Require(name, Include (Some value), tryParse)

      test <@ result = Ok (Some parsed) @>
    }


module Require_option_parseAsyncDirect =

  [<Fact>]
  let ``returns RequiredFieldMissing with correct data if value is Skip`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let (value: Skippable<char option>) = Skip
      let tryParse (_: char) : Async<int> = failwith "Should not be called"

      let expectedErr = RequestDocumentError.RequiredFieldMissing (pointer name)

      let result = Attribute.Require(name, value, tryParse) |> Async.RunSynchronously

      test <@ result |> Result.error |> List.contains expectedErr @>
    }

  [<Fact>]
  let ``returns Ok None if value is Include None`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let (value: Skippable<char option>) = Include None
      let tryParse (_: char) : Async<int> = failwith "Should not be called"

      let result = Attribute.Require(name, value, tryParse) |> Async.RunSynchronously

      test <@ result = Ok None @>
    }

  [<Fact>]
  let ``returns Ok Some with parsed value if value is Include Some`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let! value = GenX.auto<char>
      let! parsed = GenX.auto<int>
      let tryParse (_: char) : Async<int> = async.Return parsed

      let result = Attribute.Require(name, Include (Some value), tryParse) |> Async.RunSynchronously

      test <@ result = Ok (Some parsed) @>
    }


module Require_option_tryParseResult =

  [<Fact>]
  let ``returns RequiredFieldMissing with correct data if value is Skip`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let (value: Skippable<char option>) = Skip
      let tryParse (_: char) : Result<int, string> = failwith "Should not be called"

      let expectedErr = RequestDocumentError.RequiredFieldMissing (pointer name)

      let result = Attribute.Require(name, value, tryParse)

      test <@ result |> Result.error |> List.contains expectedErr @>
    }

  [<Fact>]
  let ``returns Ok None if value is Include None`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let (value: Skippable<char option>) = Include None
      let tryParse (_: char) : Result<int, string> = failwith "Should not be called"

      let result = Attribute.Require(name, value, tryParse)

      test <@ result = Ok None @>
    }

  [<Fact>]
  let ``returns Ok Some with parsed value if value is Include Some and parse returns Ok`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let! value = GenX.auto<char>
      let! parsed = GenX.auto<int>
      let tryParse (_: char) : Result<int, string> = Ok parsed

      let result = Attribute.Require(name, Include (Some value), tryParse)

      test <@ result = Ok (Some parsed) @>
    }

  [<Fact>]
  let ``returns InvalidParsed with correct data if value is Include Some and parse returns Error`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let! value = GenX.auto<char>
      let! errMsg = GenX.auto<string>
      let tryParse (_: char) : Result<int, string> = Error errMsg

      let expectedErr = RequestDocumentError.AttributeInvalidParsed (pointer name, Some errMsg)

      let result = Attribute.Require(name, Include (Some value), tryParse)

      test <@ result |> Result.error |> List.contains expectedErr @>
    }


module Require_option_tryParseAsyncResult =

  [<Fact>]
  let ``returns RequiredFieldMissing with correct data if value is Skip`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let (value: Skippable<char option>) = Skip
      let tryParse (_: char) : Async<Result<int, string>> = failwith "Should not be called"

      let expectedErr = RequestDocumentError.RequiredFieldMissing (pointer name)

      let result = Attribute.Require(name, value, tryParse) |> Async.RunSynchronously

      test <@ result |> Result.error |> List.contains expectedErr @>
    }

  [<Fact>]
  let ``returns Ok None if value is Include None`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let (value: Skippable<char option>) = Include None
      let tryParse (_: char) : Async<Result<int, string>> = failwith "Should not be called"

      let result = Attribute.Require(name, value, tryParse) |> Async.RunSynchronously

      test <@ result = Ok None @>
    }

  [<Fact>]
  let ``returns Ok Some with parsed value if value is Include Some and parse returns Ok`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let! value = GenX.auto<char>
      let! parsed = GenX.auto<int>
      let tryParse (_: char) : Async<Result<int, string>> = async.Return (Ok parsed)

      let result = Attribute.Require(name, Include (Some value), tryParse) |> Async.RunSynchronously

      test <@ result = Ok (Some parsed) @>
    }

  [<Fact>]
  let ``returns InvalidParsed with correct data if value is Include Some and parse returns Error`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let! value = GenX.auto<char>
      let! errMsg = GenX.auto<string>
      let tryParse (_: char) : Async<Result<int, string>> = async.Return (Error errMsg)

      let expectedErr = RequestDocumentError.AttributeInvalidParsed (pointer name, Some errMsg)

      let result = Attribute.Require(name, Include (Some value), tryParse) |> Async.RunSynchronously

      test <@ result |> Result.error |> List.contains expectedErr @>
    }


module Require_option_tryParseOption =

  [<Fact>]
  let ``returns RequiredFieldMissing with correct data if value is Skip`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let (value: Skippable<char option>) = Skip
      let tryParse (_: char) : int option = failwith "Should not be called"

      let expectedErr = RequestDocumentError.RequiredFieldMissing (pointer name)

      let result = Attribute.Require(name, value, tryParse)

      test <@ result |> Result.error |> List.contains expectedErr @>
    }

  [<Fact>]
  let ``returns Ok None if value is Include None`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let (value: Skippable<char option>) = Include None
      let tryParse (_: char) : int option = failwith "Should not be called"

      let result = Attribute.Require(name, value, tryParse)

      test <@ result = Ok None @>
    }

  [<Fact>]
  let ``returns Ok Some with parsed value if value is Include Some and parse returns Some`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let! value = GenX.auto<char>
      let! parsed = GenX.auto<int>
      let tryParse (_: char) : int option = Some parsed

      let result = Attribute.Require(name, Include (Some value), tryParse)

      test <@ result = Ok (Some parsed) @>
    }

  [<Fact>]
  let ``returns InvalidParsed with correct data if value is Include Some and parse returns None`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let! value = GenX.auto<char>
      let tryParse (_: char) : int option = None

      let expectedErr = RequestDocumentError.AttributeInvalidParsed (pointer name, None)

      let result = Attribute.Require(name, Include (Some value), tryParse)

      test <@ result |> Result.error |> List.contains expectedErr @>
    }


module Require_option_tryParseAsyncOption =

  [<Fact>]
  let ``returns RequiredFieldMissing with correct data if value is Skip`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let (value: Skippable<char option>) = Skip
      let tryParse (_: char) : Async<int option> = failwith "Should not be called"

      let expectedErr = RequestDocumentError.RequiredFieldMissing (pointer name)

      let result = Attribute.Require(name, value, tryParse) |> Async.RunSynchronously

      test <@ result |> Result.error |> List.contains expectedErr @>
    }

  [<Fact>]
  let ``returns Ok None if value is Include None`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let (value: Skippable<char option>) = Include None
      let tryParse (_: char) : Async<int option> = failwith "Should not be called"

      let result = Attribute.Require(name, value, tryParse) |> Async.RunSynchronously

      test <@ result = Ok None @>
    }

  [<Fact>]
  let ``returns Ok Some with parsed value if value is Include Some and parse returns Some`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let! value = GenX.auto<char>
      let! parsed = GenX.auto<int>
      let tryParse (_: char) : Async<int option> = async.Return (Some parsed)

      let result = Attribute.Require(name, Include (Some value), tryParse) |> Async.RunSynchronously

      test <@ result = Ok (Some parsed) @>
    }

  [<Fact>]
  let ``returns InvalidParsed with correct data if value is Include Some and parse returns None`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let! value = GenX.auto<char>
      let tryParse (_: char) : Async<int option> = async.Return None

      let expectedErr = RequestDocumentError.AttributeInvalidParsed (pointer name, None)

      let result = Attribute.Require(name, Include (Some value), tryParse) |> Async.RunSynchronously

      test <@ result |> Result.error |> List.contains expectedErr @>
    }


module RequireNonNull_value =

  [<Fact>]
  let ``returns RequiredFieldMissing with correct data if value is Skip`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let (value: Skippable<char option>) = Skip

      let expectedErr = RequestDocumentError.RequiredFieldMissing (pointer name)

      let result = Attribute.RequireNonNull(name, value)

      test <@ result |> Result.error |> List.contains expectedErr @>
    }

  [<Fact>]
  let ``returns Ok if value is Include Some`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let! value = GenX.auto<char>
      let result = Attribute.RequireNonNull(name, Include (Some value))
      test <@ result = Ok value @>
    }

  [<Fact>]
  let ``returns InvalidNull with correct data if value is Include None`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let (value: Skippable<char option>) = Include None

      let expectedErr = RequestDocumentError.InvalidNull (pointer name, true)

      let result = Attribute.RequireNonNull(name, value)

      test <@ result |> Result.error |> List.contains expectedErr @>
    }


module RequireNonNull_stringMap =

  [<Fact>]
  let ``returns RequiredFieldMissing with correct data if value is Skip`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let (value: Skippable<string option>) = Skip
      let! valueMap = GenX.auto<Map<string, int>>

      let expectedErr = RequestDocumentError.RequiredFieldMissing (pointer name)

      let result = Attribute.RequireNonNull(name, value, valueMap)

      test <@ result |> Result.error |> List.contains expectedErr @>
    }

  [<Fact>]
  let ``returns InvalidNull with correct data if value is Include None`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let (value: Skippable<string option>) = Include None
      let! valueMap = GenX.auto<Map<string, int>>

      let expectedErr = RequestDocumentError.InvalidNull (pointer name, true)

      let result = Attribute.RequireNonNull(name, value, valueMap)

      test <@ result |> Result.error |> List.contains expectedErr @>
    }

  [<Fact>]
  let ``returns Ok with mapped value if value is Include Some and present in map`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let! value = GenX.auto<string>
      let! mapped = GenX.auto<int>
      let! valueMap = GenX.auto<Map<string, int>> |> Gen.map (Map.add value mapped)

      let result = Attribute.RequireNonNull(name, Include (Some value), valueMap)

      test <@ result = Ok mapped @>
    }

  [<Fact>]
  let ``returns InvalidEnum with correct data when Include Some value is not in map`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let! value = GenX.auto<string>
      let! valueMap = GenX.auto<Map<string, int>> |> Gen.map (Map.remove value)

      let allowedValues = valueMap |> Map.toList |> List.map fst
      let expectedErr = RequestDocumentError.AttributeInvalidEnum (pointer name, value, allowedValues)

      let result = Attribute.RequireNonNull(name, Include (Some value), valueMap)

      test <@ result |> Result.error |> List.contains expectedErr @>
    }


module RequireNonNull_enumMap =

  type Enum =
    | A = 0
    | B = 1

  [<Fact>]
  let ``returns RequiredFieldMissing with correct data if value is Skip`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let (value: Skippable<Enum option>) = Skip
      let! valueMap = GenX.auto<Map<Enum, int>>

      let expectedErr = RequestDocumentError.RequiredFieldMissing (pointer name)

      let result = Attribute.RequireNonNull(name, value, valueMap)

      test <@ result |> Result.error |> List.contains expectedErr @>
    }

  [<Fact>]
  let ``returns InvalidNull with correct data if value is Include None`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let (value: Skippable<Enum option>) = Include None
      let! valueMap = GenX.auto<Map<Enum, int>>

      let expectedErr = RequestDocumentError.InvalidNull (pointer name, true)
      
      let result = Attribute.RequireNonNull(name, value, valueMap)
      
      test <@ result |> Result.error |> List.contains expectedErr @>
    }

  [<Fact>]
  let ``returns Ok with mapped value if value is Include Some and present in map`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let! value = GenX.auto<Enum>
      let! mapped = GenX.auto<int>
      let! valueMap = GenX.auto<Map<Enum, int>> |> Gen.map (Map.add value mapped)

      let result = Attribute.RequireNonNull(name, Include (Some value), valueMap)

      test <@ result = Ok mapped @>
    }

  [<Fact>]
  let ``returns InvalidEnum with correct data when Include Some value is not in map`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let! value = GenX.auto<Enum>
      let! valueMap = GenX.auto<Map<Enum, int>> |> Gen.map (Map.remove value)

      let allowedValues = valueMap |> Map.toList |> List.map (fst >> string)
      let expectedErr = RequestDocumentError.AttributeInvalidEnum (pointer name, string value, allowedValues)

      let result = Attribute.RequireNonNull(name, Include (Some value), valueMap)

      test <@ result |> Result.error |> List.contains expectedErr @>
    }


module RequireNonNull_tryParseResult =

  [<Fact>]
  let ``returns RequiredFieldMissing with correct data if value is Skip`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let (value: Skippable<char option>) = Skip
      let tryParse (_: char) : Result<int, string> = failwith "Should not be called"

      let expectedErr = RequestDocumentError.RequiredFieldMissing (pointer name)

      let result = Attribute.RequireNonNull(name, value, tryParse)

      test <@ result |> Result.error |> List.contains expectedErr @>
    }

  [<Fact>]
  let ``returns InvalidNull with correct data if value is Include None`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let (value: Skippable<char option>) = Include None
      let tryParse (_: char) : Result<int, string> = failwith "Should not be called"

      let expectedErr = RequestDocumentError.InvalidNull (pointer name, true)
      
      let result = Attribute.RequireNonNull(name, value, tryParse)
      
      test <@ result |> Result.error |> List.contains expectedErr @>
    }

  [<Fact>]
  let ``returns Ok with parsed value if value is Include Some and parse returns Ok`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let! value = GenX.auto<char>
      let! parsed = GenX.auto<int>
      let tryParse (_: char) : Result<int, string> = Ok parsed

      let result = Attribute.RequireNonNull(name, Include (Some value), tryParse)

      test <@ result = Ok parsed @>
    }

  [<Fact>]
  let ``returns InvalidParsed with correct data if value is Include Some and parse returns Error`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let! value = GenX.auto<char>
      let! errMsg = GenX.auto<string>
      let tryParse (_: char) : Result<int, string> = Error errMsg

      let expectedErr = RequestDocumentError.AttributeInvalidParsed (pointer name, Some errMsg)

      let result = Attribute.RequireNonNull(name, Include (Some value), tryParse)

      test <@ result |> Result.error |> List.contains expectedErr @>
    }


module RequireNonNull_parseDirect =

  [<Fact>]
  let ``returns RequiredFieldMissing with correct data if value is Skip`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let (value: Skippable<char option>) = Skip
      let tryParse (_: char) : int = failwith "Should not be called"

      let expectedErr = RequestDocumentError.RequiredFieldMissing (pointer name)

      let result = Attribute.RequireNonNull(name, value, tryParse)

      test <@ result |> Result.error |> List.contains expectedErr @>
    }

  [<Fact>]
  let ``returns InvalidNull with correct data if value is Include None`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let (value: Skippable<char option>) = Include None
      let tryParse (_: char) : int = failwith "Should not be called"

      let expectedErr = RequestDocumentError.InvalidNull (pointer name, true)
      
      let result = Attribute.RequireNonNull(name, value, tryParse)
      
      test <@ result |> Result.error |> List.contains expectedErr @>
    }

  [<Fact>]
  let ``returns Ok with parsed value if value is Include Some`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let! value = GenX.auto<char>
      let! parsed = GenX.auto<int>
      let tryParse (_: char) : int = parsed

      let result = Attribute.RequireNonNull(name, Include (Some value), tryParse)

      test <@ result = Ok parsed @>
    }


module RequireNonNull_parseAsyncDirect =

  [<Fact>]
  let ``returns RequiredFieldMissing with correct data if value is Skip`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let (value: Skippable<char option>) = Skip
      let tryParse (_: char) : Async<int> = failwith "Should not be called"

      let expectedErr = RequestDocumentError.RequiredFieldMissing (pointer name)

      let result = Attribute.RequireNonNull(name, value, tryParse) |> Async.RunSynchronously

      test <@ result |> Result.error |> List.contains expectedErr @>
    }

  [<Fact>]
  let ``returns InvalidNull with correct data if value is Include None`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let (value: Skippable<char option>) = Include None
      let tryParse (_: char) : Async<int> = failwith "Should not be called"

      let expectedErr = RequestDocumentError.InvalidNull (pointer name, true)
      
      let result = Attribute.RequireNonNull(name, value, tryParse) |> Async.RunSynchronously
      
      test <@ result |> Result.error |> List.contains expectedErr @>
    }

  [<Fact>]
  let ``returns Ok with parsed value if value is Include Some`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let! value = GenX.auto<char>
      let! parsed = GenX.auto<int>
      let tryParse (_: char) : Async<int> = async.Return parsed

      let result = Attribute.RequireNonNull(name, Include (Some value), tryParse) |> Async.RunSynchronously

      test <@ result = Ok parsed @>
    }


module RequireNonNull_tryParseAsyncResult =

  [<Fact>]
  let ``returns RequiredFieldMissing with correct data if value is Skip`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let (value: Skippable<char option>) = Skip
      let tryParse (_: char) : Async<Result<int, string>> = failwith "Should not be called"

      let expectedErr = RequestDocumentError.RequiredFieldMissing (pointer name)

      let result = Attribute.RequireNonNull(name, value, tryParse) |> Async.RunSynchronously

      test <@ result |> Result.error |> List.contains expectedErr @>
    }

  [<Fact>]
  let ``returns InvalidNull with correct data if value is Include None`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let (value: Skippable<char option>) = Include None
      let tryParse (_: char) : Async<Result<int, string>> = failwith "Should not be called"

      let expectedErr = RequestDocumentError.InvalidNull (pointer name, true)
      
      let result = Attribute.RequireNonNull(name, value, tryParse) |> Async.RunSynchronously
      
      test <@ result |> Result.error |> List.contains expectedErr @>
    }

  [<Fact>]
  let ``returns Ok with parsed value if value is Include Some and parse returns Ok`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let! value = GenX.auto<char>
      let! parsed = GenX.auto<int>
      let tryParse (_: char) : Async<Result<int, string>> = async.Return (Ok parsed)

      let result = Attribute.RequireNonNull(name, Include (Some value), tryParse) |> Async.RunSynchronously

      test <@ result = Ok parsed @>
    }

  [<Fact>]
  let ``returns InvalidParsed with correct data if value is Include Some and parse returns Error`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let! value = GenX.auto<char>
      let! errMsg = GenX.auto<string>
      let tryParse (_: char) : Async<Result<int, string>> = async.Return (Error errMsg)

      let expectedErr = RequestDocumentError.AttributeInvalidParsed (pointer name, Some errMsg)

      let result = Attribute.RequireNonNull(name, Include (Some value), tryParse) |> Async.RunSynchronously

      test <@ result |> Result.error |> List.contains expectedErr @>
    }


module RequireNonNull_tryParseOption =

  [<Fact>]
  let ``returns RequiredFieldMissing with correct data if value is Skip`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let (value: Skippable<char option>) = Skip
      let tryParse (_: char) : int option = failwith "Should not be called"

      let expectedErr = RequestDocumentError.RequiredFieldMissing (pointer name)

      let result = Attribute.RequireNonNull(name, value, tryParse)

      test <@ result |> Result.error |> List.contains expectedErr @>
    }

  [<Fact>]
  let ``returns InvalidNull with correct data if value is Include None`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let (value: Skippable<char option>) = Include None
      let tryParse (_: char) : int option = failwith "Should not be called"

      let expectedErr = RequestDocumentError.InvalidNull (pointer name, true)
      
      let result = Attribute.RequireNonNull(name, value, tryParse)
      
      test <@ result |> Result.error |> List.contains expectedErr @>
    }

  [<Fact>]
  let ``returns Ok with parsed value if value is Include Some and parse returns Some`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let! value = GenX.auto<char>
      let! parsed = GenX.auto<int>
      let tryParse (_: char) : int option = Some parsed

      let result = Attribute.RequireNonNull(name, Include (Some value), tryParse)

      test <@ result = Ok parsed @>
    }

  [<Fact>]
  let ``returns InvalidParsed with correct data if value is Include Some and parse returns None`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let! value = GenX.auto<char>
      let tryParse (_: char) : int option = None

      let expectedErr = RequestDocumentError.AttributeInvalidParsed (pointer name, None)

      let result = Attribute.RequireNonNull(name, Include (Some value), tryParse)

      test <@ result |> Result.error |> List.contains expectedErr @>
    }


module RequireNonNull_tryParseAsyncOption =

  [<Fact>]
  let ``returns RequiredFieldMissing with correct data if value is Skip`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let (value: Skippable<char option>) = Skip
      let tryParse (_: char) : Async<int option> = failwith "Should not be called"

      let expectedErr = RequestDocumentError.RequiredFieldMissing (pointer name)

      let result = Attribute.RequireNonNull(name, value, tryParse) |> Async.RunSynchronously

      test <@ result |> Result.error |> List.contains expectedErr @>
    }

  [<Fact>]
  let ``returns InvalidNull with correct data if value is Include None`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let (value: Skippable<char option>) = Include None
      let tryParse (_: char) : Async<int option> = failwith "Should not be called"

      let expectedErr = RequestDocumentError.InvalidNull (pointer name, true)
      
      let result = Attribute.RequireNonNull(name, value, tryParse) |> Async.RunSynchronously
      
      test <@ result |> Result.error |> List.contains expectedErr @>
    }

  [<Fact>]
  let ``returns Ok Some with parsed value if value is Include Some and parse returns Some`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let! value = GenX.auto<char>
      let! parsed = GenX.auto<int>
      let tryParse (_: char) : Async<int option> = async.Return (Some parsed)

      let result = Attribute.RequireNonNull(name, Include (Some value), tryParse) |> Async.RunSynchronously

      test <@ result = Ok parsed @>
    }

  [<Fact>]
  let ``returns InvalidParsed with correct data if value is Include Some and parse returns None`` () =
    Property.check <| property {
      let! name = Gen.memberName
      let! value = GenX.auto<char>
      let tryParse (_: char) : Async<int option> = async.Return None

      let expectedErr = RequestDocumentError.AttributeInvalidParsed (pointer name, None)

      let result = Attribute.RequireNonNull(name, Include (Some value), tryParse) |> Async.RunSynchronously

      test <@ result |> Result.error |> List.contains expectedErr @>
    }
