module FSharp.JsonApi.Tests.Gen

  open Hedgehog
  open FSharp.JsonSkippable
  open FSharp.JsonApi

  let private memberNameAnyChar = Gen.alphaNum
  let private memberNameNonFirstLastChar = Gen.choice [Gen.alphaNum; Gen.constant '-'; Gen.constant '_']

  /// Generates a valid JSON-API member name. Does not include U+0020 SPACE or
  /// U+0080 and above (these are not recommended by the spec since they are not
  /// URL safe).
  let memberName = gen {
    let! first = memberNameAnyChar |> Gen.map string
    let! middle = memberNameNonFirstLastChar |> Gen.string (Range.constant 0 8)
    if middle = "" then
      return first + middle
    else
      let! last = memberNameAnyChar |> Gen.map string
      return first + middle + last
  }

  /// Generates a fieldset specification.
  let fieldsets : Gen<Fieldsets> =
    memberName
    |> Gen.list (Range.exponential 1 10)
    |> Gen.list (Range.exponential 0 10)
    |> Gen.map (fun typeNames -> 
        typeNames
        |> List.map (fun fieldNames ->
          match fieldNames with
          | [] -> failwith "Should never happen"
          | hd::tl -> hd, Set.ofList tl
        )
        |> Map.ofList
    )

  /// Generates a fieldset specification guaranteed to contain the specified
  /// type and field.
  let fieldsetsWith typeName fieldName =
    fieldsets
    |> Gen.map (Map.addOrUpdate typeName (Set.empty.Add fieldName) (Set.add fieldName))

  /// Generates a fieldset specification guaranteed not to contain the specified
  /// type.
  let fieldsetsWithoutType typeName =
    fieldsets |> Gen.filter (not << Map.containsKey typeName)

  /// Generates a fieldset specification guaranteed not to contain the specified
  /// type without the specified field.
  let fieldsetsWithTypeWithoutField typeName fieldName =
    fieldsets
    |> Gen.map (Map.addOrUpdate typeName Set.empty (Set.remove fieldName))

  /// Generates a list of include paths.
  let includePaths : Gen<IncludePath list> =
    memberName
    |> Gen.list (Range.linear 0 10)
    |> Gen.list (Range.linear 0 10)

  /// Returns include paths that are guaranteed to not start with the specified
  /// relationship name.
  let includePathsWithoutHead relName =
    includePaths
    |> Gen.filter (List.forall (fun path -> path |> List.tryHead <> Some relName))

  /// Returns include paths where at least one path is guaranteed to start with
  /// the specified relationship name.
  let includePathsWithHead relName = gen {
    let! path = memberName |> Gen.list (Range.linear 0 10)
    return! includePaths |> GenX.addElement (relName :: path)
  }

  /// Generates 'Skip' part of the time.
  let skippable gen =
    gen |> Gen.option |> Gen.map Skippable.ofOption

  /// Generates a resource identifier
  let resourceIdentifier = gen {
    let! t = memberName
    let! id = memberName
    return { Type = t; Id = id }
  }
