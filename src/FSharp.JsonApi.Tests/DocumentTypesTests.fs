module FSharp.JsonApi.Tests.DocumentTypesTests

open System
open Xunit
open Hedgehog
open Swensen.Unquote
open FSharp.JsonSkippable
open FSharp.JsonApi

module JsonApi =

  [<Fact>]
  let ``v1_0 has correct version`` () =
    test <@ JsonApi.v1_0.Version = Include "1.0" @>

  [<Fact>]
  let ``v1_0 has no meta`` () =
    test <@ JsonApi.v1_0.Meta = Skip @>

  [<Fact>]
  let ``addMeta adds (only) the specified key and value to Meta, overwriting existing values`` () =
    Property.check <| property {
      let! original = GenX.auto<JsonApi>
      let! key = GenX.auto<string>
      let! value1 = GenX.auto<int>
      let! value2 = GenX.auto<int> |> GenX.notEqualTo value1

      let updated =
        original
        |> JsonApi.addMeta key value1
        |> JsonApi.addMeta key value2

      test
        <@ match original.Meta with
           | Skip ->
              updated = { original with Meta = Include (Map.empty.Add(key, box value2)) }
           | Include m ->
              updated = { original with Meta = Include (m.Add(key, value2)) }
        @>
    }

  [<Fact>]
  let ``addMetaIf adds (only) the specified key and value to Meta, overwriting existing values, if condition is true`` () =
    Property.check <| property {
      let! original = GenX.auto<JsonApi>
      let! key = GenX.auto<string>
      let! value1 = GenX.auto<int>
      let! value2 = GenX.auto<int> |> GenX.notEqualTo value1

      let updated =
        original
        |> JsonApi.addMetaIf true key value1
        |> JsonApi.addMetaIf true key value2

      test
        <@ match original.Meta with
           | Skip ->
              updated = { original with Meta = Include (Map.empty.Add(key, box value2)) }
           | Include m ->
              updated = { original with Meta = Include (m.Add(key, value2)) }
        @>
    }

  [<Fact>]
  let ``addMetaIf does not modify the object when condition is false`` () =
    Property.check <| property {
      let! original = GenX.auto<JsonApi>
      let! key = GenX.auto<string>
      let! value = GenX.auto<int>

      let updated = original |> JsonApi.addMetaIf false key value

      test <@ updated = original @>
    }


module Link =

  [<Fact>]
  let ``self has correct value`` () =
    test <@ Link.self = "self" @>

  [<Fact>]
  let ``related has correct value`` () =
    test <@ Link.related = "related" @>

  [<Fact>]
  let ``about has correct value`` () =
    test <@ Link.about = "about" @>

  [<Fact>]
  let ``first has correct value`` () =
    test <@ Link.first = "first" @>

  [<Fact>]
  let ``last has correct value`` () =
    test <@ Link.last = "last" @>

  [<Fact>]
  let ``prev has correct value`` () =
    test <@ Link.prev = "prev" @>

  [<Fact>]
  let ``next has correct value`` () =
    test <@ Link.next = "next" @>

  [<Fact>]
  let ``addMeta adds (only) the specified key and value to Meta, overwriting existing values`` () =
    Property.check <| property {
      let! original = GenX.auto<Link>
      let! key = GenX.auto<string>
      let! value1 = GenX.auto<int>
      let! value2 = GenX.auto<int> |> GenX.notEqualTo value1

      let updated =
        original
        |> Link.addMeta key value1
        |> Link.addMeta key value2

      test
        <@ match original.Meta with
           | Skip ->
              updated = { original with Meta = Include (Map.empty.Add(key, box value2)) }
           | Include m ->
              updated = { original with Meta = Include (m.Add(key, value2)) }
        @>
    }

  [<Fact>]
  let ``addMetaIf adds (only) the specified key and value to Meta, overwriting existing values, if condition is true`` () =
    Property.check <| property {
      let! original = GenX.auto<Link>
      let! key = GenX.auto<string>
      let! value1 = GenX.auto<int>
      let! value2 = GenX.auto<int> |> GenX.notEqualTo value1

      let updated =
        original
        |> Link.addMetaIf true key value1
        |> Link.addMetaIf true key value2

      test
        <@ match original.Meta with
           | Skip ->
              updated = { original with Meta = Include (Map.empty.Add(key, box value2)) }
           | Include m ->
              updated = { original with Meta = Include (m.Add(key, value2)) }
        @>
    }

  [<Fact>]
  let ``addMetaIf does not modify the object when condition is false`` () =
    Property.check <| property {
      let! original = GenX.auto<Link>
      let! key = GenX.auto<string>
      let! value = GenX.auto<int>

      let updated = original |> Link.addMetaIf false key value

      test <@ updated = original @>
    }


module Links =

  [<Fact>]
  let ``create returns a link collection with the specified link name and URI and no meta`` () =
    Property.check <| property {
      let! name = GenX.auto<string>
      let! uri = GenX.auto<Uri>

      let (Links links) = Links.create name uri

      test <@ (links.TryFind name).Value.Href.Value = uri @>
      test <@ (links.TryFind name).Value.Meta = Skip @>
    }

  [<Fact>]
  let ``createWithMeta returns a link collection with the specified link name and meta and no href`` () =
    Property.check <| property {
      let! name = GenX.auto<string>
      let! key, value = GenX.auto<string * int>

      let (Links links) = Links.createWithMeta name [key, value]

      test <@ (links.TryFind name).Value.Meta.Value.TryFind(key).Value = box value @>
    }

  [<Fact>]
  let ``createWithMeta does not add meta if the list is empty`` () =
    Property.check <| property {
      let! name = GenX.auto<string>

      let (Links links) = Links.createWithMeta name []

      test <@ (links.TryFind name).Value.Meta = Skip @>
    }

  [<Fact>]
  let ``createNull returns a link collection with the specified link with Href = None and Meta = Skip`` () =
    Property.check <| property {
      let! name = GenX.auto<string>

      let (Links links) = Links.createNull name

      test <@ (links.TryFind name).Value = { Href = None; Meta = Skip } @>
    }

  // TODO
