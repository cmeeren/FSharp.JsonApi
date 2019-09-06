module FSharp.JsonApi.Tests.SerializationTests

open System
open System.Collections.Generic
open System.Dynamic
open Xunit
open Hedgehog
open Swensen.Unquote
open Newtonsoft.Json
open FSharp.JsonSkippable
open FSharp.JsonApi

let serialize (x: 'a) =
  JsonConvert.SerializeObject(x, Serialization.getSettings Map.empty)

let serializeWith (settings: JsonSerializerSettings) (x: 'a) =
  JsonConvert.SerializeObject(x, settings)

let deserialize<'a> json =
  JsonConvert.DeserializeObject<'a>(json, Serialization.getSettings Map.empty)

let deserializeWith<'a> (settings: JsonSerializerSettings) json =
  JsonConvert.DeserializeObject<'a>(json, settings)

[<Fact>]
let ``serializing None gives null`` () =
  let json = serialize None
  test <@ json = "null" @>

[<Fact>]
let ``deserializing null to an option-wrapped value gives None`` () =
  let x = deserialize<int option> "null"
  test <@ x = None @>

[<Fact>]
let ``serializing Some gives the inner value`` () =
  Property.check <| property {
    let! value = GenX.auto<int>
    let json = serialize (Some value)
    test <@ json = sprintf "%i" value @>
  }

[<Fact>]
let ``deserializing a non-null value to an option-wrapped value wraps the value in Some`` () =
  Property.check <| property {
    let! value = GenX.auto<int>
    let x = deserialize<int option> (sprintf "%i" value)
    test <@ x = Some value @>
  }

[<Fact>]
let ``serializing excludes Skip members`` () =
  Property.check <| property {
    let! value = GenX.auto<int>
    let x = {| X = Skip; Y = value |}
    let json = serialize x
    test <@ json = sprintf """{"Y":%i}""" value @>
  }

[<Fact>]
let ``deserializing missing JSON properties to Skippable-wrapped properties gives Skip`` () =
  Property.check <| property {
    let! value = GenX.auto<int>
    let x = deserialize<{| X: int Skippable; Y: int |}> (sprintf """{"Y":%i}""" value)
    test <@ x = {| X = Skip; Y = value |} @>
  }

[<Fact>]
let ``serializing includes and unwraps Include members`` () =
  Property.check <| property {
    let! value = GenX.auto<int>
    let x = {| X = Include value |}
    let json = serialize x
    test <@ json = sprintf """{"X":%i}""" value @>
  }

[<Fact>]
let ``deserializing to a Skippable-wrapped value wraps JSON properties in Include`` () =
  Property.check <| property {
    let! value = GenX.auto<int>
    let x = deserialize<{| X: int Skippable |}> (sprintf """{"X":%i}""" value)
    test <@ x = {| X = Include value |} @>
  }

type EnumTest =
  | A = 1
  | b = 2

[<Fact>]
let ``serializing an enum gives the string name of the enum as-is (no casing difference)`` () =
  Property.check <| property {
    let! value = Gen.item [EnumTest.A; EnumTest.b]
    let json = serialize value
    test <@ json = "\"" + string value + "\"" @>
  }

[<Fact>]
let ``deserializing a string to an enum gives the correct enum case`` () =
  Property.check <| property {
    let! value = Gen.item [EnumTest.A; EnumTest.b]
    let x = deserialize ("\"" + string value + "\"")
    test <@ x = value @>
  }

[<Fact>]
let ``deserializing a non-existent string to an enum throws an exception`` () =
  Property.check <| property {
    let! value =
      Gen.string (Range.exponential 1 10) Gen.alpha
      |> GenX.iNotEqualTo "A"
      |> GenX.iNotEqualTo "B"
    raises <@ deserialize<EnumTest> ("\"" + string value + "\"") @>
  }

[<Fact>]
let ``serializing a Link where Meta is Skip gives the raw URL`` () =
  Property.check <| property {
    let! url = GenX.uri |> Gen.option
    let link = { Href = url; Meta = Skip }
    test <@ serialize link = serialize url @>
  }

[<Fact>]
let ``serializing a Link where Meta is Include gives a full Link object`` () =
  Property.check <| property {
    let! url = GenX.uri |> Gen.option
    let! meta = GenX.auto<Map<string, obj>>
    let link = { Href = url; Meta = Include meta }
    test <@ serialize link = sprintf """{"href":%s,"meta":%s}""" (serialize url) (serialize meta) @>
  }

[<Fact>]
let ``deserializing a Link without meta works`` () =
  Property.check <| property {
    let! url = GenX.uri |> Gen.option
    let x = deserialize<Link> (serialize url)
    test <@ x = { Href = url; Meta = Skip } @>
  }

[<Fact>]
let ``deserializing a Link with meta works`` () =
  Property.check <| property {
    let! url = GenX.uri |> Gen.option
    let json = sprintf """{"href":%s,"meta":{}}""" (serialize url)
    let x = deserialize<Link> json
    test <@ x.Href = url @>
    test <@ x.Meta.isInclude @>
  }

[<Fact>]
let ``serializing Links unwraps the link object`` () =
  let json = serialize (Links Map.empty)
  test <@ json = "{}" @>

[<Fact>]
let ``deserializing to Links wraps in Links`` () =
  let links = deserialize<Links> "{}"
  test <@ links = Links Map.empty @>

[<Fact>]
let ``correct property names for JsonApi`` () =
  let jsonApi = { Version = Include ""; Meta = Include Map.empty }
  let json = serialize jsonApi
  test <@ json = """{"version":"","meta":{}}""" @>

[<Fact>]
let ``correct property names for Link`` () =
  let link = { Href = None; Meta = Include Map.empty }
  let json = serialize link
  test <@ json = """{"href":null,"meta":{}}""" @>

[<Fact>]
let ``correct property names for ErrorSource`` () =
  let errorSource = { Pointer = Include ""; Parameter = Include "" }
  let json = serialize errorSource
  test <@ json = """{"pointer":"","parameter":""}""" @>

[<Fact>]
let ``correct property names for Error`` () =
  let error = { 
    Id = Include ""
    Links = Include (Links Map.empty)
    Status = Include ""
    Code = Include ""
    Title = Include ""
    Detail = Include ""
    Source = Include { Pointer = Skip; Parameter = Skip }
    Meta = Include Map.empty
  }
  let json = serialize error
  test <@ json = """{"id":"","links":{},"status":"","code":"","title":"","detail":"","source":{},"meta":{}}""" @>

[<Fact>]
let ``correct property names for ResourceIdentifier`` () =
  let identifier = { Type = ""; Id = "" }
  let json = serialize identifier
  test <@ json = """{"type":"","id":""}""" @>

[<Fact>]
let ``correct property names for ToOne`` () =
  let toOne = { ToOne.Links = Include (Links Map.empty); Data = Include None; Meta = Include Map.empty }
  let json = serialize toOne
  test <@ json = """{"links":{},"data":null,"meta":{}}""" @>

[<Fact>]
let ``correct property names for ToMany`` () =
  let toMany = { ToMany.Links = Include (Links Map.empty); Data = Include []; Meta = Include Map.empty }
  let json = serialize toMany
  test <@ json = """{"links":{},"data":[],"meta":{}}""" @>

[<Fact>]
let ``correct property names for Resource`` () =
  let resource = { 
    Type = ""
    Id = Include ""
    Attributes = Include <| obj()
    Links = Include (Links Map.empty)
    Relationships = Include <| obj()
    Meta = Include Map.empty
  }
  let json = serialize resource
  test <@ json = """{"type":"","id":"","attributes":{},"links":{},"relationships":{},"meta":{}}""" @>
  
[<Fact>]
let ``correct property names for ResourceDocument`` () =
  let doc = { 
    ResourceDocument.JsonApi = Include { Version = Include ""; Meta = Skip }
    Links = Include (Links Map.empty)
    Meta = Include Map.empty
    Data = None
    Included = Include []
  }
  let json = serialize doc
  test <@ json = """{"jsonapi":{"version":""},"links":{},"meta":{},"data":null,"included":[]}""" @>

[<Fact>]
let ``correct property names for ResourceCollectionDocument`` () =
  let doc = { 
    ResourceCollectionDocument.JsonApi = Include { Version = Include ""; Meta = Skip }
    Links = Include (Links Map.empty)
    Meta = Include Map.empty
    Data = []
    Included = Include []
  }
  let json = serialize doc
  test <@ json = """{"jsonapi":{"version":""},"links":{},"meta":{},"data":[],"included":[]}""" @>

[<Fact>]
let ``correct property names for ResourceIdentifierDocument`` () =
  let doc = { 
    ResourceIdentifierDocument.JsonApi = Include { Version = Include ""; Meta = Skip }
    Links = Include (Links Map.empty)
    Meta = Include Map.empty
    Data = None
  }
  let json = serialize doc
  test <@ json = """{"jsonapi":{"version":""},"links":{},"meta":{},"data":null}""" @>

[<Fact>]
let ``correct property names for ResourceIdentifierCollectionDocument`` () =
  let doc = { 
    ResourceIdentifierCollectionDocument.JsonApi = Include { Version = Include ""; Meta = Skip }
    Links = Include (Links Map.empty)
    Meta = Include Map.empty
    Data = []
  }
  let json = serialize doc
  test <@ json = """{"jsonapi":{"version":""},"links":{},"meta":{},"data":[]}""" @>

[<Fact>]
let ``correct property names for ErrorDocument`` () =
  let doc = { 
    JsonApi = Include { Version = Include ""; Meta = Skip }
    Errors = []
    Links = Include (Links Map.empty)
    Meta = Include Map.empty
  }
  let json = serialize doc
  test <@ json = """{"jsonapi":{"version":""},"errors":[],"links":{},"meta":{}}""" @>

type A = { X: int }
type B = { Y: string }

[<Fact>]
let ``deserializes resources to correct type based on type map`` () =
  let typeMap = 
    [ "AB", typeof<Resource<A,B>>
      "BA", typeof<Resource<B,A>> ]
    |> Map.ofList
  let settings = Serialization.getSettings typeMap

  let json = """{"type":"AB","attributes":{"X":1,"Y":"A"},"relationships":{"X":2,"Y":"B"}}"""
  let res : Resource<A,B> = 
    deserializeWith<Resource<obj,obj>> settings json
    |> Resource.unbox
  test <@ res.Attributes.Value.X = 1 @>
  test <@ res.Relationships.Value.Y = "B" @>

  let json = """{"type":"BA","attributes":{"X":1,"Y":"A"},"relationships":{"X":2,"Y":"B"}}"""
  let res : Resource<B,A> = 
    deserializeWith<Resource<obj,obj>> settings json
    |> Resource.unbox
  test <@ res.Attributes.Value.Y = "A" @>
  test <@ res.Relationships.Value.X = 2 @>

[<Fact>]
let ``deserializes unknown resources using nested ExpandoObjects`` () =
  let typeMap = 
    [ "AB", typeof<Resource<A,B>>
      "BA", typeof<Resource<B,A>> ]
    |> Map.ofList
  let settings = Serialization.getSettings typeMap

  let json = """{"type":"C","attributes":{"X":1,"Y":"A"},"relationships":{"X":2,"Y":"B","Z":{"C": 3}}}"""
  let res : Resource<ExpandoObject,ExpandoObject> = 
    deserializeWith<Resource<obj,obj>> settings json |> Resource.unbox

  let attrs = res.Attributes.Value :> IDictionary<string, obj>
  let rels = res.Relationships.Value :> IDictionary<string, obj>

  test <@ attrs.["X"] |> unbox = 1L @>
  test <@ attrs.["Y"] |> unbox = "A" @>
  test <@ rels.["X"] |> unbox = 2L @>
  test <@ rels.["Y"] |> unbox = "B" @>
  test <@ (rels.["Z"] :?> IDictionary<string, obj>).["C"] |> unbox = 3L @>


[<Fact>]
let ``deserializes meta using nested maps`` () =
  let settings = Serialization.getSettings Map.empty

  let json = """{"meta":{"X":2,"Y":"B","Z":{"C": 3}}}"""
  let res = deserializeWith<JsonApi> settings json

  let expected =
    Map.empty
    |> Map.add "X" (box 2L)
    |> Map.add "Y" (box "B")
    |> Map.add "Z" (Map.empty |> Map.add "C" (box 3L) |> box)

  test <@ res.Meta.Value = expected @>
