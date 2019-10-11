module FSharp.JsonApi.Serialization

  open System
  open System.Dynamic
  open Newtonsoft.Json
  open Newtonsoft.Json.Linq
  open Newtonsoft.Json.Converters
  open FSharp.JsonSkippable
  open FSharp.JsonSkippable.Serialization


  // Serialize the actual Uri objects using Uri.ToString() instead of
  // Newtonsoft.Json's default Uri.OriginalString to avoid unwanted
  // ports when the Uri has been transformed by UriBuilder.
  //  - https://github.com/JamesNK/Newtonsoft.Json/issues/2190
  //  - https://github.com/dotnet/corefx/issues/41679
  type UriConverter() =
    inherit JsonConverter()

    override __.CanConvert (t: Type) =
      t = typeof<Uri>

    override __.WriteJson(writer: JsonWriter, value: obj, serializer: JsonSerializer) =
      match value with
      | :? Uri as uri -> serializer.Serialize(writer, uri.ToString())
      | x -> failwith (sprintf "Uri converter got unknown type %s" <| x.GetType().AssemblyQualifiedName)

    override __.ReadJson(reader: JsonReader, t: Type, existing: obj, serializer: JsonSerializer) =
      serializer.Deserialize<string>(reader) |> Uri |> box


  type LinkConverter() =
    inherit JsonConverter()

    override __.CanConvert (t: Type) =
      t = typeof<Link>

    override __.WriteJson(writer: JsonWriter, value: obj, serializer: JsonSerializer) =
      match value with
      | :? Link as link ->
          if link.Meta = Skip then serializer.Serialize(writer, link.Href)
          else
            writer.WriteStartObject()
            writer.WritePropertyName "href"
            serializer.Serialize(writer, link.Href)
            writer.WritePropertyName "meta"
            serializer.Serialize(writer, link.Meta)
            writer.WriteEndObject()
      | x -> failwith (sprintf "Link converter got unknown type %s" <| x.GetType().AssemblyQualifiedName)

    override __.ReadJson(reader: JsonReader, t: Type, existing: obj, serializer: JsonSerializer) =
      let token = JToken.ReadFrom reader
      match token.Type with
      | JTokenType.Null ->
          box { Href = None; Meta = Skip }
      | JTokenType.String ->
          let url = serializer.Deserialize<_>(token.CreateReader())
          box { Href = url; Meta = Skip }
      | _ ->
          let href = serializer.Deserialize<_>(token.["href"].CreateReader())
          let meta = serializer.Deserialize<_>(token.["meta"].CreateReader())
          box { Href = href; Meta = meta }


  type LinksConverter() =
    inherit JsonConverter()

    override __.CanConvert (t: Type) =
      t = typeof<Links>

    override __.WriteJson(writer: JsonWriter, value: obj, serializer: JsonSerializer) =
      match value with
      | :? Links as linkColl ->
          let (Links links) = linkColl
          serializer.Serialize(writer, links)
      | x -> failwith (sprintf "Link collection converter got unknown type %s" <| x.GetType().AssemblyQualifiedName)

    override __.ReadJson(reader: JsonReader, t: Type, existing: obj, serializer: JsonSerializer) =
      let token = JToken.ReadFrom reader
      serializer.Deserialize<Map<string, Link>>(token.CreateReader()) |> Links |> box


  /// typeMap is a mapping from a JSON-API type name to the concrete
  /// Resource<'attrs, 'rels> that resource should be deserialized to.
  type ResourceConverter(typeMap: Map<string, Type>) =
    inherit JsonConverter()

    override __.CanConvert (t: Type) =
      t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<Resource<_,_>>

    override __.CanWrite = false

    override __.WriteJson(writer: JsonWriter, value: obj, serializer: JsonSerializer) =
      failwith "ResourceConverter can only read, not write"

    override __.ReadJson(reader: JsonReader, t: Type, existing: obj, serializer: JsonSerializer) =
      let item = JObject.Load(reader)
      let typeName = item.Value "type" |> Option.ofObj

      let target =
        match typeName |> Option.bind typeMap.TryFind with
        | Some t -> Activator.CreateInstance(t)
        | None -> Activator.CreateInstance(typeof<Resource<ExpandoObject,ExpandoObject>>)

      serializer.Populate(item.CreateReader(), target)
      Resource.dynamicBox (target.GetType()) target |> box


  type MetaConverter() =
    inherit JsonConverter()

    override __.CanConvert (t: Type) =
      t = typeof<Map<string, obj>>

    override __.CanWrite = false

    override __.WriteJson(writer: JsonWriter, value: obj, serializer: JsonSerializer) =
      failwith "MetaConverter can only read, not write"

    override __.ReadJson(reader: JsonReader, t: Type, existing: obj, serializer: JsonSerializer) =
      let token = JToken.ReadFrom reader
      let o = serializer.Deserialize<ExpandoObject>(token.CreateReader())
      let rec expandoToMap (o: ExpandoObject) =
        (o :> Collections.Generic.IDictionary<string, obj>)
        |> Seq.map (fun kvp ->
            let value =
              match kvp.Value with
              | :? ExpandoObject as o -> expandoToMap o |> box
              | v -> v
            kvp.Key, value
        )
        |> Map.ofSeq
      o |> expandoToMap |> box


  /// Gets the serialization settings that should be used when serializing JSON-API
  /// documents. typeMap is a mapping from a JSON-API type name to the concrete
  /// Resource<'attrs, 'rels> that resource should be deserialized to.
  let internal getSettings (typeMap: Map<string, Type>) =
    let s =
      JsonSerializerSettings(
        NullValueHandling = NullValueHandling.Include,
        ContractResolver = SkippableContractResolver()
      )
    s.Converters.Add <| UriConverter()
    s.Converters.Add <| LinkConverter()
    s.Converters.Add <| LinksConverter()
    s.Converters.Add <| ResourceConverter(typeMap)
    s.Converters.Add <| MetaConverter()
    s.Converters.Add <| StringEnumConverter()
    s.Converters.Add <| Microsoft.FSharpLu.Json.CompactUnionJsonConverter()
    s
