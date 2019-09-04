module FSharp.JsonApi.Serialization

  open System
  open System.Dynamic
  open System.Reflection
  open Newtonsoft.Json
  open Newtonsoft.Json.Linq
  open Newtonsoft.Json.Converters
  open FSharp.JsonSkippable
  open FSharp.JsonSkippable.Serialization


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

  type private ReflectionHelper =
    static member Box res = Resource.box res

  let private getConcreteBox = memoize (fun (targetType: Type) ->
    let genericBox = typeof<ReflectionHelper>.GetMethod("Box", BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Static)
    genericBox.MakeGenericMethod (targetType.GenericTypeArguments)
  )


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

      (target.GetType() |> getConcreteBox).Invoke(null, [|target|])



  /// Gets the serialization settings that should be used when serializing JSON-API
  /// documents. typeMap is a mapping from a JSON-API type name to the concrete
  /// Resource<'attrs, 'rels> that resource should be deserialized to.
  let internal getSettings (typeMap: Map<string, Type>) =
    let s =
      JsonSerializerSettings(
        NullValueHandling = NullValueHandling.Include,
        ContractResolver = SkippableContractResolver()
      )
    s.Converters.Add <| LinkConverter()
    s.Converters.Add <| LinksConverter()
    s.Converters.Add <| ResourceConverter(typeMap)
    s.Converters.Add <| StringEnumConverter()
    s.Converters.Add <| Microsoft.FSharpLu.Json.CompactUnionJsonConverter()
    s
