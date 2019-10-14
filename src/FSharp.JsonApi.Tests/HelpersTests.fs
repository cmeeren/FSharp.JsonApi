module FSharp.JsonApi.Tests.HelpersTests

open Xunit
open Hedgehog
open Swensen.Unquote
open FSharp.JsonApi


module Uri =


  module addSegment =

    [<Fact>]
    let ``does not change scheme, authority, query, or fragment`` () =
      Property.check <| property {
        let! uri = GenX.uri
        let! segment = GenX.lString 1 10 Gen.alphaNum

        let newUri = uri |> Uri.addSegment segment

        test <@ uri.Scheme = newUri.Scheme @>
        test <@ uri.Authority = newUri.Authority @>
        test <@ uri.Query = newUri.Query @>
        test <@ uri.Fragment = newUri.Fragment @>
      }

    [<Fact>]
    let ``new path ends with single slash plus specified segment`` () =
      Property.check <| property {
        let! uri = GenX.uri
        let! segment = GenX.lString 1 10 Gen.alphaNum

        let newUri = uri |> Uri.addSegment segment

        test <@ newUri.AbsolutePath.EndsWith("/" + segment) @>
        test <@ newUri.AbsolutePath.EndsWith("//" + segment) |> not @>
      }

    [<Fact>]
    let ``new path starts with old path with slashes trimmed from end, plus single slash`` () =
      Property.check <| property {
        let! uri = GenX.uri
        let! segment = GenX.lString 1 10 Gen.alphaNum

        let newUri = uri |> Uri.addSegment segment

        test <@ newUri.AbsolutePath.StartsWith(uri.AbsolutePath.TrimEnd('/') + "/") @>
        test <@ newUri.AbsolutePath.StartsWith(uri.AbsolutePath.TrimEnd('/') + "//") |> not @>
      }

  module addSegments =

    [<Fact>]
    let ``does not change scheme, authority, query, or fragment`` () =
      Property.check <| property {
        let! uri = GenX.uri
        let! segments =
          GenX.lString 1 10 Gen.alphaNum
          |> GenX.lList 1 5

        let newUri = uri |> Uri.addSegments segments

        test <@ uri.Scheme = newUri.Scheme @>
        test <@ uri.Authority = newUri.Authority @>
        test <@ uri.Query = newUri.Query @>
        test <@ uri.Fragment = newUri.Fragment @>
      }

    [<Fact>]
    let ``new path ends with single slash plus specified segments joined by slash`` () =
      Property.check <| property {
        let! uri = GenX.uri
        let! segments =
          GenX.lString 1 10 Gen.alphaNum
          |> GenX.lList 1 5

        let newUri = uri |> Uri.addSegments segments

        test <@ newUri.AbsolutePath.EndsWith("/" + String.concat "/" segments) @>
        test <@ newUri.AbsolutePath.EndsWith("//" + String.concat "/" segments) |> not @>
      }

    [<Fact>]
    let ``new path starts with old path with slashes trimmed from end, plus single slash`` () =
      Property.check <| property {
        let! uri = GenX.uri
        let! segments =
          GenX.lString 1 10 Gen.alphaNum
          |> GenX.lList 1 5

        let newUri = uri |> Uri.addSegments segments

        test <@ newUri.AbsolutePath.StartsWith(uri.AbsolutePath.TrimEnd('/') + "/") @>
        test <@ newUri.AbsolutePath.StartsWith(uri.AbsolutePath.TrimEnd('/') + "//") |> not @>
      }


  module addQuery =

    [<Fact>]
    let ``does not change scheme, authority, path, or fragment`` () =
      Property.check <| property {
        let! uri = GenX.uri
        let! key, value = GenX.lString 1 10 Gen.alphaNum |> Gen.tuple

        let newUri = uri |> Uri.addQuery key value

        test <@ uri.Scheme = newUri.Scheme @>
        test <@ uri.Authority = newUri.Authority @>
        test <@ uri.AbsolutePath = newUri.AbsolutePath @>
        test <@ uri.Fragment = newUri.Fragment @>
      }

    [<Fact>]
    let ``new query ends with the specified key-value pair`` () =
      Property.check' 1000<tests> <| property {
        let! uri = GenX.uri
        let! key, value = GenX.lString 1 10 Gen.alphaNum |> Gen.tuple

        let newUri = uri |> Uri.addQuery key value

        test <@ newUri.Query.EndsWith(key + "=" + value) @>
      }

    [<Fact>]
    let ``new query starts with the original query`` () =
      Property.check' 1000<tests> <| property {
        let! uri = GenX.uri
        let! key, value = GenX.lString 1 10 Gen.alphaNum |> Gen.tuple

        let newUri = uri |> Uri.addQuery key value

        test <@ newUri.Query.StartsWith uri.Query @>
      }


  module setQuery =

    [<Fact>]
    let ``does not change scheme, authority, path, or fragment`` () =
      Property.check <| property {
        let! uri = GenX.uri
        let! key, value = GenX.lString 1 10 Gen.alphaNum |> Gen.tuple

        let newUri = uri |> Uri.setQuery key value

        test <@ uri.Scheme = newUri.Scheme @>
        test <@ uri.Authority = newUri.Authority @>
        test <@ uri.AbsolutePath = newUri.AbsolutePath @>
        test <@ uri.Fragment = newUri.Fragment @>
      }

    [<Fact>]
    let ``new query ends with the specified key-value pair`` () =
      Property.check' 1000<tests> <| property {
        let! uri = GenX.uri
        let! key, value = GenX.lString 1 10 Gen.alphaNum |> Gen.tuple

        let newUri = uri |> Uri.setQuery key value

        test <@ newUri.Query.EndsWith(key + "=" + value) @>
      }

    [<Fact>]
    let ``new query contains only one instance of the specified key`` () =
      Property.check' 1000<tests> <| property {
        let! uri = GenX.uri
        let! key, value = GenX.lString 1 10 Gen.alphaNum |> Gen.tuple

        let newUri = uri |> Uri.setQuery key value

        test
          <@
            newUri.Query.TrimStart('?')
            |> String.split "&"
            |> List.map (String.split "=")
            |> List.filter (fun kv -> kv |> List.tryItem 0 = Some key)
            |> List.length
              = 1
          @>
      }
