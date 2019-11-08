module FSharp.JsonApi.Tests.SetterTests

open Xunit
open FSharp.JsonApi

type Entity = Entity
type Arg = Arg
type SetError = SetError
type ApiError = ApiError

let mapRequestDocError (_: RequestDocumentError) = ApiError
let mapQueryError (_: QueryError) = ApiError
let mapSetError (_: SetError) = ApiError

let set = Setter(mapRequestDocError, mapQueryError)


module Optional_simpleSetter_requestDocArg =

  [<Fact>]
  let ``can resolve overload`` () =
    let __ () =
      let setter : Arg -> Entity -> Entity = failwith ""
      let parsedArg : Result<Arg option, RequestDocumentError list> = failwith ""
      set.Optional(setter, parsedArg) |> ignore
    true


module Optional_simpleSetter_requestDocArg_async =

  [<Fact>]
  let ``can resolve overload`` () =
    let __ () =
      let setter : Arg -> Entity -> Entity = failwith ""
      let parsedArg : Async<Result<Arg option, RequestDocumentError list>> = failwith ""
      set.Optional(setter, parsedArg) |> ignore
    true


module Optional_simpleSetter_queryArg =

  [<Fact>]
  let ``can resolve overload`` () =
    let __ () =
      let setter : Arg -> Entity -> Entity = failwith ""
      let parsedArg : Result<Arg option, QueryError list> = failwith ""
      set.Optional(setter, parsedArg) |> ignore
    true


module Optional_simpleSetter_queryArg_async =

  [<Fact>]
  let ``can resolve overload`` () =
    let __ () =
      let setter : Arg -> Entity -> Entity = failwith ""
      let parsedArg : Async<Result<Arg option, QueryError list>> = failwith ""
      set.Optional(setter, parsedArg) |> ignore
    true


module Optional_simpleSetter_simpleArg =

  [<Fact>]
  let ``can resolve overload`` () =
    let __ () =
      let setter : Arg -> Entity -> Entity = failwith ""
      let arg : Arg option = failwith ""
      set.Optional(setter, arg) |> ignore
    true


module Optional_simpleSetter_simpleArg_async =

  [<Fact>]
  let ``can resolve overload`` () =
    let __ () =
      let setter : Arg -> Entity -> Entity = failwith ""
      let arg : Async<Arg option> = failwith ""
      set.Optional(setter, arg) |> ignore
    true


module Optional_resultListSetter_requestDocArg =

  [<Fact>]
  let ``can resolve overload`` () =
    let __ () =
      let setter : Arg -> Entity -> Result<Entity, SetError list> = failwith ""
      let parsedArg : Result<Arg option, RequestDocumentError list> = failwith ""
      set.Optional(setter, mapSetError, parsedArg) |> ignore
    true


module Optional_resultListSetter_requestDocArg_async =

  [<Fact>]
  let ``can resolve overload`` () =
    let __ () =
      let setter : Arg -> Entity -> Result<Entity, SetError list> = failwith ""
      let parsedArg : Async<Result<Arg option, RequestDocumentError list>> = failwith ""
      set.Optional(setter, mapSetError, parsedArg) |> ignore
    true


module Optional_resultListSetter_queryArg =

  [<Fact>]
  let ``can resolve overload`` () =
    let __ () =
      let setter : Arg -> Entity -> Result<Entity, SetError list> = failwith ""
      let parsedArg : Result<Arg option, QueryError list> = failwith ""
      set.Optional(setter, mapSetError, parsedArg) |> ignore
    true


module Optional_resultListSetter_queryArg_async =

  [<Fact>]
  let ``can resolve overload`` () =
    let __ () =
      let setter : Arg -> Entity -> Result<Entity, SetError list> = failwith ""
      let parsedArg : Async<Result<Arg option, QueryError list>> = failwith ""
      set.Optional(setter, mapSetError, parsedArg) |> ignore
    true


module Optional_resultListSetter_simpleArg =

  [<Fact>]
  let ``can resolve overload`` () =
    let __ () =
      let setter : Arg -> Entity -> Result<Entity, SetError list> = failwith ""
      let arg : Arg option = failwith ""
      set.Optional(setter, mapSetError, arg) |> ignore
    true


module Optional_resultListSetter_simpleArg_async =

  [<Fact>]
  let ``can resolve overload`` () =
    let __ () =
      let setter : Arg -> Entity -> Result<Entity, SetError list> = failwith ""
      let arg : Async<Arg option> = failwith ""
      set.Optional(setter, mapSetError, arg) |> ignore
    true


module Optional_resultSingleSetter_requestDocArg =

  [<Fact>]
  let ``can resolve overload`` () =
    let __ () =
      let setter : Arg -> Entity -> Result<Entity, SetError> = failwith ""
      let parsedArg : Result<Arg option, RequestDocumentError list> = failwith ""
      set.Optional(setter, mapSetError, parsedArg) |> ignore
    true


module Optional_resultSingleSetter_requestDocArg_async =

  [<Fact>]
  let ``can resolve overload`` () =
    let __ () =
      let setter : Arg -> Entity -> Result<Entity, SetError> = failwith ""
      let parsedArg : Async<Result<Arg option, RequestDocumentError list>> = failwith ""
      set.Optional(setter, mapSetError, parsedArg) |> ignore
    true


module Optional_resultSingleSetter_queryArg =

  [<Fact>]
  let ``can resolve overload`` () =
    let __ () =
      let setter : Arg -> Entity -> Result<Entity, SetError> = failwith ""
      let parsedArg : Result<Arg option, QueryError list> = failwith ""
      set.Optional(setter, mapSetError, parsedArg) |> ignore
    true


module Optional_resultSingleSetter_queryArg_async =

  [<Fact>]
  let ``can resolve overload`` () =
    let __ () =
      let setter : Arg -> Entity -> Result<Entity, SetError> = failwith ""
      let parsedArg : Async<Result<Arg option, QueryError list>> = failwith ""
      set.Optional(setter, mapSetError, parsedArg) |> ignore
    true


module Optional_resultSingleSetter_simpleArg =

  [<Fact>]
  let ``can resolve overload`` () =
    let __ () =
      let setter : Arg -> Entity -> Result<Entity, SetError> = failwith ""
      let arg : Arg option = failwith ""
      set.Optional(setter, mapSetError, arg) |> ignore
    true


module Optional_resultSingleSetter_simpleArg_async =

  [<Fact>]
  let ``can resolve overload`` () =
    let __ () =
      let setter : Arg -> Entity -> Result<Entity, SetError> = failwith ""
      let arg : Async<Arg option> = failwith ""
      set.Optional(setter, mapSetError, arg) |> ignore
    true


module Required_simpleSetter_requestDocArg =

  [<Fact>]
  let ``can resolve overload`` () =
    let __ () =
      let setter : Arg -> Entity -> Entity = failwith ""
      let parsedArg : Result<Arg, RequestDocumentError list> = failwith ""
      set.Required(setter, parsedArg) |> ignore
    true


module Required_simpleSetter_requestDocArg_async =

  [<Fact>]
  let ``can resolve overload`` () =
    let __ () =
      let setter : Arg -> Entity -> Entity = failwith ""
      let parsedArg : Async<Result<Arg, RequestDocumentError list>> = failwith ""
      set.Required(setter, parsedArg) |> ignore
    true


module Required_simpleSetter_queryArg =

  [<Fact>]
  let ``can resolve overload`` () =
    let __ () =
      let setter : Arg -> Entity -> Entity = failwith ""
      let parsedArg : Result<Arg, QueryError list> = failwith ""
      set.Required(setter, parsedArg) |> ignore
    true


module Required_simpleSetter_queryArg_async =

  [<Fact>]
  let ``can resolve overload`` () =
    let __ () =
      let setter : Arg -> Entity -> Entity = failwith ""
      let parsedArg : Async<Result<Arg, QueryError list>> = failwith ""
      set.Required(setter, parsedArg) |> ignore
    true


module Required_simpleSetter_simpleArg =

  [<Fact>]
  let ``can resolve overload`` () =
    let __ () =
      let setter : Arg -> Entity -> Entity = failwith ""
      let arg : Arg = failwith ""
      set.Required(setter, arg) |> ignore
    true


module Required_simpleSetter_simpleArg_async =

  [<Fact>]
  let ``can resolve overload`` () =
    let __ () =
      let setter : Arg -> Entity -> Entity = failwith ""
      let arg : Async<Arg> = failwith ""
      set.Required(setter, arg) |> ignore
    true


module Required_resultListSetter_requestDocArg =

  [<Fact>]
  let ``can resolve overload`` () =
    let __ () =
      let setter : Arg -> Entity -> Result<Entity, SetError list> = failwith ""
      let parsedArg : Result<Arg, RequestDocumentError list> = failwith ""
      set.Required(setter, mapSetError, parsedArg) |> ignore
    true


module Required_resultListSetter_requestDocArg_async =

  [<Fact>]
  let ``can resolve overload`` () =
    let __ () =
      let setter : Arg -> Entity -> Result<Entity, SetError list> = failwith ""
      let parsedArg : Async<Result<Arg, RequestDocumentError list>> = failwith ""
      set.Required(setter, mapSetError, parsedArg) |> ignore
    true


module Required_resultListSetter_queryArg =

  [<Fact>]
  let ``can resolve overload`` () =
    let __ () =
      let setter : Arg -> Entity -> Result<Entity, SetError list> = failwith ""
      let parsedArg : Result<Arg, QueryError list> = failwith ""
      set.Required(setter, mapSetError, parsedArg) |> ignore
    true


module Required_resultListSetter_queryArg_async =

  [<Fact>]
  let ``can resolve overload`` () =
    let __ () =
      let setter : Arg -> Entity -> Result<Entity, SetError list> = failwith ""
      let parsedArg : Async<Result<Arg, QueryError list>> = failwith ""
      set.Required(setter, mapSetError, parsedArg) |> ignore
    true


module Required_resultListSetter_simpleArg =

  [<Fact>]
  let ``can resolve overload`` () =
    let __ () =
      let setter : Arg -> Entity -> Result<Entity, SetError list> = failwith ""
      let parsedArg : Arg = failwith ""
      set.Required(setter, mapSetError, parsedArg) |> ignore
    true


module Required_resultListSetter_simpleArg_async =

  [<Fact>]
  let ``can resolve overload`` () =
    let __ () =
      let setter : Arg -> Entity -> Result<Entity, SetError list> = failwith ""
      let parsedArg : Async<Arg> = failwith ""
      set.Required(setter, mapSetError, parsedArg) |> ignore
    true


module Required_resultSingleSetter_requestDocArg =

  [<Fact>]
  let ``can resolve overload`` () =
    let __ () =
      let setter : Arg -> Entity -> Result<Entity, SetError> = failwith ""
      let parsedArg : Result<Arg, RequestDocumentError list> = failwith ""
      set.Required(setter, mapSetError, parsedArg) |> ignore
    true


module Required_resultSingleSetter_requestDocArg_async =

  [<Fact>]
  let ``can resolve overload`` () =
    let __ () =
      let setter : Arg -> Entity -> Result<Entity, SetError> = failwith ""
      let parsedArg : Async<Result<Arg, RequestDocumentError list>> = failwith ""
      set.Required(setter, mapSetError, parsedArg) |> ignore
    true


module Required_resultSingleSetter_queryArg =

  [<Fact>]
  let ``can resolve overload`` () =
    let __ () =
      let setter : Arg -> Entity -> Result<Entity, SetError> = failwith ""
      let parsedArg : Result<Arg, QueryError list> = failwith ""
      set.Required(setter, mapSetError, parsedArg) |> ignore
    true


module Required_resultSingleSetter_queryArg_async =

  [<Fact>]
  let ``can resolve overload`` () =
    let __ () =
      let setter : Arg -> Entity -> Result<Entity, SetError> = failwith ""
      let parsedArg : Async<Result<Arg, QueryError list>> = failwith ""
      set.Required(setter, mapSetError, parsedArg) |> ignore
    true


module Required_resultSingleSetter_simpleArg =

  [<Fact>]
  let ``can resolve overload`` () =
    let __ () =
      let setter : Arg -> Entity -> Result<Entity, SetError> = failwith ""
      let parsedArg : Arg = failwith ""
      set.Required(setter, mapSetError, parsedArg) |> ignore
    true


module Required_resultSingleSetter_simpleArg_async =

  [<Fact>]
  let ``can resolve overload`` () =
    let __ () =
      let setter : Arg -> Entity -> Result<Entity, SetError> = failwith ""
      let parsedArg : Async<Arg> = failwith ""
      set.Required(setter, mapSetError, parsedArg) |> ignore
    true
