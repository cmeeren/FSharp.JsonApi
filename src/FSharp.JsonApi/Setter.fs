namespace FSharp.JsonApi


/// Provides helper methods to chain and lift normal "immutable setter"
/// functions (either 'a -> 'b -> 'b or 'a -> 'b -> Result<'b, 'err>) to accept
/// parsed, possibly optional arguments, combining any errors.
type Setter<'apiError>
    ( mapRequestDocumentError: RequestDocumentError -> 'apiError,
      mapQueryError: QueryError -> 'apiError
    ) =

  /// Lifts a normal "immutable setter" to accept a parsed optional argument
  /// result and an entity result. Passes through the entity if the argument is
  /// None. Combines any errors.
  member __.Optional
      ( setter: 'arg -> 'entity -> 'entity,
        parsedArg: Result<'arg option, RequestDocumentError list>
      ) : Result<'entity, 'apiError list> -> Result<'entity, 'apiError list> =
    fun entityResult ->
      match entityResult, parsedArg with
      | Error errs, Ok _ -> Error errs
      | Ok _, Error errs -> errs |> List.map mapRequestDocumentError |> Error
      | Error errs1, Error errs2 -> errs1 @ (errs2 |> List.map mapRequestDocumentError) |> Error
      | Ok x, Ok None -> Ok x
      | Ok x, Ok (Some arg) -> setter arg x |> Ok

  /// Lifts a normal "immutable setter" to accept a parsed optional argument
  /// result and an entity result. Passes through the entity if the argument is
  /// None. Combines any errors.
  member this.Optional
      ( setter: 'arg -> 'entity -> 'entity,
        parsedArg: Async<Result<'arg option, RequestDocumentError list>>
      ) : Async<Result<'entity, 'apiError list>> -> Async<Result<'entity, 'apiError list>> =
    fun entityResult ->
      async {
        let! res = entityResult
        let! arg = parsedArg
        return this.Optional(setter, arg) res
      }

  /// Lifts a normal "immutable setter" to accept a parsed optional argument
  /// result and an entity result. Passes through the entity if the argument is
  /// None. Combines any errors.
  member __.Optional
      ( setter: 'arg -> 'entity -> 'entity,
        parsedArg: Result<'arg option, QueryError list>
      ) : Result<'entity, 'apiError list> -> Result<'entity, 'apiError list> =
    fun entityResult ->
      match entityResult, parsedArg with
      | Error errs, Ok _ -> Error errs
      | Ok _, Error errs -> errs |> List.map mapQueryError |> Error
      | Error errs1, Error errs2 -> errs1 @ (errs2 |> List.map mapQueryError) |> Error
      | Ok x, Ok None -> Ok x
      | Ok x, Ok (Some arg) -> setter arg x |> Ok

  /// Lifts a normal "immutable setter" to accept a parsed optional argument
  /// result and an entity result. Passes through the entity if the argument is
  /// None. Combines any errors.
  member this.Optional
      ( setter: 'arg -> 'entity -> 'entity,
        parsedArg: Async<Result<'arg option, QueryError list>>
      ) : Async<Result<'entity, 'apiError list>> -> Async<Result<'entity, 'apiError list>> =
    fun entityResult ->
      async {
        let! res = entityResult
        let! arg = parsedArg
        return this.Optional(setter, arg) res
      }

  /// Lifts a normal "immutable setter" to accept an optional argument and an
  /// entity result. Passes through the entity if the argument is None. Combines
  /// any errors.
  member __.Optional
      ( setter: 'arg -> 'entity -> 'entity,
        arg: 'arg option
      ) : Result<'entity, 'apiError list> -> Result<'entity, 'apiError list> =
    fun entityResult ->
      match entityResult, arg with
      | Error errs, _ -> Error errs
      | Ok x, None -> Ok x
      | Ok x, Some arg -> setter arg x |> Ok

  /// Lifts a normal "immutable setter" to accept an optional argument and an
  /// entity result. Passes through the entity if the argument is None. Combines
  /// any errors.
  member this.Optional
      ( setter: 'arg -> 'entity -> 'entity,
        arg: Async<'arg option>
      ) : Async<Result<'entity, 'apiError list>> -> Async<Result<'entity, 'apiError list>> =
    fun entityResult ->
      async {
        let! res = entityResult
        let! arg = arg
        return this.Optional(setter, arg) res
      }

  /// Lifts a normal result-returning "immutable setter" to accept a parsed
  /// optional argument result and an entity result. Passes through the entity
  /// if the argument is None. Combines any errors.
  member __.Optional
      ( setter: 'arg -> 'entity -> Result<'entity, 'setError list>,
        mapSetError: 'setError -> 'apiError,
        parsedArg: Result<'arg option, RequestDocumentError list>
      ) : Result<'entity, 'apiError list> -> Result<'entity, 'apiError list> =
    fun entityResult ->
      match entityResult, parsedArg with
      | Error errs, Ok _ -> Error errs
      | Ok _, Error errs -> errs |> List.map mapRequestDocumentError |> Error
      | Error errs1, Error errs2 -> errs1 @ (errs2 |> List.map mapRequestDocumentError) |> Error
      | Ok x, Ok None -> Ok x
      | Ok x, Ok (Some arg) -> setter arg x |> Result.mapError (List.map mapSetError)

  /// Lifts a normal result-returning "immutable setter" to accept a parsed
  /// optional argument result and an entity result. Passes through the entity
  /// if the argument is None. Combines any errors.
  member this.Optional
      ( setter: 'arg -> 'entity -> Result<'entity, 'setError list>,
        mapSetError: 'setError -> 'apiError,
        parsedArg: Async<Result<'arg option, RequestDocumentError list>>
      ) : Async<Result<'entity, 'apiError list>> -> Async<Result<'entity, 'apiError list>> =
    fun entityResult ->
      async {
        let! res = entityResult
        let! arg = parsedArg
        return this.Optional(setter, mapSetError, arg) res
      }


  /// Lifts a normal result-returning "immutable setter" to accept a parsed
  /// optional argument result and an entity result. Passes through the entity
  /// if the argument is None. Combines any errors.
  member __.Optional
      ( setter: 'arg -> 'entity -> Result<'entity, 'setError list>,
        mapSetError: 'setError -> 'apiError,
        parsedArg: Result<'arg option, QueryError list>
      ) : Result<'entity, 'apiError list> -> Result<'entity, 'apiError list> =
    fun entityResult ->
      match entityResult, parsedArg with
      | Error errs, Ok _ -> Error errs
      | Ok _, Error errs -> errs |> List.map mapQueryError |> Error
      | Error errs1, Error errs2 -> errs1 @ (errs2 |> List.map mapQueryError) |> Error
      | Ok x, Ok None -> Ok x
      | Ok x, Ok (Some arg) -> setter arg x |> Result.mapError (List.map mapSetError)

  /// Lifts a normal result-returning "immutable setter" to accept a parsed
  /// optional argument result and an entity result. Passes through the entity
  /// if the argument is None. Combines any errors.
  member this.Optional
      ( setter: 'arg -> 'entity -> Result<'entity, 'setError list>,
        mapSetError: 'setError -> 'apiError,
        parsedArg: Async<Result<'arg option, QueryError list>>
      ) : Async<Result<'entity, 'apiError list>> -> Async<Result<'entity, 'apiError list>> =
    fun entityResult ->
      async {
        let! res = entityResult
        let! arg = parsedArg
        return this.Optional(setter, mapSetError, arg) res
      }

  /// Lifts a normal result-returning "immutable setter" to accept an optional
  /// argument and an entity result. Passes through the entity if the argument
  /// is None. Combines any errors.
  member __.Optional
      ( setter: 'arg -> 'entity -> Result<'entity, 'setError list>,
        mapSetError: 'setError -> 'apiError,
        arg: 'arg option
      ) : Result<'entity, 'apiError list> -> Result<'entity, 'apiError list> =
    fun entityResult ->
      match entityResult, arg with
      | Error errs, _ -> Error errs
      | Ok x, None -> Ok x
      | Ok x, Some arg -> setter arg x |> Result.mapError (List.map mapSetError)

  /// Lifts a normal result-returning "immutable setter" to accept an optional
  /// argument and an entity result. Passes through the entity if the argument
  /// is None. Combines any errors.
  member this.Optional
      ( setter: 'arg -> 'entity -> Result<'entity, 'setError list>,
        mapSetError: 'setError -> 'apiError,
        arg: Async<'arg option>
      ) : Async<Result<'entity, 'apiError list>> -> Async<Result<'entity, 'apiError list>> =
    fun entityResult ->
      async {
        let! res = entityResult
        let! arg = arg
        return this.Optional(setter, mapSetError, arg) res
      }

  /// Lifts a normal result-returning "immutable setter" to accept a parsed
  /// optional argument result and an entity result. Passes through the entity
  /// if the argument is None. Combines any errors.
  member this.Optional
      ( setter: 'arg -> 'entity -> Result<'entity, 'setError>,
        mapSetError: 'setError -> 'apiError,
        parsedArg: Result<'arg option, RequestDocumentError list>
      ) : Result<'entity, 'apiError list> -> Result<'entity, 'apiError list> =
    this.Optional((fun a e -> setter a e |> Result.mapError List.singleton), mapSetError, parsedArg)

  /// Lifts a normal result-returning "immutable setter" to accept a parsed
  /// optional argument result and an entity result. Passes through the entity
  /// if the argument is None. Combines any errors.
  member this.Optional
      ( setter: 'arg -> 'entity -> Result<'entity, 'setError>,
        mapSetError: 'setError -> 'apiError,
        parsedArg: Async<Result<'arg option, RequestDocumentError list>>
      ) : Async<Result<'entity, 'apiError list>> -> Async<Result<'entity, 'apiError list>> =
    this.Optional((fun a e -> setter a e |> Result.mapError List.singleton), mapSetError, parsedArg)

  /// Lifts a normal result-returning "immutable setter" to accept a parsed
  /// optional argument result and an entity result. Passes through the entity
  /// if the argument is None. Combines any errors.
  member this.Optional
      ( setter: 'arg -> 'entity -> Result<'entity, 'setError>,
        mapSetError: 'setError -> 'apiError,
        parsedArg: Result<'arg option, QueryError list>
      ) : Result<'entity, 'apiError list> -> Result<'entity, 'apiError list> =
    this.Optional((fun a e -> setter a e |> Result.mapError List.singleton), mapSetError, parsedArg)

  /// Lifts a normal result-returning "immutable setter" to accept a parsed
  /// optional argument result and an entity result. Passes through the entity
  /// if the argument is None. Combines any errors.
  member this.Optional
      ( setter: 'arg -> 'entity -> Result<'entity, 'setError>,
        mapSetError: 'setError -> 'apiError,
        parsedArg: Async<Result<'arg option, QueryError list>>
      ) : Async<Result<'entity, 'apiError list>> -> Async<Result<'entity, 'apiError list>> =
    this.Optional((fun a e -> setter a e |> Result.mapError List.singleton), mapSetError, parsedArg)

  /// Lifts a normal result-returning "immutable setter" to accept an optional
  /// argument and an entity result. Passes through the entity if the argument
  /// is None. Combines any errors.
  member this.Optional
      ( setter: 'arg -> 'entity -> Result<'entity, 'setError>,
        mapSetError: 'setError -> 'apiError,
        arg: 'arg option
      ) : Result<'entity, 'apiError list> -> Result<'entity, 'apiError list> =
    this.Optional((fun a e -> setter a e |> Result.mapError List.singleton), mapSetError, arg)

  /// Lifts a normal result-returning "immutable setter" to accept an optional
  /// argument and an entity result. Passes through the entity if the argument
  /// is None. Combines any errors.
  member this.Optional
      ( setter: 'arg -> 'entity -> Result<'entity, 'setError>,
        mapSetError: 'setError -> 'apiError,
        arg: Async<'arg option>
      ) : Async<Result<'entity, 'apiError list>> -> Async<Result<'entity, 'apiError list>> =
    this.Optional((fun a e -> setter a e |> Result.mapError List.singleton), mapSetError, arg)

  /// Lifts a normal "immutable setter" to accept a parsed argument result and
  /// an entity result. Combines any errors.
  member this.Required
      ( setter: 'arg -> 'entity -> 'entity,
        parsedArg: Result<'arg, RequestDocumentError list>
      ) : Result<'entity, 'apiError list> -> Result<'entity, 'apiError list> =
    this.Optional(setter, parsedArg |> Result.map Some)

  /// Lifts a normal "immutable setter" to accept a parsed argument result and
  /// an entity result. Combines any errors.
  member this.Required
      ( setter: 'arg -> 'entity -> 'entity,
        parsedArg: Async<Result<'arg, RequestDocumentError list>>
      ) : Async<Result<'entity, 'apiError list>> -> Async<Result<'entity, 'apiError list>> =
    this.Optional(setter, parsedArg |> AsyncResult.map Some)

  /// Lifts a normal "immutable setter" to accept a parsed argument result and
  /// an entity result. Combines any errors.
  member this.Required
      ( setter: 'arg -> 'entity -> 'entity,
        parsedArg: Result<'arg, QueryError list>
      ) : Result<'entity, 'apiError list> -> Result<'entity, 'apiError list> =
    this.Optional(setter, parsedArg |> Result.map Some)

  /// Lifts a normal "immutable setter" to accept a parsed argument result and
  /// an entity result. Combines any errors.
  member this.Required
      ( setter: 'arg -> 'entity -> 'entity,
        parsedArg: Async<Result<'arg, QueryError list>>
      ) : Async<Result<'entity, 'apiError list>> -> Async<Result<'entity, 'apiError list>> =
    this.Optional(setter, parsedArg |> AsyncResult.map Some)

  /// Lifts a normal "immutable setter" to accept an entity result. Combines any
  /// errors.
  member this.Required
      ( setter: 'arg -> 'entity -> 'entity,
        arg: 'arg
      ) : Result<'entity, 'apiError list> -> Result<'entity, 'apiError list> =
    this.Optional(setter, Some arg)

  /// Lifts a normal "immutable setter" to accept an entity result. Combines any
  /// errors.
  member this.Required
      ( setter: 'arg -> 'entity -> 'entity,
        arg: Async<'arg>
      ) : Async<Result<'entity, 'apiError list>> -> Async<Result<'entity, 'apiError list>> =
    this.Optional(setter, arg |> Async.map Some)

  /// Lifts a normal result-returning "immutable setter" to accept a parsed
  /// argument result and an entity result. Combines any errors.
  member this.Required
      ( setter: 'arg -> 'entity -> Result<'entity, 'setError list>,
        mapSetError: 'setError -> 'apiError,
        parsedArg: Result<'arg, RequestDocumentError list>
      ) : Result<'entity, 'apiError list> -> Result<'entity, 'apiError list> =
    this.Optional(setter, mapSetError, parsedArg |> Result.map Some)

  /// Lifts a normal result-returning "immutable setter" to accept a parsed
  /// argument result and an entity result. Combines any errors.
  member this.Required
      ( setter: 'arg -> 'entity -> Result<'entity, 'setError list>,
        mapSetError: 'setError -> 'apiError,
        parsedArg: Async<Result<'arg, RequestDocumentError list>>
      ) : Async<Result<'entity, 'apiError list>> -> Async<Result<'entity, 'apiError list>> =
    this.Optional(setter, mapSetError, parsedArg |> AsyncResult.map Some)

  /// Lifts a normal result-returning "immutable setter" to accept a parsed
  /// argument result and an entity result. Combines any errors.
  member this.Required
      ( setter: 'arg -> 'entity -> Result<'entity, 'setError list>,
        mapSetError: 'setError -> 'apiError,
        parsedArg: Result<'arg, QueryError list>
      ) : Result<'entity, 'apiError list> -> Result<'entity, 'apiError list> =
    this.Optional(setter, mapSetError, parsedArg |> Result.map Some)

  /// Lifts a normal result-returning "immutable setter" to accept a parsed
  /// argument result and an entity result. Combines any errors.
  member this.Required
      ( setter: 'arg -> 'entity -> Result<'entity, 'setError list>,
        mapSetError: 'setError -> 'apiError,
        parsedArg: Async<Result<'arg, QueryError list>>
      ) : Async<Result<'entity, 'apiError list>> -> Async<Result<'entity, 'apiError list>> =
    this.Optional(setter, mapSetError, parsedArg |> AsyncResult.map Some)

  /// Lifts a normal result-returning "immutable setter" to accept an entity
  /// result. Combines any errors.
  member this.Required
      ( setter: 'arg -> 'entity -> Result<'entity, 'setError list>,
        mapSetError: 'setError -> 'apiError,
        arg: 'arg
      ) : Result<'entity, 'apiError list> -> Result<'entity, 'apiError list> =
    this.Optional(setter, mapSetError, Some arg)

  /// Lifts a normal result-returning "immutable setter" to accept an entity
  /// result. Combines any errors.
  member this.Required
      ( setter: 'arg -> 'entity -> Result<'entity, 'setError list>,
        mapSetError: 'setError -> 'apiError,
        arg: Async<'arg>
      ) : Async<Result<'entity, 'apiError list>> -> Async<Result<'entity, 'apiError list>> =
    this.Optional(setter, mapSetError, arg |> Async.map Some)

  /// Lifts a normal result-returning "immutable setter" to accept a parsed
  /// argument result and an entity result. Combines any errors.
  member this.Required
      ( setter: 'arg -> 'entity -> Result<'entity, 'setError>,
        mapSetError: 'setError -> 'apiError,
        parsedArg: Result<'arg, RequestDocumentError list>
      ) : Result<'entity, 'apiError list> -> Result<'entity, 'apiError list> =
    this.Optional((fun a e -> setter a e |> Result.mapError List.singleton), mapSetError, parsedArg |> Result.map Some)

  /// Lifts a normal result-returning "immutable setter" to accept a parsed
  /// argument result and an entity result. Combines any errors.
  member this.Required
      ( setter: 'arg -> 'entity -> Result<'entity, 'setError>,
        mapSetError: 'setError -> 'apiError,
        parsedArg: Async<Result<'arg, RequestDocumentError list>>
      ) : Async<Result<'entity, 'apiError list>> -> Async<Result<'entity, 'apiError list>> =
    this.Optional((fun a e -> setter a e |> Result.mapError List.singleton), mapSetError, parsedArg |> AsyncResult.map Some)

  /// Lifts a normal result-returning "immutable setter" to accept a parsed
  /// argument result and an entity result. Combines any errors.
  member this.Required
      ( setter: 'arg -> 'entity -> Result<'entity, 'setError>,
        mapSetError: 'setError -> 'apiError,
        parsedArg: Result<'arg, QueryError list>
      ) : Result<'entity, 'apiError list> -> Result<'entity, 'apiError list> =
    this.Optional((fun a e -> setter a e |> Result.mapError List.singleton), mapSetError, parsedArg |> Result.map Some)

  /// Lifts a normal result-returning "immutable setter" to accept a parsed
  /// argument result and an entity result. Combines any errors.
  member this.Required
      ( setter: 'arg -> 'entity -> Result<'entity, 'setError>,
        mapSetError: 'setError -> 'apiError,
        parsedArg: Async<Result<'arg, QueryError list>>
      ) : Async<Result<'entity, 'apiError list>> -> Async<Result<'entity, 'apiError list>> =
    this.Optional((fun a e -> setter a e |> Result.mapError List.singleton), mapSetError, parsedArg |> AsyncResult.map Some)

  /// Lifts a normal result-returning "immutable setter" to accept an entity
  /// result. Combines any errors.
  member this.Required
      ( setter: 'arg -> 'entity -> Result<'entity, 'setError>,
        mapSetError: 'setError -> 'apiError,
        arg: 'arg
      ) : Result<'entity, 'apiError list> -> Result<'entity, 'apiError list> =
    this.Optional((fun a e -> setter a e |> Result.mapError List.singleton), mapSetError, Some arg)

  /// Lifts a normal result-returning "immutable setter" to accept an entity
  /// result. Combines any errors.
  member this.Required
      ( setter: 'arg -> 'entity -> Result<'entity, 'setError>,
        mapSetError: 'setError -> 'apiError,
        arg: Async<'arg>
      ) : Async<Result<'entity, 'apiError list>> -> Async<Result<'entity, 'apiError list>> =
    this.Optional((fun a e -> setter a e |> Result.mapError List.singleton), mapSetError, arg |> Async.map Some)
