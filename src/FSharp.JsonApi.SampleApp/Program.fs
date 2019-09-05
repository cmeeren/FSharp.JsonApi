namespace FSharp.JsonApi.SampleApp

open System
open Microsoft.AspNetCore
open Microsoft.AspNetCore.Hosting
open Serilog

module Program =

  [<EntryPoint>]
  let main args =
    try
      // Force initialization of the JsonApiContext to catch errors at startup
      ignore Resources.jsonApiCtx

      WebHost
        .CreateDefaultBuilder(args)
        .UseSerilog(Action<_,_> Setup.setupLogger)
        .UseStartup<Startup>()
        .Build()
        .Run()
      0

    finally Log.CloseAndFlush()
