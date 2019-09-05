namespace FSharp.JsonApi.SampleApp

open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.DependencyInjection
open Serilog
open Serilog.Events
open Giraffe

open HttpHandlers
open Routes


module Setup =

  let setupLogger hostingContext (loggerConfiguration: LoggerConfiguration) =
    loggerConfiguration
      .MinimumLevel.Override("Microsoft", LogEventLevel.Information)
      .MinimumLevel.Override("Giraffe", LogEventLevel.Information)
      .Enrich.FromLogContext()
      .WriteTo.Console()
      |> ignore


type Startup() =

  member this.ConfigureServices(services: IServiceCollection) : unit =
    services.AddGiraffe() |> ignore

  member this.Configure(app: IApplicationBuilder, env: IHostingEnvironment) : unit =
    app
      .UseGiraffeErrorHandler(fun ex _ -> unhandledExceptionHandler ex)
      .UseGiraffe(mainHandler)
