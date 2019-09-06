namespace FSharp.JsonApi.SampleApp

open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.DependencyInjection
open Serilog
open Serilog.Events
open Giraffe

open HttpHandlers
open Routes


[<AutoOpen>]
module ApplicationBuilderExtensions =

  open Microsoft.AspNetCore.StaticFiles

  type IApplicationBuilder with
    /// Enables static file serving using the specified mappings between subpath
    /// suffixes and content types.
    member this.UseStaticFiles(pathSuffixContentTypes: #seq<string * string>) =
      this.UseStaticFiles(
        StaticFileOptions(
          ContentTypeProvider =
            { new IContentTypeProvider with
                member __.TryGetContentType(subpath, contentType) =
                  let mimeType =
                    pathSuffixContentTypes
                    |> Seq.tryPick (fun (suffix, mimeType) ->
                        if subpath.EndsWith suffix then Some mimeType else None
                    )
                  match mimeType with
                  | Some mt ->
                      contentType <- mt
                      true
                  | None -> false
            }
        )
      )


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
      .UseDefaultFiles()
      .UseStaticFiles([
        (".html", "text/html")
        (".png", "image/png")
        (".yaml", "application/x-yaml")
      ])
      .UseGiraffeErrorHandler(fun ex _ -> unhandledExceptionHandler ex)
      .UseGiraffe(mainHandler)
