FSharp.JsonApi
==============

Use F# to easily create and consume flexible, strongly typed web APIs following the [JSON-API specification](https://jsonapi.org/).

Core features:

* Full support for sparse fieldsets and included resources
* Support for loading included resources asynchronously on-demand, in parallel
* Uses [FSharp.JsonSkippable](https://github.com/cmeeren/FSharp.JsonSkippable) for strong typing of whether JSON properties are included or excluded

The focus is on server implementations, but it may also be useful when implementing clients (please get in touch!).

Table of contents
-----------------

TODO

Installation
------------

FSharp.JsonApi consists of three NuGet packages:

* **FSharp.JsonApi** contains all the core stuff: JSON-API document models for serialization/deserialization, resource builders, parsing and validation of query parameters and documents, etc.
* **FSharp.JsonApi.AspNetCore** contains lots of useful helpers and additional overloads for parsing and validating requests using ASP.NET Core’s `HttpContext`
* **FSharp.JsonApi.Giraffe** – a few simple helpers that may be useful if using [Giraffe](https://github.com/giraffe-fsharp/Giraffe/)

If using Giraffe, just install FSharp.JsonApi.Giraffe and you’ll get the other two automatically.

If using ASP.NET Core, but not Giraffe, install FSharp.JsonApi.AspNetCore and you’ll also get the core library automatically.

If you don’t use ASP.NET Core, you can easily use the core library to build your own abstractions.

Contributing
------------

Contributions and ideas are welcome! Please see [Contributing.md](https://github.com/cmeeren/FSharp.JsonApi/blob/master/.github/CONTRIBUTING.md) for details.

Quick start
-----------

TODO

Documentation
-------------

TODO

Release notes
-------------

TODO

