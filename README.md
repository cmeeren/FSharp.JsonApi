FSharp.JsonApi
==============

Use F# to create and consume flexible, strongly typed web APIs following the [JSON-API specification](https://jsonapi.org/).

Table of contents
-----------------

TODO

Installation
------------

FSharp.JsonApi consists of three NuGet packages:

* **FSharp.JsonApi** contains all the core stuff: Document model for serialization/deserialization, resource builders, etc.
* **FSharp.JsonApi.AspNetCore** contains lots of really useful helpers for parsing and validating requests using ASP.NET Core’s `HttpContext`
* **FSharp.JsonApi.Giraffe** – a few helpers that may be useful if using [Giraffe](https://github.com/giraffe-fsharp/Giraffe/)

If using Giraffe, just install FSharp.JsonApi.Giraffe and you’ll get the other two automatically.

If using ASP.NET Core, but not Giraffe, install FSharp.JsonApi.AspNetCore and you’ll also get the core library automatically.

If you don’t use ASP.NET Core, you can use the core library to build your own abstractions. (Please get in touch – some logic in FSharp.JsonApi.AspNetCore may be refactored to FSharp.JsonApi, but I don’t want to do that before I know what library users actually need.)

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

