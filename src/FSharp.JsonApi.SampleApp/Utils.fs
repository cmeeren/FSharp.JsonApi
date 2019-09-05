[<AutoOpen>]
module Utils

open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns


// Since F# doesn't have any support for nameof without instances yet, let's
// define a couple of helpers as a replacement.


/// Returns the member name referenced in the quotation. For example,
/// nameof <@ person.Name @> will return "Name". Can be used with
/// the "any" helper if no instance is available.
let nameof expression =
  match expression with
  | Let(_, _, Lambdas(_, Call(_, mi, _))) -> mi.Name
  | PropertyGet(_, mi, _) -> mi.Name
  | Lambdas(_, Call(_, mi, _)) -> mi.Name
  | _ -> failwith "Unsupported expression"
  
  
/// A reflection helper for specifying a property in a quotation without
/// needing an instance of the type, e.g. <@ any<Person>.Name @>.
/// Useful together with nameof. Throws if actually invoked.
let any<'a> : 'a = failwith "Reflection helper only, do not invoke"
