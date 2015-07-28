namespace Eventual.Sets

open System.Collections.Immutable
open Eventual

[<Sealed>]
type GSet<'a>(elements:IImmutableSet<'a>) =
    inherit AbstractCvRDT<GSet<'a>, IImmutableSet<'a>>()

    static member Empty = GSet(ImmutableHashSet<_>.Empty)

    override x.Merge(that) =
        let elements = x.State.Union(that.State)
        GSet(elements)

    override x.State = elements

    member x.Add(value:'a) =
        GSet(elements.Add(value))


