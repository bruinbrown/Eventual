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

[<Sealed>]
type TwoPhaseSet<'a>(adds:IImmutableSet<'a>, removes:IImmutableSet<'a>) =
    inherit AbstractCvRDT<TwoPhaseSet<'a>, IImmutableSet<'a> * IImmutableSet<'a>>()
    
    static member Empty = TwoPhaseSet(ImmutableHashSet<_>.Empty, ImmutableHashSet<_>.Empty)

    override this.State = (adds, removes)

    member this.Adds = adds

    member this.Removes = removes

    member this.Add(value:'a) =
        if (adds.Contains(value)) && (removes.Contains(value)) then failwith "Unable to add to set. It's already been removed"
        TwoPhaseSet(adds.Add(value), removes)

    member this.Remove(value:'a) =
        if not (adds.Contains(value)) then failwith "Element to be tombstoned was not in the addition set"
        else TwoPhaseSet(adds, removes.Add(value))

    override this.Merge(that) =
        let adds' = adds.Union(that.Adds)
        let removes' = removes.Union(that.Removes)
        TwoPhaseSet(adds', removes')
