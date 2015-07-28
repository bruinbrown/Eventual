namespace Eventual.Counters

open System.Collections.Immutable
open Eventual

[<AutoOpen>]
module ImmutableExtensions =
    type System.Collections.Immutable.IImmutableDictionary<'TKey, 'TValue> with
        member this.ApplyUpdate(key:'TKey, updater:'TValue -> 'TValue, ?missingDefault) =
            match this.TryGetValue(key) with
            | true, x -> let v = updater x
                         this.SetItem(key, v)
            | false, _ -> let missingDefault = defaultArg missingDefault <| Unchecked.defaultof<'TValue>
                          let v = updater missingDefault
                          this.SetItem(key, v)

[<Sealed>]
type GCounter(state:IImmutableDictionary<string, int64>) =
    inherit AbstractCvRDT<GCounter, IImmutableDictionary<string, int64>>()

    let value = state.Values |> Seq.sum

    static member Zero = GCounter(ImmutableDictionary<_,_>.Empty)

    member this.Value = value

    override this.State = state
    
    member this.Increment(identifier, ?delta) =
        let delta = defaultArg delta 1L
        if delta = 0L then this
        else
            GCounter(state.ApplyUpdate(identifier, fun x -> x + delta))

    override this.Merge(that) =
        let state = this.State
                    |> Seq.fold (fun (s:IImmutableDictionary<string, int64>) t -> let v = s.GetValueOrDefault(t.Key, 0L)
                                                                                  if t.Value > v then s.SetItem(t.Key, t.Value)
                                                                                  else s) that.State
        GCounter(state)

[<Sealed>]
type PNCounter(increments:GCounter, decrements:GCounter) as this =
    inherit AbstractCvRDT<PNCounter, GCounter * GCounter>()

    let value = increments.Value - decrements.Value

    let change identifier delta =
        if delta < 0L then
            let decrements = decrements.Increment(identifier, delta)
            PNCounter(increments, decrements)
        elif delta > 0L then
            let increments = increments.Increment(identifier, delta)
            PNCounter(increments, decrements)
        else
            this

    static member Zero = PNCounter(GCounter.Zero, GCounter.Zero)

    member this.Value = value

    member internal this.Increments = increments

    member internal this.Decrements = decrements

    member this.Increment(identifier, ?delta) =
        let delta = defaultArg delta 1L
        change identifier delta

    member this.Decrement(identifier, ?delta) =
        let delta = defaultArg delta 1L
        change identifier delta

    override this.State = (increments, decrements)

    override this.Merge(that) =
        let increments = this.Increments.Merge(that.Increments)
        let decrements = this.Decrements.Merge(that.Decrements)
        PNCounter(increments, decrements)