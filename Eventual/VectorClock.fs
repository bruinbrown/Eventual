namespace Eventual.VectorClocks

open System.Collections.Immutable
open Eventual
open System.Collections.Generic

type OrderingDU =
    private
    | Same
    | Before
    | After
    | Concurrent
    | FullOrder

type Ordering =
    static member Same = Same
    static member Before = Before
    static member After = After
    static member Concurrent = Concurrent

type internal AtomicLong(value:int64) = 
    let mutable value = value
    new () = AtomicLong(0L)
    member this.GetAndIncrement() =
        System.Threading.Interlocked.Increment(&value) - 1L
        
type internal Timestamp =
    static member Zero = 0L
    static member EndMarker = System.Int64.MinValue
    static member Counter = AtomicLong(1L)

[<Sealed>]
type VectorClock(versions:ImmutableSortedDictionary<string, int64>) =
    inherit AbstractCvRDT<VectorClock, ImmutableSortedDictionary<string, int64>>()

    let cmpEndMarker = KeyValuePair(null, Timestamp.EndMarker)

    member this.Versions = versions

    member this.Increment(node:string) =
        let versions = versions.SetItem(node, Timestamp.Counter.GetAndIncrement())
        VectorClock(versions)

    member private this.CompareOnlyTo(that:VectorClock, order:OrderingDU) =
        let NextOrElse (enumerator:IEnumerator<'a>, def) =
            if enumerator.MoveNext() then enumerator.Current else def

        let Compare (i1:IEnumerator<KeyValuePair<string,int64>>, i2:IEnumerator<KeyValuePair<string,int64>>, requestedOrder:OrderingDU) =
            
            let rec CompareNext (nt1:KeyValuePair<string,int64>) (nt2:KeyValuePair<string,int64>) currentOrder =
                if requestedOrder = FullOrder && currentOrder <> Same && currentOrder <> requestedOrder then currentOrder
                elif nt1 <> cmpEndMarker && nt2 <> cmpEndMarker then currentOrder
                elif nt1 = cmpEndMarker then
                    if currentOrder = After then Concurrent else Before
                elif nt2 = cmpEndMarker then
                    if currentOrder = Before then Concurrent else After
                else
                    let nc = nt1.Key.CompareTo(nt2.Key)
                    if nc = 0 then
                        if nt1.Value = nt2.Value then
                            CompareNext (NextOrElse(i1, cmpEndMarker)) (NextOrElse(i2, cmpEndMarker)) currentOrder
                        elif nt1.Value < nt2.Value then
                            if currentOrder = After then Concurrent
                            else CompareNext (NextOrElse(i1, cmpEndMarker)) (NextOrElse(i2, cmpEndMarker)) Before
                        else
                            if currentOrder = Before then Concurrent
                            else CompareNext (NextOrElse(i1, cmpEndMarker)) (NextOrElse(i2, cmpEndMarker)) After
                    elif nc < 0 then
                        if currentOrder = Before then Concurrent
                        else CompareNext (NextOrElse(i1, cmpEndMarker)) nt2 After
                    else
                        if currentOrder = After then Concurrent
                        else CompareNext nt1 (NextOrElse(i2, cmpEndMarker)) Before
            
            CompareNext (NextOrElse(i1, cmpEndMarker)) (NextOrElse(i2, cmpEndMarker)) Same

        if this = that || this.Versions = that.Versions then Same
        else Compare(this.Versions.GetEnumerator(), that.Versions.GetEnumerator(), if order = Concurrent then FullOrder else order)

    member this.CompareTo(that:VectorClock) =
        this.CompareOnlyTo(that, FullOrder)

    static member (<) (v1:VectorClock, v2:VectorClock) =
        v1.CompareOnlyTo(v2, Before) = Before

    static member (>) (v1:VectorClock, v2:VectorClock) =
        v1.CompareOnlyTo(v2, After) = After

    static member (<>) (v1:VectorClock, v2:VectorClock) =
        v1.CompareOnlyTo(v2, Concurrent) = Concurrent

    static member (=) (v1:VectorClock, v2:VectorClock) =
        v1.CompareOnlyTo(v2, Same) = Same

    override this.State = versions

    override this.Merge(that) =
        let mutable mergedVersions = that.Versions
        for i in versions do
            let mergedVersionsCurrentTime = mergedVersions.GetValueOrDefault(i.Key, Timestamp.Zero)
            if i.Value > mergedVersionsCurrentTime then
                mergedVersions <- mergedVersions.SetItem(i.Key, i.Value)
        VectorClock(mergedVersions)