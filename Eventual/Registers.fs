namespace Eventual.Registers

open Eventual

[<AutoOpen>]
module SystemExtensions = 
    open System

    type DateTime with
        member this.TotalMilliseconds = 
            let ts = this - DateTime(1970, 1, 1)
            ts.TotalMilliseconds |> int64

type IClock = 
    abstract NextTimestamp : int64 -> int64

type ClockValue(value) = 
    interface IClock with
        member this.NextTimestamp(_) = value

[<AutoOpen>]
module private DefaultClocks = 
    open System

    let DefaultClock = 
        { new IClock with
              member x.NextTimestamp(currentTimestamp : int64) : int64 = 
                  System.Math.Max(DateTime.UtcNow.TotalMilliseconds, currentTimestamp) }
    
    let ReverseClock = 
        { new IClock with
              member x.NextTimestamp(currentTimestamp : int64) = 
                  Math.Min(-DateTime.UtcNow.TotalMilliseconds, currentTimestamp - 1L) }

type LWWRegister<'a when 'a : equality>(node : string, value : 'a, timestamp : int64) = 
    inherit AbstractCvRDT<LWWRegister<'a>, 'a * string * int64>()
    member this.Value = value
    member this.Timestamp = timestamp
    member this.UpdatedBy = node

    member this.WithValue(node : string, value : 'a) =
        this.WithValue(node, value, DefaultClock)
    
    member this.WithValue(node : string, value : 'a, clock : IClock) = 
        LWWRegister(node, value, clock.NextTimestamp(timestamp))
    
    override this.State = (value, node, timestamp)
    
    override this.Merge(that) = 
        if that.Timestamp > this.Timestamp then that
        elif that.Timestamp < this.Timestamp then this
        elif that.UpdatedBy.ToString() < this.UpdatedBy.ToString() then that
        else this
