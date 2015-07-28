namespace Eventual

/// Non-Generic interface to allow for collections of disparate types
type CvRDT =
    abstract member Merge : obj -> obj

/// Concrete interface to allow for generic usage
type CvRDT<'a> =
    inherit CvRDT
    abstract member Merge : 'a -> 'a

/// Base type from which almost all CRDTs will inherit from
[<AbstractClass>]
//type 'a is the concrete CRDT type which implements the abstract class and allows us to do things like generic merging
//type 'b is the type which contains the state stored within the CRDT, e.g. Flag is a bool, GCounter is an immutable dictionary
//'b must implement this abstract class to allow us to retrieve the state from itself in the equality override
//'b must implement equality to allow us to compare the state stored within the actor
type AbstractCvRDT<'a, 'b when 'a :> AbstractCvRDT<'a, 'b> and 'b : equality> () =
    
    abstract member Merge : 'a -> 'a

    abstract member State : 'b

    override this.GetHashCode() =
        this.State.GetHashCode()

    override this.Equals(other:obj) =
        if other.GetType() = typeof<'a> then (other :?> 'a).State = this.State
        else false

    interface CvRDT<'a> with
        member this.Merge(that:'a) =
            this.Merge(that)

        member this.Merge(that:obj) =
            match that with
            | :? 'a as that -> this.Merge(that) :> obj
            | _ -> invalidArg "that" "Unable to merge CRDTs of different types"