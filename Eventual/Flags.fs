namespace Eventual.Flags

open Eventual

[<Sealed>]
type Flag(state:bool) =
    inherit AbstractCvRDT<Flag, bool>()

    static member Unset = Flag(false)

    override this.State = state

    override this.Merge(that:Flag) =
        if that.State then that
        else this
