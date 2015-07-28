namespace Eventual.Tests

open NUnit.Framework
open Eventual.Flags

[<TestFixture>]
module FlagTests =

    [<Test>]
    let ``Merging two false flags gives false flag`` () =
        let flag1 = Flag(false)
        let flag2 = Flag(false)
        let expected = Flag(false)
        let actual = flag1.Merge(flag2)
        Assert.AreEqual(expected, actual)

    [<Test>]
    let ``Merging two different flags gives true`` () =
        let flag1 = Flag(true)
        let flag2 = Flag(false)
        let expected = Flag(true)
        let actual1Way = flag1.Merge(flag2)
        let actual2Way = flag2.Merge(flag1)
        Assert.AreEqual(expected, actual1Way)
        Assert.AreEqual(expected, actual2Way)

    [<Test>]
    let ``Merging two true flags gives true`` () =
        let flag1 = Flag(true)
        let flag2 = Flag(true)
        let expected = Flag(true)
        let actual = flag1.Merge(flag2)
        Assert.AreEqual(expected, actual)