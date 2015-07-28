namespace Eventual.Tests

open Eventual.Counters
open System.Collections.Immutable
open System.Collections.Generic
open NUnit.Framework

[<TestFixture>]
module GCounterTests =
    
    let kvp key value =
        KeyValuePair(key, value)
    
    [<Test>]
    let ``Adding 2 GCounters should give the correct total``() =
        let state1 = [kvp "node1" 546L; kvp "node2" 547L].ToImmutableDictionary()
        let state2 = [kvp "node1" 547L; kvp "node2" 548L].ToImmutableDictionary()
        let expectedValue = 1095L //This is max node1 + max node2
        let gcounter1 = GCounter(state1)
        let gcounter2 = GCounter(state2)
        let gcounterMerge = gcounter1.Merge(gcounter2)
        Assert.AreEqual(expectedValue, gcounterMerge.Value)
        ()