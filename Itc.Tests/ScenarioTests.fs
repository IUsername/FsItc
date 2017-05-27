namespace Itc.Tests
open NUnit.Framework
open Itc.Id
open Itc.Event
open Itc.Stamp
open Itc.Tests.Processing

[<TestFixture>]
type ``Scenario Tests`` ()=

    // Example from ITC 2008 paper, section 5.1
    //
    //                                    e
    //                             a3-----------a4----------+
    //                            /                          \                 
    //                e          /                            \          e       
    //            a1-----a2-----f                              j-----a5-----a6    
    //           /               \                            /
    //          /                 \                          /
    //         /                   c1---+              c3---+                  
    //  seed--f                          \            /
    //         \                          \          /
    //          \                          j--------f
    //           \                        /  (sync)  \
    //            \                      /            \
    //             b1-----b2-----b3-----+              b4                     
    //                 e      e
    //
    [<Test>] 
    member x.``Example from ITC 2008 paper, section 5.1`` () =
        let seed = Stamp.Seed
        let (a1,b1) = fork seed
        Assert.AreEqual(Stamp.Create(Node(One,Zero), Event.Create(0)), a1)
        Assert.AreEqual(Stamp.Create(Node(Zero,One), Event.Create(0)), b1)

        let a2 = event a1
        Assert.AreEqual(Stamp.Create(Node(One,Zero), Event.Create(0,1,0)), a2)

        let (a3,c1) = fork a2
        Assert.AreEqual(Stamp.Create(Node(Node(One,Zero),Zero), Event.Create(0,1,0)), a3)
        Assert.AreEqual(Stamp.Create(Node(Node(Zero,One),Zero), Event.Create(0,1,0)), c1)

        let a4 = event a3
        Assert.AreEqual(Stamp.Create(Node(Node(One,Zero),Zero), Event.Create(0, Event.Create(1,1,0), Event.Create(0))), a4)

        let b2 = event b1
        Assert.AreEqual(Stamp.Create(Node(Zero,One), Event.Create(0,0,1)), b2)

        let b3 = event b2
        Assert.AreEqual(Stamp.Create(Node(Zero,One), Event.Create(0,0,2)), b3)

        let (c3,b4) = sync b3 c1
        Assert.AreEqual(Stamp.Create(Node(Node(Zero,One),Zero), Event.Create(1,0,1)), c3)
        Assert.AreEqual(Stamp.Create(Node(Zero,One), Event.Create(1,0,1)), b4)

        let a5 = join a4 c3
        Assert.AreEqual(Stamp.Create(Node(One,Zero), Event.Create(1, Event.Create(0,1,0), Event.Create(1))), a5)

        let a6 = event a5
        Assert.AreEqual(Stamp.Create(Node(One,Zero), Event.Create(2)), a6)

    //         ^            ^ time ↑
    //         |            |
    // evt p3  *            |
    //         |            |
    // rcv p2  *---+    +---* q2 rcv
    //         |    \  /    |
    //         |     \/     |
    //         |     /\     |
    //         |    /  \    |
    // rcv p1  *---+    +---* q1 snd
    //         |            |
    [<Test>] 
    member x.``Ordering of events`` () =
        let seed = Stamp.Seed
        let (f1,f2) = fork seed
        let p = CreateProcess("p", f1)
        let q = CreateProcess("q", f2)

        let (msgp1, p1) = p.Send "p1"
        printfn "p1:%s after send" (print p1.CurrentStamp)

        let (msgq1, q1) = q.Send "q1"       
        printfn "q1:%s after send" (print q1.CurrentStamp)

        let p2 = p1.Receive msgq1
        printfn "p2:%s after receive" (print p2.CurrentStamp)

        let q2 = q1.Receive msgp1
        printfn "q2:%s after receive" (print q2.CurrentStamp)

        let p3 = p2.Increment
        printfn "p3:%s after event" (print p3.CurrentStamp)

        Assert.IsTrue(leq p1.CurrentStamp p2.CurrentStamp)
        Assert.IsTrue(leq p1.CurrentStamp p3.CurrentStamp)
        Assert.IsTrue(leq p2.CurrentStamp p3.CurrentStamp)
        Assert.IsTrue(leq q1.CurrentStamp q2.CurrentStamp)

        Assert.IsTrue(leq p1.CurrentStamp q2.CurrentStamp)
        Assert.IsTrue(leq q1.CurrentStamp p2.CurrentStamp)

        Assert.IsTrue(isConcurrent p1.CurrentStamp q1.CurrentStamp)
        Assert.IsTrue(isConcurrent p2.CurrentStamp q2.CurrentStamp)
        Assert.IsTrue(isConcurrent p3.CurrentStamp q2.CurrentStamp) 

        let list = [q2; q1; p3; p2; p1]
        let ordered = List.sortWith stampComparer list

        for s in ordered do printf "%s:%s " s.name (print s.CurrentStamp)

        let filter (except:list<_>) (items:list<_>) =
            items |> List.filter (fun i -> except |> List.exists (fun t -> t = i))

        let containsInOrder subSet list =
            (filter subSet list) = subSet

        Assert.IsTrue(containsInOrder [p1;p2;p3] ordered)
        Assert.IsTrue(containsInOrder [q1;q2] ordered)
        Assert.IsTrue(containsInOrder [p1;q2] ordered)       
        Assert.IsTrue(containsInOrder [q1;p2;p3] ordered)

    // Source http://zoo.cs.yale.edu/classes/cs426/2013/bib/parker83detection.pdf
    // Fig. 1. Partition graph G(f) for file stored redundantly at sites A, B, C, D.
    //
    //                   +------+
    //          +--------+ ABCD +-------+
    //          |        +------+       |
    //          |                       |
    //        +-v--+                  +-v--+
    //     *1 | AB +-------+  +-------+ CD +-------+
    //        +----+       |  |       +----+       |
    //          |         +v--v+                 +-v-+
    //          |      *2 | BC |                 | D |
    //          |         +----+                 +---+
    //          |            |                     |
    //        +-v-+          |        +-----+      |
    //     *3 | A |          +--------> BCD <------+
    //        +---+                   +-----+
    //          |                        |
    //          |     *4 +------+        |
    //          +--------> ABCD <--------+
    //                   +------+
    //
    // [*#] indicates modification to file

    // Fork seed stamp into four "sites"
    [<Test>] 
    member x.``Example from Detection of Mutual Inconsistency in Distributed Systems, Fig. 1`` () =
        let (a,b,c,d) = Stamp.Seed |> fork4
        printfn "Site A: %s" (print a)
        printfn "Site B: %s" (print b)
        printfn "Site C: %s" (print c)
        printfn "Site D: %s" (print d)

        let stampOnA = peek a
        printfn "Initial file A stamp: %s" (print stampOnA)

        let stampOnB = peek b
        printfn "Initial file B stamp: %s" (print stampOnB)
        
        let stampOnC = peek c
        printfn "Initial file C stamp: %s" (print stampOnC)
        
        let stampOnD = peek d
        printfn "Initial file D stamp: %s" (print stampOnD)

        // Due to network partitions, clusters of sites are isolated from others at
        // various times.

        // A and B can communicate (forming {AB} partition), but are isolated from C and D
        // C and D can communicate (forming {CD} partition), but are isolated from A and B

        // File is modified at site A in {AB} partition
        let a = event a
        let stampOnA = peek a
        printfn "File modified on A: %s" (print stampOnA)

        // Site A notifies site B about the change to the file
        printfn "Sync A->B"
        Assert.IsFalse(isConcurrent b stampOnA)
        let b = join b stampOnA
        let stampOnB = peek b
        printfn "File synced on B: %s" (print stampOnB)

        // A and B become isolated from each other
        // C and D become isolated from each other

        // B and C resume communication, forming {BC} partition
        // Site B notifies site C about the previous change to the file
        printfn "Sync B->C"
        Assert.IsFalse(isConcurrent c stampOnB)
        let c = join c stampOnB
        let stampOnC = peek c
        printfn "File synced on C: %s" (print stampOnC)

        // File is modified at site C in {BC} partition
        let c = event c
        let stampOnC = peek c
        printfn "File modified on C: %s" (print stampOnC)

        // Site C shares the change with site B
        // There should not be a conflict (concurrent stamp) between B and C.
        printfn "Sync C->B"
        Assert.IsFalse(isConcurrent b stampOnC)
        let b = join b stampOnC
        let stampOnB = peek b
        printfn "File synced on B: %s" (print stampOnB)

        // File is modified at site A in {A} partition
        let a = event a
        let stampOnA = peek a
        printfn "File modified on A: %s" (print stampOnA)

        // B, C, and D resume communication, forming {BCD} partition
        // Since they can now communicate with each other, B and C share information
        // about the modified file with D
        printfn "Sync C->D"
        Assert.IsFalse(isConcurrent d stampOnC)
        let d = join d stampOnC
        let stampOnD = peek d
        printfn "File synced on D: %s" (print stampOnD)

        // A, B, C, and D again establish communication and share information
        // about the file changes
        //
        // There should be a conflict (concurrent stamps) between A and the
        // other sites: B, C, and D
        Assert.IsTrue(isConcurrent a stampOnB)
        Assert.IsTrue(isConcurrent a stampOnC)
        Assert.IsTrue(isConcurrent a stampOnD)

        // The concurrent stamps indicate there is a conflict which must
        // be resolved (manual or otherwise). Once the conflict is resolved
        // the updated file is stored and a new stamp must be generated that
        // dominates the others... this can be done by joining the stamps from
        // the (previously) concurrent stamps and inflating the stamp at A.
        let a = join a stampOnB
        let a = join a stampOnC
        let a = join a stampOnD
        let a = event a // inflate for conflict resolution
        let stampOnA = peek a // update file metadata
        printfn "Resolve conflict on A: %s" (print stampOnA)

        // Site A communicates conflict resolution to other sites
        printfn "Sync A->B"
        Assert.IsFalse(isConcurrent b stampOnA)
        let b = join b stampOnA
        let stampOnB = peek b
        printfn "File synced on B: %s" (print stampOnB)

        printfn "Sync A->C"
        Assert.IsFalse(isConcurrent c stampOnA)
        let c = join c stampOnA
        let stampOnC = peek c
        printfn "File synced on C: %s" (print stampOnC)

        printfn "Sync A->D"
        Assert.IsFalse(isConcurrent d stampOnA)
        let d = join d stampOnA
        let stampOnD = peek d
        printfn "File synced on D: %s" (print stampOnD)