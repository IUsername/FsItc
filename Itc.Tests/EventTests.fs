namespace Itc.Tests
open NUnit.Framework
open Itc.Event

[<TestFixture>]
type ``Event Tests`` ()=
    
    [<Test>] 
    member x.``Print of Zero Leaf should print 0`` () =
        Assert.AreEqual(print (EventLeaf 0), "0")

    [<Test>] 
    member x.``Print of Node should print (0,3,(1,2,3))`` () =
        let node = EventNode 0 (EventLeaf 3) (EventNode 1 (EventLeaf 2) (EventLeaf 3))
        Assert.AreEqual(print node, "(0,3,(1,2,3))")

    [<Test>] 
    member x.``Leaf should not accept negative N`` () =
         Assert.Throws<System.Exception> (fun () -> EventLeaf -1 |> ignore) |> ignore

    [<Test>] 
    member x.``Node should not accept negative N`` () =
         Assert.Throws<System.Exception> (fun () -> EventNode -1 (EventLeaf 2) (EventLeaf 5) |> ignore) |> ignore

    [<Test>] 
    member x.``lift := n↑m = n+m`` () =
        let n = 2
        let m = 1
        let e = EventLeaf n
        let result = lift m e
        Assert.AreEqual(result, EventLeaf (n+m))

    [<Test>] 
    member x.``lift := (n,e1,e2)↑m = (n+m,e1,e2)`` () =
        let n = 2
        let m = 1
        let e = EventNode n (EventLeaf 8) (EventLeaf 9)
        let result = lift m e
        Assert.AreEqual(result, EventNode (n+m) (EventLeaf 8) (EventLeaf 9))

    [<Test>] 
    member x.``sink := n↓m = n-m`` () =
        let n = 2
        let m = 1
        let e = EventLeaf n
        let result = sink m e
        Assert.AreEqual(result, EventLeaf (n-m))

    [<Test>] 
    member x.``sink := (n,e1,e2)↓m = (n-m,e1,e2)`` () =
        let n = 2
        let m = 1
        let e = EventNode n (EventLeaf 8) (EventLeaf 9)
        let result = sink m e
        Assert.AreEqual(result, EventNode (n-m) (EventLeaf 8) (EventLeaf 9))

    [<Test>]
    member x.``Sink should cascade to sub-tree when sinking greater than N`` () =
        let e = Event.Create(2,3,4)
        let result = sink 3 e
        Assert.AreEqual(result, Event.Create(0,2,3))

    [<Test>]
    member x.``norm(n) = n`` () =
        let n = 2
        let e = EventLeaf n
        let result = normalize e
        Assert.AreEqual(result, EventLeaf n)

    [<Test>]
    member x.``norm((n,m,m)) = n+m`` () =
        let n = 2
        let m = 3
        let e = Event.Create(n,m,m)
        let result = normalize e
        Assert.AreEqual(result, EventLeaf (n+m))

    [<Test>]
    member x.``norm((n,e1,e2)) = (n+m,e1↓m,e2↓m), where m=min(min(e1),min(e2)) for leaf`` () =       
        let e = Event.Create(1,2,3)
        let result = normalize e
        Assert.AreEqual(result, Event.Create(3,0,1)) 
        
    [<Test>]
    member x.``norm((n,e1,e2)) = (n+m,e1↓m,e2↓m), where m=min(min(e1),min(e2)) for node`` () =       
        let e = Event.Create(2,Event.Create(2,1,0), Event.Create(3))
        let result = normalize e
        Assert.AreEqual(result, Event.Create(4, Event.Create(0,1,0), Event.Create(1)))

    [<Test>]
    member x.``Normalize should return recursively normalized event tree`` () =      
        let e = Event.Create(1, Event.Create(2,3,4), Event.Create(1,2,4))
        let result = normalize e
        Assert.AreEqual(result, Event.Create(4,Event.Create(2,0,1), Event.Create(0,0,2)))

    [<Test>]
    member x.``min(n) = n`` () =      
        let e = EventLeaf 3       
        Assert.AreEqual(minN e, 3)

    [<Test>]
    member x.``min((n,e1,e2)) = n+min(min(e1),min(e2))`` () =      
        let e = Event.Create(1,2,3)       
        Assert.AreEqual(minN e, 3)

    [<Test>]
    member x.``max(n) = n`` () =      
        let e = EventLeaf 3       
        Assert.AreEqual(maxN e, 3)
        
    [<Test>]
    member x.``max((n,e1,e2)) = n+max(max(e1),max(e2))`` () =      
        let e = Event.Create(1,2,3)     
        Assert.AreEqual(maxN e, 1+3)

    [<Test>]
    member x.``leq(n1,n2) = n1<=n2, case n1 < n2`` () =      
        let e1 = EventLeaf 1
        let e2 = EventLeaf 2
        Assert.IsTrue(leq e1 e2)
        
    [<Test>]
    member x.``leq(n1,n2) = n1<=n2, case n1 == n2`` () =      
        let e1 = EventLeaf 1
        let e2 = EventLeaf 1
        Assert.IsTrue(leq e1 e2)

    [<Test>]
    member x.``leq(n1,n2) = n1<=n2, case n1 > n2`` () =      
        let e1 = EventLeaf 2
        let e2 = EventLeaf 1
        Assert.IsFalse(leq e1 e2)

    [<Test>]
    member x.``leq(n1,(n2,l2,r2)) = n1<=n2`` () =      
        let e1 = EventLeaf 1
        let e2 = EventNode 2 (EventLeaf 0) (EventLeaf 0)
        Assert.IsTrue(leq e1 e2)

    [<Test>]
    member x.``leq((n1,l1,r2),n2) = n1<=n2 Λ leq(l1↑n1,n2) Λ leq(r1↑n1,n2), case n1 < n2`` () =      
        let e1 = EventNode 2 (EventLeaf 0) (EventLeaf 0)
        let e2 = EventLeaf 3
        Assert.IsTrue(leq e1 e2)

    [<Test>]
    member x.``leq((n1,l1,r2),n2) = n1<=n2 Λ leq(l1↑n1,n2) Λ leq(r1↑n1,n2), case n1+l1<n2`` () =      
        let e1 = EventNode 2 (EventLeaf 1) (EventLeaf 0)
        let e2 = EventLeaf 3
        Assert.IsTrue(leq e1 e2)

    [<Test>]
    member x.``leq((n1,l1,r2),n2) = n1<=n2 Λ leq(l1↑n1,n2) Λ leq(r1↑n1,n2), case n1+r1<n2`` () =      
        let e1 = EventNode 2 (EventLeaf 0) (EventLeaf 1)
        let e2 = EventLeaf 3
        Assert.IsTrue(leq e1 e2)
      
    [<Test>]
    member x.``leq((n1,l1,r2),n2) = n1<=n2 Λ leq(l1↑n1,n2) Λ leq(r1↑n1,n2), case n1+l1>n2`` () =      
        let e1 = EventNode 2 (EventLeaf 2) (EventLeaf 0)
        let e2 = EventLeaf 3
        Assert.IsFalse(leq e1 e2)       
        
    [<Test>]
    member x.``leq((n1,l1,r2),n2) = n1<=n2 Λ leq(l1↑n1,n2) Λ leq(r1↑n1,n2), case n1+r1>n2`` () =      
        let e1 = EventNode 2 (EventLeaf 0) (EventLeaf 2)
        let e2 = EventLeaf 3
        Assert.IsFalse(leq e1 e2) 

    [<Test>]
    member x.``leq((n1,l1,r2),(n2,l2,r2)) = n1<=n2 Λ leq(l1↑n1,l2↑n2) Λ leq(r1↑n1,r2↑n2), case n1=n2, l1<l2`` () =      
        let e1 = EventNode 2 (EventLeaf 0) (EventLeaf 0)
        let e2 = EventNode 2 (EventLeaf 0) (EventLeaf 0)
        Assert.IsTrue(leq e1 e2) 

    [<Test>]
    member x.``leq((n1,l1,r2),(n2,l2,r2)) = n1<=n2 Λ leq(l1↑n1,l2↑n2) Λ leq(r1↑n1,r2↑n2), case n1=n2, l1=l2`` () =      
        let e1 = EventNode 2 (EventLeaf 2) (EventLeaf 0)
        let e2 = EventNode 2 (EventLeaf 2) (EventLeaf 0)
        Assert.IsTrue(leq e1 e2) 

    [<Test>]
    member x.``leq((n1,l1,r2),(n2,l2,r2)) = n1<=n2 Λ leq(l1↑n1,l2↑n2) Λ leq(r1↑n1,r2↑n2), case n1=n2, l1>l2`` () =      
        let e1 = EventNode 2 (EventLeaf 3) (EventLeaf 0)
        let e2 = EventNode 2 (EventLeaf 2) (EventLeaf 0)
        Assert.IsFalse(leq e1 e2) 

    [<Test>]
    member x.``join(n1,n2) = max(n1,n2)`` () =      
        let e1 = EventLeaf 1
        let e2 = EventLeaf 2
        Assert.AreEqual(join e1 e2, EventLeaf 2) 

    [<Test>]
    member x.``join(n1,(n2,l2,r2)) = join((n1,0,0),(n2,l2,r2))`` () =      
        let e1 = EventLeaf 1
        let e2 = Event.Create(2,0,1)
        Assert.AreEqual(join e1 e2, Event.Create(2,0,1)) 

    [<Test>]
    member x.``join((n1,l1,r1),n2) = join((n1,l1,r1),(n2,0,0))`` () =      
        let e1 = Event.Create(2,0,1)
        let e2 = EventLeaf 1
        Assert.AreEqual(join e1 e2, Event.Create(2,0,1)) 

    [<Test>]
    member x.``join((n1,l1,r1),(n2,l2,r2)) = join((n2,l2,r2),(n1,l1,r1)), if n1>n2`` () =      
        let e1 = Event.Create(2,0,1)
        let e2 = Event.Create(1,1,0)
        Assert.AreEqual(join e1 e2, Event.Create(2,0,1)) 

    [<Test>]
    member x.``join((n1,l1,r1),(n2,l2,r2)) = norm((n1,join(l1,l2↑n2-n1),join(r1,r2↑n2-n1)))`` () =      
        let e1 = Event.Create(1,2,0)
        let e2 = Event.Create(3,1,3)
        Assert.AreEqual(join e1 e2, Event.Create(4,0,2)) 