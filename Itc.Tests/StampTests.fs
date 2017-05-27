namespace Itc.Tests
open NUnit.Framework
open Itc.Id
open Itc.Event
open Itc.Stamp

[<TestFixture>]
type ``Stamp Tests`` ()=

    [<Test>] 
    member x.``Seed should be (1,0)`` () =
        Assert.AreEqual(Stamp.Seed, Stamp.Create(One, (EventLeaf 0)))   
        
    [<Test>] 
    member x.``fill(0,e) = e`` () =
        let s = Stamp.Create(Zero, Event.Create(1,2,3))
        Assert.AreEqual(Event.Create(1,2,3), fill s)   

    [<Test>] 
    member x.``fill(1,e) = max(e), test e is leaf`` () =
        let s = Stamp.Create(One, Event.Create(3))
        Assert.AreEqual(Event.Create(3), fill s)
        
    [<Test>] 
    member x.``fill(1,e) = max(e), test e is node`` () =
        let s = Stamp.Create(One, Event.Create(3,1,0))
        Assert.AreEqual(Event.Create(4), fill s)   

    [<Test>] 
    member x.``fill(i,n) = n, test id leaf:0`` () =
        let s = Stamp.Create(Zero, Event.Create(3))
        Assert.AreEqual(Event.Create(3), fill s)   

    [<Test>] 
    member x.``fill(i,n) = n, test id leaf:1`` () =
        let s = Stamp.Create(One, Event.Create(3))
        Assert.AreEqual(Event.Create(3), fill s)   

    [<Test>] 
    member x.``fill(i,n) = n, test id node`` () =
        let s = Stamp.Create(Node(Node(One, Zero), Zero), Event.Create(3))
        Assert.AreEqual(Event.Create(3), fill s)   

    [<Test>] 
    member x.``fill((1,ir)) = norm((n,max(max(el),min(eʹr)),eʹr)), where eʹr=fill(ir,er)`` () =
        let s = Stamp.Create(Node(One, Zero), Event.Create(1, Event.Create(0,1,0), Event.Create(1)))
        Assert.AreEqual(Event.Create(2), fill s)   

    [<Test>] 
    member x.``fill((il,1)) = norm((n,eʹl,max(max(er),min(eʹl)))), where eʹl=fill(il,el)`` () =
        let s = Stamp.Create(Node(Zero, One), Event.Create(1, Event.Create(1), Event.Create(0,1,0)))
        Assert.AreEqual(Event.Create(2), fill s)  

    [<Test>] 
    member x.``fill((il,ir)) = norm(n,fill(il,el),fill(ir,er))`` () =
        let s = Stamp.Create(Node(Node(One,Zero),Node(Zero, One)), Event.Create(1, Event.Create(0,1,2), Event.Create(0,1,2)))
        Assert.AreEqual(Event.Create(2, Event.Create(1), Event.Create(0,0,1)), fill s)  

    [<Test>] 
    member x.``grow(1,n) = (n+1,0)`` () =
        let s = Stamp.Create(One, Event.Create(2))
        let (e,cost) = grow s
        Assert.AreEqual(Event.Create(3), e)  
        Assert.AreEqual(0, cost)  

    [<Test>] 
    member x.``grow(i,n), where (eʹ,c) = grow(i,(n,0,0)), test id:(0,1)`` () =
        let s = Stamp.Create(Node(Zero,One), Event.Create(1))
        let (e,cost) = grow s
        Assert.AreEqual(Event.Create(1,0,1), e)  
        Assert.AreEqual(1001, cost) 

    [<Test>] 
    member x.``grow(i,n), where (eʹ,c) = grow(i,(n,0,0)), test id:(1,0)`` () =
        let s = Stamp.Create(Node(One,Zero), Event.Create(1))
        let (e,cost) = grow s
        Assert.AreEqual(Event.Create(1,1,0), e)  
        Assert.AreEqual(1001, cost) 

    [<Test>] 
    member x.``grow((0,ir),(n,el,er)) = ((n,el,eʹr),cr+1), where (eʹr,cr)=grow(ir,er)`` () =
        let s = Stamp.Create(Node(Zero,One), Event.Create(1,1,0))
        let (e,cost) = grow s
        Assert.AreEqual(Event.Create(1,1,1), e)  
        Assert.AreEqual(1, cost) 

    [<Test>] 
    member x.``grow((il,0),(n,el,er)) = ((n,eʹl,er),cl+1), where (eʹl,cl)=grow(ilr,el)`` () =
        let s = Stamp.Create(Node(One,Zero), Event.Create(1,1,0))
        let (e,cost) = grow s
        Assert.AreEqual(Event.Create(1,2,0), e)  
        Assert.AreEqual(1, cost) 

    [<Test>] 
    member x.``Leq should return True when comparing original stamp and inflated stamp`` () =
        let s1 = Stamp.Seed
        let s2 = event s1      
        Assert.IsTrue(leq s1 s2) 

    [<Test>] 
    member x.``Leq should return False when comparing inflated stamp and original stamp`` () =
        let s1 = Stamp.Seed
        let s2 = event s1      
        Assert.IsFalse(leq s2 s1) 

    [<Test>] 
    member x.``Leq should return True when comparing with itself`` () =
        let s = event (event Stamp.Seed)  
        Assert.IsTrue(leq s s) 