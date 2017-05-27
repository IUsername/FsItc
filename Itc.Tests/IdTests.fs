namespace Itc.Tests
open NUnit.Framework
open Itc.Id

[<TestFixture>]
type ``Id Tests`` ()=
    
    [<Test>] member test.
        ``Print of Zero Leaf should print 0`` ()=
        Assert.AreEqual(print Zero, "0")

    [<Test>] member test.
        ``Print of One Leaf should print 1`` ()=
        Assert.AreEqual(print One, "1")

    [<Test>] member test.
        ``Print of Node should print values separated by comma`` ()=
        Assert.AreEqual(print (Node(Zero, One)), "(0,1)")

    [<Test>] member test.
        ``Print of Complex Node should print values separated by comma`` ()=
        Assert.AreEqual(print (Node(One, Node(Zero, One))), "(1,(0,1))")

    [<Test>] member test.
        ``norm(i) = i`` ()=
        Assert.AreEqual(normalize One, One)

    [<Test>] member test.
        ``norm((0,0)) = 0`` ()=
        Assert.AreEqual(normalize (Node(Zero,Zero)), Zero)

    [<Test>] member test.
        ``norm((1,1)) = 1`` ()=
        Assert.AreEqual(normalize (Node(One,One)), One)

    [<Test>] member test.
        ``norm((0,1)) = (0,1)`` ()=
        Assert.AreEqual(normalize (Node(Zero,One)), Node(Zero,One))

    [<Test>] member test.
        ``norm((1,0)) = (1,0)`` ()=
        Assert.AreEqual(normalize (Node(One,Zero)), Node(One,Zero))

    [<Test>] member test.
        ``split(0) = (0,0)`` ()=
        Assert.AreEqual(split Zero, Node(Zero,Zero))

    [<Test>] member test.
        ``split(1) = ((1,0),(0,1))`` ()=
        Assert.AreEqual(split One, Node(Node(One,Zero), Node(Zero,One)))

    [<Test>] member test.
        ``split((0,1)) = ((0,(1,0)),((0,(0,1)))`` ()=
        Assert.AreEqual(split (Node(Zero,One)), Node(Node(Zero, Node(One,Zero)), Node(Zero, Node(Zero,One))))

    [<Test>] member test.
        ``split((0,(0,1))) = ((0,(0,(1,0))),(0,(0,(0,1))))`` ()=
        Assert.AreEqual(split (Node(Zero,Node(Zero,One))) |> print, "((0,(0,(1,0))),(0,(0,(0,1))))")

    [<Test>] member test.
        ``split((1,0)) = (((1,0),0),((0,1),0))`` ()=
        Assert.AreEqual(split (Node(One, Zero)) |> print, "(((1,0),0),((0,1),0))")

    [<Test>] member test.
        ``split(((1,0),0)) = ((((1,0),0),0),(((0,1),0),0))`` ()=
        Assert.AreEqual(split (Node(Node(One, Zero), Zero)) |> print, "((((1,0),0),0),(((0,1),0),0))")

    [<Test>] member test.
        ``split(((0,1),(1,0))) = (((0,1),0),(0,(1,0)))`` ()=
        Assert.AreEqual(split (Node(Node(Zero, One), Node(One,Zero))) |> print, "(((0,1),0),(0,(1,0)))")

    [<Test>] member test.
        ``sum(0,0) = 0`` ()=
        Assert.AreEqual(sum Zero Zero, Zero)

    [<Test>] member test.
        ``sum(0,1) = 1`` ()=
        Assert.AreEqual(sum Zero One, One)

    [<Test>] member test.
        ``sum(1,0) = 1`` ()=
        Assert.AreEqual(sum One Zero, One)

    [<Test>] member test.
        ``sum(0,(0,1)) = (0,1)`` ()=
        Assert.AreEqual(sum Zero (Node(Zero,One)), Node(Zero,One))

    [<Test>] member test.
        ``sum(0,(1,0)) = (1,0)`` ()=
        Assert.AreEqual(sum Zero (Node(One,Zero)), Node(One,Zero))

    [<Test>] member test.
        ``sum((0,1),0) = (0,1)`` ()=
        Assert.AreEqual(sum (Node(Zero,One)) Zero, Node(Zero,One))

    [<Test>] member test.
        ``sum((1,0),0) = (1,0)`` ()=
        Assert.AreEqual(sum (Node(One,Zero)) Zero, Node(One,Zero))

    [<Test>] member test.
        ``sum((0,1),(0,1)) = (0,1)`` ()=
        Assert.AreEqual(sum (Node(Zero,One)) (Node(Zero,One)), Node(Zero,One))

    [<Test>] member test.
        ``sum((0,1),(1,0)) = 1`` ()=
        Assert.AreEqual(sum (Node(Zero,One)) (Node(One,Zero)), One)

    [<Test>] member test.
        ``sum((1,0),(1,0)) = (1,0)`` ()=
        Assert.AreEqual(sum (Node(One,Zero)) (Node(One,Zero)), Node(One,Zero))

    [<Test>] member test.
        ``sum((1,0),(0,1)) = 1`` ()=
        Assert.AreEqual(sum (Node(One,Zero)) (Node(Zero,One)), One)

    [<Test>] member test.
        ``sum(((1,0),0),((0,1),0)) = (1,0)`` ()=
        Assert.AreEqual(sum (Node(Node(One,Zero), Zero)) (Node(Node(Zero,One),Zero)), Node(One,Zero))