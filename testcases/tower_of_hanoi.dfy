function {:induction false} Power2(n: nat): nat
    decreases n
{
    if n == 0 then 1
    else 2 * Power2(n - 1)
}

function {:induction false} MoveDisk(from: int, to: int): (int, int)
{
    (from, to)
}

function {:induction false} ValidState(peg0: seq<int>, peg1: seq<int>, peg2: seq<int>): bool
{
    (forall i, j :: 0 <= i < j < |peg0| ==> peg0[i] < peg0[j]) &&
    (forall i, j :: 0 <= i < j < |peg1| ==> peg1[i] < peg1[j]) &&
    (forall i, j :: 0 <= i < j < |peg2| ==> peg2[i] < peg2[j])
}

function {:induction false} Hanoi(n: nat, source: int, destination: int, auxiliary: int): seq<(int, int)>
    requires 0 <= source <= 2 && 0 <= destination <= 2 && 0 <= auxiliary <= 2
    requires source != destination && source != auxiliary && destination != auxiliary
    decreases n
{
    if n == 0 then []
    else if n == 1 then [MoveDisk(source, destination)]
    else
        Hanoi(n - 1, source, auxiliary, destination) +
        [MoveDisk(source, destination)] +
        Hanoi(n - 1, auxiliary, destination, source)
}

// Apply a sequence of moves to a state
function {:induction false} ApplyMoves(peg0: seq<int>, peg1: seq<int>, peg2: seq<int>, moves: seq<(int, int)>): (seq<int>, seq<int>, seq<int>)
    decreases |moves|
{
    if |moves| == 0 then (peg0, peg1, peg2)
    else
        var (from, to) := moves[0];
        var (p0, p1, p2) := if from == 0 && |peg0| > 0 then
            if to == 1 then (peg0[..|peg0|-1], peg1 + [peg0[|peg0|-1]], peg2)
            else (peg0[..|peg0|-1], peg1, peg2 + [peg0[|peg0|-1]])
        else if from == 1 && |peg1| > 0 then
            if to == 0 then (peg0 + [peg1[|peg1|-1]], peg1[..|peg1|-1], peg2)
            else (peg0, peg1[..|peg1|-1], peg2 + [peg1[|peg1|-1]])
        else if from == 2 && |peg2| > 0 then
            if to == 0 then (peg0 + [peg2[|peg2|-1]], peg1, peg2[..|peg2|-1])
            else (peg0, peg1 + [peg2[|peg2|-1]], peg2[..|peg2|-1])
        else
            (peg0, peg1, peg2);
        ApplyMoves(p0, p1, p2, moves[1..])
}

// Initial state: all disks on source peg
function {:induction false} InitialState(n: nat, source: int): (seq<int>, seq<int>, seq<int>)
    requires 0 <= source <= 2
{
    var disks := seq(n, i => i);
    if source == 0 then (disks, [], [])
    else if source == 1 then ([], disks, [])
    else ([], [], disks)
}

// does hanoi work?
method {:induction false} TestHanoi(n: nat, source: int, destination: int, auxiliary: int)
    requires 0 <= source <= 2 && 0 <= destination <= 2 && 0 <= auxiliary <= 2
    requires source != destination && source != auxiliary && destination != auxiliary
{
    var moves := Hanoi(n, source, destination, auxiliary);
    var (p0_init, p1_init, p2_init) := InitialState(n, source);
    var (p0_final, p1_final, p2_final) := ApplyMoves(p0_init, p1_init, p2_init, moves);
    
    // check that the number of moves is correct
    assert |moves| == (if n == 0 then 0 else Power2(n) - 1) by {
        HanoiMoveCount(n, source, destination, auxiliary);
    }
    
    // check that each peg has the correct disks
    assert AllDisksOnDestination(n, p0_final, p1_final, p2_final, destination) by {
        HanoiCorrectness(n, source, destination, auxiliary);
    }
    
    // check that the state is valid
    assert ValidState(p0_final, p1_final, p2_final) by {
        HanoiValidState(n, source, destination, auxiliary);
    }
}

