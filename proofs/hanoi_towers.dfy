lemma HanoiMoveCount(n: nat, source: int, destination: int, auxiliary: int)
    requires 0 <= source <= 2 && 0 <= destination <= 2 && 0 <= auxiliary <= 2
    requires source != destination && source != auxiliary && destination != auxiliary
    ensures |Hanoi(n, source, destination, auxiliary)| == (if n == 0 then 0 else Power2(n) - 1)
    decreases n
{
    // TODO: prove this
}

lemma HanoiValidState(n: nat, source: int, destination: int, auxiliary: int)
    requires 0 <= source <= 2 && 0 <= destination <= 2 && 0 <= auxiliary <= 2
    requires source != destination && source != auxiliary && destination != auxiliary
    ensures ValidState(
        ApplyMoves(InitialState(n, source).0, InitialState(n, source).1, InitialState(n, source).2,
                   Hanoi(n, source, destination, auxiliary)).0,
        ApplyMoves(InitialState(n, source).0, InitialState(n, source).1, InitialState(n, source).2,
                   Hanoi(n, source, destination, auxiliary)).1,
        ApplyMoves(InitialState(n, source).0, InitialState(n, source).1, InitialState(n, source).2,
                   Hanoi(n, source, destination, auxiliary)).2)
    decreases n
{
    // TODO: prove this
}

