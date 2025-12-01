function {:induction false} Sequence(n : int): int
requires n > 0
{
    if n == 1 then 1 else n + Sequence(n - 1)
}

lemma {:induction false} SequenceTest(n : int) 
requires n > 0
ensures Sequence(n) == n * (n + 1) / 2
{
    assert Sequence(n) == n * (n + 1) / 2 by {
        Proof(n);
    }
}
