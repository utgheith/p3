lemma {:induction false} proof(x: int)
    ensures x < More(x)
    decreases x
{
    if x <= 0 {
    } else {
        proof(x - 2);
    }
}
