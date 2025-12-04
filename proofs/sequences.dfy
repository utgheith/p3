lemma {:induction false} Proof(n : int) 
requires n > 0
ensures Sequence(n) == n * (n + 1) / 2
{
    if n != 1 {
        Proof(n - 1);
    } 
}