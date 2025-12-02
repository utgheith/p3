lemma {:induction false} PrimeDivisorLemma (n: nat)
    requires n > 1
    ensures exists p :: Prime(p) && Divides(p, n)
    decreases n
{
    if Prime(n) {

    } else {

    }
}
