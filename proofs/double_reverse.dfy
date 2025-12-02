lemma {:induction false} DoubleReverseLemma(s: seq<int>)
    ensures Reverse(Reverse(s)) == s
{
}
