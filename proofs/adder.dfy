lemma {:induction false} AddAllProof(lst: seq<int>, acc: int)
    ensures add_all_fold(lst, acc) == add_all_recursive(lst, acc)
    decreases |lst|
{
}