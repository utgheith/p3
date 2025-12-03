lemma {:induction false} SwapIsPermutation<T(!new)>(s: seq<T>, i: nat, j: nat)
  requires i < |s| && j < |s|
  ensures IsPermutation(s, Swap(s, i, j))
{
}