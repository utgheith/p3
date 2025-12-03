lemma {:induction false} SumDistributes(a: seq<int>, b: seq<int>)
  ensures Sum(a + b) == Sum(a) + Sum(b)
  decreases |a|
{
}

lemma {:induction false} Prove_TreeSize(t: Tree)
  ensures Size(t) == |Flatten(t)|
{
}

lemma {:induction false} Prove_TreeSum(t: Tree)
  ensures SumTree(t) == Sum(Flatten(t))
{
}
