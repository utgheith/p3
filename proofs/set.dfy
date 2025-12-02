lemma {:induction false} TopDownBottomUpEquivalence(x : int)
  requires x >= 0
  ensures TopDown(x) == BottomUp(x, x)
{
  
}
