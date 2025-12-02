lemma {:induction false} MaxMin_Bounds(a: int, b: int)
  ensures Max(a, b) >= a
  ensures Max(a, b) >= b
  ensures Min(a, b) <= a
  ensures Min(a, b) <= b
  ensures Max(a, b) == a || Max(a, b) == b
  ensures Min(a, b) == a || Min(a, b) == b
{
}

lemma {:induction false} MaxMin_Symmetry(a: int, b: int)
  ensures Max(a, b) == Max(b, a)
  ensures Min(a, b) == Min(b, a)
  ensures Max(a, a) == a
  ensures Min(a, a) == a
{
}

lemma {:induction false} MaxMin_Relation(a: int, b: int)
  ensures Min(a, b) <= Max(a, b)
  ensures Max(a, b) + Min(a, b) == a + b
{
}
