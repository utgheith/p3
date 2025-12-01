// Proofs for mutual recursion functions in testcases/mutual_rec.dfy
lemma {:induction false} FEqualsSum(n: nat, m: nat)
  ensures f(n, m) == n + m
  decreases n, m
{
}

lemma {:induction false} GEqualsSum(n: nat, m: nat)
  ensures g(n, m) == n + m
  decreases n, m
{
}
