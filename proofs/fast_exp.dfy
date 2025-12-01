// Proofs for fast exponentiation in testcases/fast_exp.dfy
lemma {:induction false} BasicPowAdd(base: int, n: nat, m: nat)
  ensures BasicPow(base, n + m) == BasicPow(base, n) * BasicPow(base, m)
  decreases n
{
}

lemma {:induction false} FastPowEqualsBasicPow(base: int, exp: nat)
  ensures FastPow(base, exp) == BasicPow(base, exp)
  decreases exp
{
}
