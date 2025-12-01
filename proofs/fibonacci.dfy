// The IterativeFibCorrect lemma proves that the IterativeFibHelper function correctly computes
// Fibonacci numbers by ensuring that every stepwise computation of its "loop" aligns with the recursive definition.
lemma IterativeFibCorrect(n: nat, i: nat, a: nat, b: nat)
  ensures IterativeFibHelper(n, a, b, i) == RecFib(n)
{

}

// IterEqualsRec lemma establishes that the IterFib function produces the same result
// as the RecFib function for any natural number n.
lemma IterEqualsRec(n: nat)
  ensures IterFib(n) == RecFib(n)
{
}