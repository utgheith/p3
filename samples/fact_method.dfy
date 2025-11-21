// Illustrates how to prove that an imperative implementation
// is functionally equivelant to a functional spec

// Defines factorial
ghost function fact_spec(n: nat): nat {
  if (n == 0) then 1 else n * fact_spec(n-1)
}

// implements factorial
// most of the assertion below are not strictly required (Dafny can figure them out) but
//    it's not a bad idea to practice thinking like a verifier
method fact(n: nat) returns (out:int)
  ensures out == fact_spec(n)
{
  out := 1;
  var i := 0;
  // The invariant holds before the loop
  assert out == fact_spec(i) && i <= n; // not required
  while (i < n)
    decreases (n-i)                 // not required
    invariant i <= n
    invariant out == fact_spec(i)
  {
    // Remember the body for a loop?
    //    I => WP(body, I)
    // I hope you agree that:
    assert (out == fact_spec(i) && (i <= n)) ==> (out == fact_spec(i) && ((i+1) <= n)); // not required
    // it makes more sense to talk the assertion below backwards and
    // observe how applying the WP subsititutions preserves the loop
    // invariant. This is a key insight.
    assert out == fact_spec(i) && (i+1) <= n; // not required
    assert out*(i+1) == (i+1)*fact_spec(i) && (i+1) <= n; // not required
    assert out*(i+1) == fact_spec(i+1) && (i+1) <= n; // not required
    i := i + 1;
    // invariant might be violated but we'll restore it beofre the end of the body
    assert out*i == fact_spec(i) && i <= n; // not required
    out := out * i;
    assert out == fact_spec(i) && i <= n; // not required
  }
  assert !(i < n) && (i <= n) && (out == fact_spec(i)); // not required
  assert (i >= n) && (i <= n) && (out == fact_spec(i)); // not required
  assert (i == n) && (out == fact_spec(i)); // not required
  assert (i == n) && (out == fact_spec(n)); // not required
  assert out == fact_spec(n); // not required, QED
}

lemma fact_is_greater_than_zero(n : nat)
  ensures fact_spec(n) > 0
{

}

lemma fact_n1_is_greater_than_fact_n(n : nat)
  requires n > 0
  ensures fact_spec(n+1) > fact_spec(n)
{

  // Need to remind dafny that fact(n) > 0
  fact_is_greater_than_zero(n);

  // Dafny 4.0 can figure out this calculation on its own but
  // it's not a bad idea to try to think like it
  calc {
    fact_spec(n+1);
  ==
    (n+1) * fact_spec(n);
  ==
    n*fact_spec(n) + fact_spec(n);
  >=
    fact_spec(n);
  }


}

lemma fact_n1_is_greater_than_or_equal_fact_n(n: nat)
  ensures fact_spec(n+1) >= fact_spec(n)
{

}

