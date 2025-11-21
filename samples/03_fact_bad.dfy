// It is easy to complicate your loop invariant by choosing the
// "wrong" inititial state.
//
// The code is still correct but slightly harder to reason about.

ghost function fact_spec(n: nat): nat {
  if (n == 0) then 1 else n * fact_spec(n-1)
}

method fact(n: nat) returns (out: nat)
  ensures out == fact_spec(n)
{
  out := 1;
  var i := 1;
  while (i <= n)
    decreases (n-i)
    invariant i <= n+1
    invariant out == fact_spec(i-1)
  {
    out := out * i;
    i := i + 1;
  }
}

method Main() {
  var out := fact(5);
  print out, "\n";
}
