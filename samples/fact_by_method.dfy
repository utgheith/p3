// Illustrates the common functional-on-the-outside style where:
//    - The spec is functional
//    - The implementation is imperative
//    - Users use the functional spec not the method

// The interface
function fact(n: nat): (out: nat) {
  if (n == 0) then 1 else n * fact(n-1)
} by method {
  out := 1;
  var i := 0;

  while (i < n)
    invariant i <= n
    invariant out == fact(i)
  {
    i := i + 1;
    out := out * i;
  }
}

lemma fact_is_greater_than_zero(n : nat)
  ensures fact(n) > 0
{
}

lemma fact_n1_is_greater_than_fact_n(n : nat)
  requires n > 0
  ensures fact(n+1) > fact(n)
{
  // Need to remind dafny that fact(n) > 0
  fact_is_greater_than_zero(n);
}

lemma fact_n1_is_greater_than_or_equal_fact_n(n: nat)
  ensures fact(n+1) >= fact(n)
{
}

method Main() {
  print fact(5), "\n";
}

