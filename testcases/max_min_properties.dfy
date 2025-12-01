function {:induction false} Max(a: int, b: int): int
{
  if a >= b then a else b
}

function {:induction false} Min(a: int, b: int): int
{
  if a <= b then a else b
}

method {:induction false} MaxMinPropertiesTest(a: int, b: int)
{
  // Max is at least each argument
  assert Max(a, b) >= a;
  assert Max(a, b) >= b;

  // Min is at most each argument
  assert Min(a, b) <= a;
  assert Min(a, b) <= b;

  // Max and Min return one of the arguments
  assert Max(a, b) == a || Max(a, b) == b;
  assert Min(a, b) == a || Min(a, b) == b;

  // Commutativity
  assert Max(a, b) == Max(b, a);
  assert Min(a, b) == Min(b, a);

  // Idempotence
  assert Max(a, a) == a;
  assert Min(a, a) == a;

  // Relation between Max and Min
  assert Min(a, b) <= Max(a, b);

  // Sum identity
  assert Max(a, b) + Min(a, b) == a + b;
}
