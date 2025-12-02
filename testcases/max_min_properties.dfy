// max_min_properties.dfy
// Basic properties of Max and Min on integers.

function {:induction false} Max(a: int, b: int): int
{
  if a >= b then a else b
}

function {:induction false} Min(a: int, b: int): int
{
  if a <= b then a else b
}

// Test 1: bounds + “returns one of the arguments”.
method {:induction false} Test_MaxMin_Bounds(a: int, b: int)
{
  assert
    Max(a, b) >= a &&
    Max(a, b) >= b &&
    Min(a, b) <= a &&
    Min(a, b) <= b &&
    (Max(a, b) == a || Max(a, b) == b) &&
    (Min(a, b) == a || Min(a, b) == b)
  by
  {
    MaxMin_Bounds(a, b);
  }
}

// Test 2: commutativity + idempotence.
method {:induction false} Test_MaxMin_Symmetry(a: int, b: int)
{
  assert
    Max(a, b) == Max(b, a) &&
    Min(a, b) == Min(b, a) &&
    Max(a, a) == a &&
    Min(a, a) == a
  by
  {
    MaxMin_Symmetry(a, b);
  }
}

// Test 3: ordering + sum identity.
method {:induction false} Test_MaxMin_Relation(a: int, b: int)
{
  assert
    Min(a, b) <= Max(a, b) &&
    Max(a, b) + Min(a, b) == a + b
  by
  {
    MaxMin_Relation(a, b);
  }
}

