// Recursive Fibonacci implementation
function RecFib(n: nat): nat
{
  if n == 0 then 0
  else if n == 1 then 1
  else RecFib(n-1) + RecFib(n-2)
}


// IterFib requires a helper that simulates the usage of a loop, but can't actually 
// use loops directly due to Dafny's restrictions on functions.
function IterFib(n: nat): nat
{
  IterativeFibHelper(n, 0, 1, 0)
}

function IterativeFibHelper(n: nat, a: nat, b: nat, i: nat): nat
  decreases n - i
{
  if i >= n then a
  else IterativeFibHelper(n, b, a + b, i + 1)
}

method FibonacciTest(n: nat)
{
  var r := IterFib(n);
  assert RecFib(n) == r by {
    IterEqualsRec(n);
  }
}