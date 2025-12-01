// Fast exponentiation is recursive (exponentiation by squaring) and should be equal to BasicPow
function {:induction false} BasicPow(base: int, exp: nat): int
  decreases exp
{
  if exp == 0 then 1 else base * BasicPow(base, exp-1)
}

function {:induction false} FastPow(base: int, exp: nat): int
  decreases exp
{
  if exp == 0 then 1
  else if exp % 2 == 0 then
    // even exponent: FastPow(base, exp/2)^2
    FastPow(base, exp / 2) * FastPow(base, exp / 2)
  else
    // odd exponent: FastPow(base, (exp-1)/2)^2 * base
    FastPow(base, (exp - 1) / 2) * FastPow(base, (exp - 1) / 2) * base
}

method FastExpTest(b: int, e: nat)
{
  var r := BasicPow(b,e);
  assert FastPow(b,e) == r by {
    FastPowEqualsBasicPow(b,e);
  }
}
