// functions and predicates for gcd and divisibility
function {:induction false} gcd(a: nat, b: nat): nat
    decreases b
{
    if b == 0 then a else gcd(b,a % b)
}

predicate {:induction false} div(d: nat, n: nat)
{
    exists k: int :: n == d * k
}

predicate {:induction false} isGcd(d: nat, a: nat, b: nat)
{
    div(d, a) &&
    div(d, b) &&
    // Every common divisor of a and b also divides d.
    (forall e: nat :: div(e, a) && div(e, b) ==> div(e, d))
}

method {:induction false} enforceGcd(a: nat, b: nat)
    requires a > 0 || b > 0
    ensures isGcd(gcd(a, b), a, b)
{
    gcdSatisfiesIsGCD(a, b);
}