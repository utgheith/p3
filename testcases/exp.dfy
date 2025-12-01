// Exponentiation: exp(a, b) = a^b
function {:induction false} exp(base: nat, exponent: nat): nat
    requires exponent >= 0
    requires base != 0 || exponent != 0
{
    if base == 0 then 0
    else if exponent == 0 then 1
    else base * exp(base, exponent - 1)
}

// Proofs:

// a^x > 0 for a > 0
method {:induction false} ExpPositiveTest(a: nat, x: nat)
    requires a > 0
    requires x >= 0
{
    assert exp(a, x) > 0 by {
        ExpPositiveProof(a, x);
    }
}

// a^x increases for a > 1 for increasing x.
method {:induction false} ExpIncreasingTest(a: nat, x: nat)
    requires a > 1
{
    assert exp(a, x) < exp(a, x + 1) by {
        ExpIncreasingProof(a, x);
    }
}

// a^x * a^y = a^(x + y)
method {:induction false} ExpAddTest(a: nat, x: nat, y: nat)
    requires a != 0 || (x != 0 && y != 0)
{
    assert exp(a, x) * exp(a, y) == exp(a, x + y) by {
        ExpAddProof(a, x, y);
    }
}

// a^x * b^x = (a * b)^x
method {:induction false} ExpMulTest(a: nat, b: nat, x: nat)
    requires (a != 0 || x != 0) && (b != 0 || x != 0)
{
    assert exp(a, x) * exp(b, x) == exp(a * b, x) by {
        ExpMulProof(a, b, x);
    }
}
