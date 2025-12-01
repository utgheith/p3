lemma {:induction false} ExpPositiveProof(a: nat, x: nat)
    requires a > 0
    requires x >= 0
    ensures exp(a, x) > 0
    decreases x
{

}

lemma {:induction false} ExpIncreasingProof(a: nat, x: nat)
    requires a > 1
    ensures exp(a, x) < exp(a, x + 1)
    decreases x
{

}

lemma {:induction false} ExpAddProof(a: nat, x: nat, y: nat)
    requires a != 0 || (x != 0 && y != 0)
    ensures exp(a, x) * exp(a, y) == exp(a, x + y)
    decreases y
{

}

lemma {:induction false} ExpMulProof(a: nat, b: nat, x: nat)
    requires (a != 0 || x != 0) && (b != 0 || x != 0)
    ensures exp(a, x) * exp(b, x) == exp(a * b, x)
    decreases x
{

}
