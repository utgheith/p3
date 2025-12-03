// divisibility lemmas
// a|0
lemma {:induction false} Div0(d: nat)
    ensures div(d, 0)
{
    assert 0 == d * 0;
}

// a|a
lemma {:induction false} DivN(n: nat)
    ensures div(n, n)
{
    assert n == n * 1;
}

// d|a & d|b ==> d|(a % b)
lemma {:induction false} DivMod(d: nat, a: nat, b: nat)
    requires b > 0
    requires div(d, a) && div(d, b)
    ensures div(d, a % b)
{
    var k1: int :| a == d * k1;
    var k2: int :| b == d * k2;

    calc {
        a % b;
        == a - (a / b) * b;
        == d * k1 - (a / b) * (d * k2);
        == d * k1 - d * ((a / b) * k2);
        == d * (k1 - (a / b) * k2);
    }
    assert div(d, a % b);
}

// d|a & d|(a % b) ==> d|a
lemma {:induction false} DivR(d: nat, a: nat, b: nat)
    requires b > 0
    requires div(d, b) && div(d, a % b)
    ensures div(d, a)
{
    var k1: int :| b == d * k1;
    var k2: int :| a % b == d * k2;
    calc {
        a;
        == (a / b) * b + a % b;
        == (a / b) * (d * k1) + d * k2;
        == d * ((a / b) * k1) + d * k2;
        == d * ((a / b) * k1 + k2);
    }
    assert div(d, a); 
}

// gcd lemmas

//asserts euclid's algorithm satisfies the isGCD predicate
lemma {:induction false} euclidStepPreservesIsGCD(a: nat, b: nat, d: nat)
    requires b > 0
    requires isGcd(d, b, a % b)
    ensures isGcd(d, a, b)
{
    assert div(d, b);
    assert div(d, a % b);

    // d|a
    DivR(d, a, b);

    // Now we need to show d is maximal
    forall e: nat 
        ensures div(e, a) && div(e, b) ==> div(e, d)
    {
        assume div(e, a) && div(e, b);
        // e divides a % b
        DivMod(e, a, b);
        // e divides d
        assert div(e, d);
    }
}

// gcd satisfies the isGCD predicate
lemma {:induction false} gcdSatisfiesIsGCD(a: nat, b: nat)
    ensures isGcd(gcd(a, b), a, b)
    decreases b
{
    if b == 0 {
        // base case
        assert gcd(a, 0) == a;
        DivN(a);
        Div0(a);
        forall e: nat
            ensures div(e, a) && div(e, 0) ==> div(e, gcd(a, 0))
        {
            assume div(e, a) && div(e, 0);
            DivN(e);
            Div0(e);
            assert div(e, gcd(a, 0));
        }
    } else {
        // inductive case
        gcdSatisfiesIsGCD(b, a % b);
        assert gcd(a, b) == gcd(b, a % b);
        assert isGcd(gcd(b, a % b), b, a % b);
        euclidStepPreservesIsGCD(a, b, gcd(b, a % b));
    }
}

// gcd is non-zero if either a or b is non-zero
lemma {:induction false} gcdNonZero(a: nat, b: nat)
    requires a > 0 || b > 0
    ensures gcd(a, b) > 0
    decreases b
{
    if b == 0 {
        assert gcd(a, 0) == a;
        assert a > 0;
    } else {
        gcdSatisfiesIsGCD(b, a % b);
        assert gcd(a, b) == gcd(b, a % b);
        gcdNonZero(b, a % b);
    }
}

// gcd divides a and b
lemma {:induction false} gcdDivides(a: nat, b: nat)
    ensures div(gcd(a, b), a) && div(gcd(a, b), b)
{
    gcdSatisfiesIsGCD(a, b);
}

// gcd is maximal
// (techically gcd(0, 0) is undefined, but we disregard that edge case)
lemma {:induction false} gcdMaximal(d: nat, a: nat, b: nat)
    requires a > 0 || b > 0
    requires div(d, a) && div(d, b)
    ensures d <= gcd(a, b)
{
    gcdSatisfiesIsGCD(a, b);
    assert div(d, gcd(a, b));
    gcdNonZero(a, b);
    assert gcd(a, b) > 0 || d == 0;
    // since d divides gcd(a,b) and gcd(a,b) > 0, we
    assert d <= gcd(a, b);
}

// asserts gcd(a, b) == gcd(b, a)
lemma {:induction false} gcdSymmetric(a: nat, b: nat)
    ensures gcd(a, b) == gcd(b, a)
    decreases b
{
    gcdSatisfiesIsGCD(a, b);
    gcdSatisfiesIsGCD(b, a);
}

// asserts the uniqueness of the gcd
// (if a or b is non-zero)
lemma {:induction false} gcdUnique(d1: nat, d2: nat, a: nat, b: nat)
    requires a > 0 || b > 0
    requires isGcd(d1, a, b)
    requires isGcd(d2, a, b)
    ensures d1 == d2
{
    gcdSatisfiesIsGCD(a, b);
    assert div(d1, gcd(a, b));
    assert div(d2, gcd(a, b));
    gcdNonZero(a, b);
    assert gcd(a, b) > 0 || d1 == 0;
    assert gcd(a, b) > 0 || d2 == 0;
    assert d1 <= gcd(a, b);
    assert d2 <= gcd(a, b);
    assert d1 >= gcd(a, b);
    assert d2 >= gcd(a, b);
    assert d1 == d2;
}