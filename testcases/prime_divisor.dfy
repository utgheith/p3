ghost function {:induction false} Divides(n1: nat, n2: nat) : bool {
    exists k: nat :: n1 * k == n2
}

ghost function {:induction false} Prime(p: nat) : bool {
    p >= 2 && forall n: nat :: n > 1 && n < p ==> !Divides(n, p)
}

method {:induction false} PrimeTest(n: nat) {
    assert n > 1 ==> exists p :: Prime(p) && Divides(p, n) by {
        if n <= 1 {

        } else {
            PrimeDivisorLemma(n);
        }
    }
}
