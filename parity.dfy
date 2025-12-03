predicate {:induction false} isEven(n: nat)
{
    if n == 0 then true 
    else if n == 1 then false 
    else isEven(n - 2)
}

method {:induction false} ParityTest(x: nat, y: nat) {
    assert (isEven(x) || isEven(y)) ==> isEven(x * y) by {
        ParityProof(x, y);
    }
}