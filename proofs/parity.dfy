lemma {:induction false} ParityProof(x: nat, y: nat) 
    ensures (isEven(x) || isEven(y)) ==> isEven(x * y)
{
    
}

