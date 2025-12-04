lemma {:induction false} ParityProof(x: nat, y: nat) 
    requires isEven(x) || isEven(y)
    ensures isEven(x * y)
    decreases x + y
{
    
}

