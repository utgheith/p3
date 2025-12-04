
lemma {:induction false} PascalRowSumProperty(n: nat, k: nat)
    requires n > 0
    requires k <= n
    ensures SumPascalRow(n, k) == (if k == 0 then 1 else SumPascalRow(n-1, k-1) + SumPascalRow(n-1, k))
    decreases k
{
    
}

lemma {:induction false} PascalRowSumProof(n: nat)
    ensures Pow(2, n) == SumPascalRow(n, n)
    decreases n
{
    
}