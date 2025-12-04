
function {:induction false} Pow(base: int, exp: nat): int
    decreases exp
{
    if exp == 0 then 1
    else base * Pow(base, exp - 1)
}

// calculates element in row n, index k of Pascal's triangle
function {:induction false} Pascal(n: nat, k: nat): nat
    decreases n, k
{
    if k > n then 0
    else if k == 0 || k == n then 1
    else Pascal(n-1, k-1) + Pascal(n-1, k)
}

// sums row from index 0 to k (inclusive)
function {:induction false} SumPascalRow(n: nat, k: nat): nat
    decreases k
{
    if k == 0 then Pascal(n, 0)
    else Pascal(n, k) + SumPascalRow(n, k-1)
}

method {:induction false} PascalRowSumCheck(n: nat)
{
    assert Pow(2, n) == SumPascalRow(n, n) by {
        PascalRowSumProof(n);
    }
}
