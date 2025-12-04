function {:induction false} combination(n: nat, k: nat): (result: nat)
    requires k <= n
    decreases n, k
{
    if k == 0 || k == n then 1
    else if k > n then 0
    else combination(n - 1, k - 1) + combination(n - 1, k)
}