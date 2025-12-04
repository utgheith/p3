include "../testcases/pascal_triangle.dfy"

lemma {:induction false} combination_symmetry(n: nat, k: nat)
    requires k <= n
    ensures combination(n, k) == combination(n, n - k)
{
}