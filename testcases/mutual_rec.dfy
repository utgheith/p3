// Mutual recursion testcase for addition
function {:induction false} f(n: nat, m: nat): nat
  decreases n, m
{
    if n == 0 then m else 1 + g(n - 1, m)
}

function {:induction false} g(n: nat, m: nat): nat
  decreases n, m
{
    if m == 0 then n else 1 + f(n, m - 1)
}

method MutualRecTest(n: nat, m: nat)
{
    assert f(n, m) == n + m by {
        FEqualsSum(n, m);
    }

    assert g(n, m) == n + m by {
        GEqualsSum(n, m);
    }
}
