function {:induction false} Sum(n: nat): nat
    decreases n
{
    if n == 0 then 0
    else n + Sum(n - 1)
}

function {:induction false} SumFormula(n: nat): nat
{
    n * (n + 1) / 2
}

lemma {:induction false} SumEqualsFormula(n: nat)
    ensures Sum(n) == SumFormula(n)
{
    proof(n);
}

