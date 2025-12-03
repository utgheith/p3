function {:induction false} sum(s: seq<int>, i: int, j: int) : int
  requires 0 <= i < j <= |s|
  decreases j - i
{
  if j - i == 1 then s[i] else s[i] + sum(s, i + 1, j)
}

function {:induction false} max(a: int, b: int): int
  ensures max(a, b) >= a && max(a, b) >= b
{
  if a >= b then a else b
}

function {:induction false} bestEndingAt(s: seq<int>, j: nat) : int
  requires 0 < j <= |s|
  decreases j
{
  if j == 1 then s[0]
  else max(bestEndingAt(s, j - 1) + s[j - 1], s[j - 1])
}

function {:induction false} bestWithinPrefix(s: seq<int>, j: nat) : int
  requires 0 <= j <= |s|
  decreases j
{
  if j == 0 then 0
  else max(bestWithinPrefix(s, j - 1), bestEndingAt(s, j))
}

function {:induction false} kruskal(s: seq<int>) : (ans : int) 
{
  bestWithinPrefix(s, |s|)
}


method {:induction false} KruskalTest(s: seq<int>)
  requires |s| > 0 
{
  assert forall i :: forall j :: 0 <= i < j <= |s| ==> kruskal(s) >= sum(s, i, j) by {
    KruskalProof(s);
  }
}