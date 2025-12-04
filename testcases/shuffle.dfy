ghost function Count<T>(s: seq<T>, x: T): nat {
  if |s| == 0 then 0
  else (if s[0] == x then 1 else 0) + Count(s[1..], x)
}

ghost predicate IsPermutation<T(!new)>(a: seq<T>, b: seq<T>) {
  |a| == |b| && forall x :: Count(a, x) == Count(b, x)
}

ghost function Swap<T>(s: seq<T>, i: nat, j: nat): seq<T>
  requires i < |s| && j < |s|
{
  s[i := s[j]][j := s[i]]
}


method Shuffle(a: array<int>)
  modifies a
  ensures IsPermutation(old(a[..]), a[..])
{
  var i := a.Length;
  while i > 1
    invariant IsPermutation(old(a[..]), a[..])
    decreases i
  {
    var j := Random(i);
    i := i - 1;
    
    assert IsPermutation(old(a[..]), Swap(a[..], i, j)) by {
      SwapIsPermutation(a[..], i, j);
    }

    // Since we cannot assign to arrays directly, we perform the swap manually
    a[i], a[j] := a[j], a[i];

  }
}

method {:axiom} Random(n: nat) returns (r: nat)
  requires n > 0
  ensures 0 <= r < n