
lemma {:induction false} recurOnJ(s: seq<int>, i: int, j: int) 
  requires 0 <= i < j <= |s|
  requires j - i > 1
  decreases j - i
  ensures sum(s, i, j) == sum(s, i, j - 1) + s[j - 1]
{
  if (j - i > 2) {
    recurOnJ(s, i + 1, j);
  }
}

lemma {:induction false} FixedKruskalProof(s: seq<int>, j: nat) 
  requires 0 < j <= |s|
  ensures forall i :: 0 <= i < j <= |s| ==> bestEndingAt(s, j) >= sum(s, i, j)
{
  if (forall i :: 0 <= i < j <= |s| ==> bestEndingAt(s, j) >= sum(s, i, j)) {

  } else {
    if (j == 1) {
      assert bestEndingAt(s, j) == s[0];
    } else {
      FixedKruskalProof(s, j - 1); 
      forall i | 0 <= i < j
        ensures sum(s, i, j) == if (i == j - 1) then s[i] else sum(s, i, j - 1) + s[j - 1]
      {
        if i != j - 1 {
          recurOnJ(s, i, j);
        }
      }
    }
  }
}

lemma {:induction false} PrefixKruskalProof(s: seq<int>, j: nat, p: nat) 
  requires 0 < j <= p <= |s|
  ensures forall i :: 0 <= i < j ==> bestWithinPrefix(s, p) >= sum(s, i, j);
{
  if (j == p) {
    assert bestWithinPrefix(s, p) >= bestEndingAt(s, j);
  } else {
    PrefixKruskalProof(s, j, p - 1);
  }
  FixedKruskalProof(s, j);
}

lemma {:induction false} KruskalProof(s: seq<int>) 
  ensures forall i :: forall j :: 0 <= i < j <= |s| ==> kruskal(s) >= sum(s, i, j)
{
  forall j | 0 < j <= |s| {
    PrefixKruskalProof(s, j, |s|);
  }
}