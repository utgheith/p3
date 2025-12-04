function {:induction false} binsearch(s: seq<int>, l: int, r: int, t: int): (result: int)
  requires 0 <= l < r <= |s|
  requires forall i, j :: l <= i < j < r ==> s[i] <= s[j]
  decreases r - l
{
  if r - l <= 1 then
    l
  else
    var m := (l + r) / 2;
    if s[m] > t then
      binsearch(s, l, m, t)
    else
      binsearch(s, m, r, t)
}

method {:induction false} binsearchTest(s: seq<int>, t: int)
  requires forall i, j :: 0 <= i < j < |s| ==> s[i] <= s[j]
  requires |s| > 0
{
  var i := binsearch(s, 0, |s|, t);
  assert 0 <= i < |s| && (forall k :: 0 <= k < |s| && s[k] < t ==> i >= k) by {
    binsearchProof(s, t);
  }
}