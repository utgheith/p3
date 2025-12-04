// Proofs for bello.dfy
lemma {:induction false} binsearchProof(s: seq<int>, t: int)
  requires forall i, j :: 0 <= i < j < |s| ==> s[i] <= s[j]
  requires |s| > 0
  ensures (forall k :: 0 <= k < |s| && s[k] < t ==> binsearch(s, 0, |s|, t) >= k) 
  ensures 0 <= binsearch(s, 0, |s|, t) < |s| 
{
}