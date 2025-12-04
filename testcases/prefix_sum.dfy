// --- spec used in proofs ---------------------------------------------------
function {:induction false} sum_seq(s: seq<int>): int
  decreases s
{
  if |s| == 0 then 0 else s[0] + sum_seq(s[1..])
}
