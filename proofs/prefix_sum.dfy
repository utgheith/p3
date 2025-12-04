include "../testcases/prefix_sum.dfy"

// --- tiny helpers ----------------------------------------------------------

lemma sum_seq_append(a: seq<int>, b: seq<int>)
  ensures sum_seq(a + b) == sum_seq(a) + sum_seq(b)
  decreases a
{

}


// snoc means appending one element to the end of a sequence
// prefix[0..i+1] = prefix[0..i] + [arr[i]]
// use the append lemma above
lemma sum_seq_prefix_snoc(arr: seq<int>, i: int)
  requires 0 <= i < |arr|
  ensures  sum_seq(arr[0..i+1]) == sum_seq(arr[0..i]) + arr[i]
{

}

// Write out the algorithm for a prefix sum. Use the lemma above
// to prove that your algorithm creates the next element in the prefix sum.

method prefix_sum_impl(arr: seq<int>) returns (out: seq<int>)
  ensures |out| == |arr|
  ensures forall i :: 0 <= i < |arr| ==> out[i] == sum_seq(arr[0..i+1])
{

}