lemma {:induction false} sorting_correctness(s: seq<int>)
    ensures |Sort(s)| == |s| &&
            forall i: int, j: int :: 0 <= i < j < |Sort(s)| ==> Sort(s)[i] <= Sort(s)[j]
    decreases |s|
{

}

lemma {:induction false} bubble_pass_preserves_length(s: seq<int>)
    ensures |BubblePass(s)| == |s|
    decreases |s|
{

}

lemma {:induction false} sort_helper_preserves_length(s: seq<int>, n: int)
    requires 0 <= n
    ensures |SortHelper(s, n)| == |s|
    decreases n
{

}

lemma {:induction false} bubble_pass_preserves_multiset(s: seq<int>)
    ensures (forall x :: x in multiset(s) ==> x in multiset(BubblePass(s))) &&
            (forall x :: x in multiset(BubblePass(s)) ==> x in multiset(s))
    decreases |s|
{

}

lemma {:induction false} bubble_pass_elements_from_original(s: seq<int>, i: int)
    requires 0 <= i < |BubblePass(s)|
    ensures BubblePass(s)[i] in multiset(s)
{

}

lemma {:induction false} element_in_multiset_bounded_by_max(s: seq<int>, elem: int)
    requires elem in multiset(s)
    requires forall i :: 0 <= i < |s| - 1 ==> s[i] <= s[|s| - 1]
    ensures elem <= s[|s| - 1]
{

}

lemma {:induction false} bubble_pass_moves_max_to_end(s: seq<int>)
    requires |s| > 0
    ensures forall i :: 0 <= i < |BubblePass(s)| - 1 ==> BubblePass(s)[i] <= BubblePass(s)[|BubblePass(s)| - 1]
    decreases |s|
{

}

lemma {:induction false} bubble_pass_preserves_max_at_end(s: seq<int>)
    requires |s| > 0
    requires forall i :: 0 <= i < |s| - 1 ==> s[i] <= s[|s| - 1]
    ensures BubblePass(s)[|BubblePass(s)| - 1] == s[|s| - 1]
    decreases |s|
{

}

lemma {:induction false} bubble_pass_prefix_when_max_at_end(s: seq<int>)
    requires |s| > 1
    requires forall i :: 0 <= i < |s| - 1 ==> s[i] <= s[|s| - 1]
    ensures BubblePass(s)[..|BubblePass(s)|-1] == BubblePass(s[..|s|-1])
    decreases |s|
{

}

lemma {:induction false} sort_helper_produces_sorted_sequence(s: seq<int>, n: int)
    requires 0 <= n
    requires n >= |s|
    ensures forall i: int, j: int :: 0 <= i < j < |SortHelper(s, n)| ==> SortHelper(s, n)[i] <= SortHelper(s, n)[j]
    decreases n, |s|
{

}

lemma {:induction false} sort_helper_decomposes_with_max(s: seq<int>, n: int)
    requires 0 <= n
    requires |s| > 0
    requires forall i :: 0 <= i < |s| - 1 ==> s[i] <= s[|s| - 1]
    ensures SortHelper(s, n) == SortHelper(s[..|s|-1], n) + [s[|s| - 1]]
    decreases n
{

}

lemma {:induction false} sort_helper_preserves_upper_bound(s: seq<int>, n: int, bound: int)
    requires 0 <= n
    requires forall i :: 0 <= i < |s| ==> s[i] <= bound
    ensures forall i :: 0 <= i < |SortHelper(s, n)| ==> SortHelper(s, n)[i] <= bound
    decreases n
{

}

