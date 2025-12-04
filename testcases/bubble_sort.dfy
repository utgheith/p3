// one pass of bubble sort
ghost function {:induction false} BubblePass(s: seq<int>): seq<int>
    decreases |s|
{
    if |s| <= 1 then s
    else if s[0] > s[1] then [s[1]] + BubblePass([s[0]] + s[2..])
    else [s[0]] + BubblePass(s[1..])
}

// main sort
ghost function {:induction false} Sort(s: seq<int>): seq<int>
{
    SortHelper(s, |s|)
}

// n passes of bubble sort
ghost function {:induction false} SortHelper(s: seq<int>, n: int): seq<int>
    decreases n
{
    if n <= 0 then s // sorted
    else SortHelper(BubblePass(s), n - 1)
}

// test the sort
method SortTest(s: seq<int>) {
    assert |Sort(s)| == |s| &&
           forall i: int, j: int ::
             0 <= i < j < |s| ==> Sort(s)[i] <= Sort(s)[j] by {
        sorting_correctness(s);
    }
}