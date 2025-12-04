function {:induction false} Reverse(s: seq<int>): seq<int> {
    if |s| == 0 then []
    else Reverse(s[1..]) + [s[0]]
}

method {:induction false} DoubleReverseTest(s: seq<int>) {
    assert Reverse(Reverse(s)) == s by {
        DoubleReverseLemma(s);
    }
}
