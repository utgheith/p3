predicate empty<T>(s: seq<T>)
{
    |s| == 0
}

function {:induction false} DeltaEncode(s: seq<int>) : seq<int>
    decreases |s|
{
    if empty(s) then []
    else if |s| == 1 then [s[0]]
    else
        var prefix := DeltaEncode(s[..|s| - 1]);
        prefix + [s[|s| - 1] - s[|s| - 2]]
}

function {:induction false} DeltaDecode(s: seq<int>) : seq<int>
    decreases |s|
{
    if empty(s) then []
    else if |s| == 1 then [s[0]]
    else
        var prefix := DeltaDecode(s[..|s| - 1]);
        prefix + [prefix[|prefix| - 1] + s[|s| - 1]]
}

function {:induction false} ExpandRun(s: (int, nat)) : seq<int>
    decreases s.1
{
    if s.1 == 0 then []
    else [s.0] + ExpandRun((s.0, s.1 - 1))
}

function {:induction false} RunLengthEncode(s: seq<int>) : seq<(int, nat)>
    decreases |s|
{
    if empty(s) then []
    else if |s| == 1 then [(s[0], 1)]
    else
        var prefix := RunLengthEncode(s[..|s| - 1]);
        var last := prefix[|prefix| - 1];
        if last.0 == s[|s| - 1] then
            prefix[..|prefix| - 1] + [(last.0, last.1 + 1)]
        else
            prefix + [(s[|s| - 1], 1)]
}

function {:induction false} RunLengthDecode(s: seq<(int, nat)>) : seq<int>
    decreases |s|
{
    if empty(s) then []
    else
        var prefix := RunLengthDecode(s[..|s| - 1]);
        prefix + ExpandRun(s[|s| - 1])
}

method {:induction false} CompressionValidation(xs: seq<int>)
    requires !empty(xs)
{
    // Delta round-trip on full sequence
    var d := DeltaEncode(xs);
    assert DeltaDecode(d) == xs by {
        DeltaRoundTrip(xs);
    }

    // RLE round-trip on original sequence
    var rle := RunLengthEncode(xs);
    assert RunLengthDecode(rle) == xs by {
        RLERoundTrip(xs);
    }

    // RLE round-trip on delta tail
    var tail := d[1..];
    var runsTail := RunLengthEncode(tail);
    assert RunLengthDecode(runsTail) == tail by {
        RLERoundTrip(tail);
    }

    // Full delta+RLE pipeline round-trip
    var enc := DeltaRLEEncode(xs);
    assert DeltaRLEDecode(enc) == xs by {
        DeltaRLERoundTrip(xs);
    }
}