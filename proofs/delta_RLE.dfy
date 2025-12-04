// Delta round-trip

lemma {:induction false} DeltaRoundTrip(s: seq<int>)
    ensures DeltaDecode(DeltaEncode(s)) == s
    decreases |s|
{
    if empty(s) {
    } else if |s| == 1 {
    } else {
        var prefix := s[..|s| - 1];
        DeltaRoundTrip(prefix);

        var d := DeltaEncode(s);
        assert d == DeltaEncode(prefix) + [s[|s| - 1] - s[|s| - 2]];
        assert d[..|d| - 1] == DeltaEncode(prefix);
        assert DeltaDecode(d[..|d| - 1]) == prefix;

        assert prefix[|prefix| - 1] == s[|s| - 2];
        assert d[|d| - 1] == s[|s| - 1] - s[|s| - 2];
        assert prefix[|prefix| - 1] + d[|d| - 1] == s[|s| - 1];

        assert DeltaDecode(d) ==
            prefix + [prefix[|prefix| - 1] + d[|d| - 1]];
        assert DeltaDecode(d) == s;
    }
}

// RLE round-trip

lemma {:induction false} ExpandRunSucc(v: int, c: nat)
    ensures ExpandRun((v, c + 1)) == ExpandRun((v, c)) + [v]
    decreases c
{
    if c == 0 {
    } else {
        ExpandRunSucc(v, c - 1);
    }
}

lemma {:induction false} RLERoundTrip(s: seq<int>)
    ensures RunLengthDecode(RunLengthEncode(s)) == s
    decreases |s|
{
    if empty(s) {
    } else if |s| == 1 {
        assert [(s[0], 1)] == RunLengthEncode(s);
        assert RunLengthDecode([(s[0], 1)]) == ExpandRun((s[0], 1));
        assert ExpandRun((s[0], 1)) == [s[0]];        
    } else {
        var prefix := s[..|s| - 1];
        var lastVal := s[|s| - 1];
        RLERoundTrip(prefix);

        var encPrefix := RunLengthEncode(prefix);
        var enc := RunLengthEncode(s);
        var lastRunPrefix := encPrefix[|encPrefix| - 1];

        // Cases on whether lastVal extends the last run or starts a new run
        if lastRunPrefix.0 == lastVal {
            // encPrefix = runsPrefix + [(lastVal, c)]
            var runsPrefix := encPrefix[..|encPrefix| - 1];
            var c := lastRunPrefix.1;
            assert encPrefix == runsPrefix + [(lastVal, c)];
            // enc = runsPrefix + [(lastVal, c+1)]
            assert enc == runsPrefix + [(lastVal, c + 1)];

            // Decode encPrefix and enc via their last run
            assert RunLengthDecode(encPrefix) ==
                     RunLengthDecode(runsPrefix) + ExpandRun((lastVal, c));
            assert RunLengthDecode(enc) ==
                     RunLengthDecode(runsPrefix) + ExpandRun((lastVal, c + 1));

            // Inductive hypothesis on prefix
            assert RunLengthDecode(encPrefix) == prefix;
            assert RunLengthDecode(runsPrefix) + ExpandRun((lastVal, c)) == prefix;
            ExpandRunSucc(lastVal, c);
            assert RunLengthDecode(enc) == prefix + [lastVal];
            assert RunLengthDecode(enc) == s;
        } else {
            // enc = encPrefix + [(lastVal,1)]
            assert enc == encPrefix + [(lastVal, 1)];
            // Decode
            assert RunLengthDecode(enc) ==
                RunLengthDecode(encPrefix) + ExpandRun((lastVal, 1));
            assert RunLengthDecode(encPrefix) == prefix;
            assert ExpandRun((lastVal, 1)) == [lastVal];
            assert RunLengthDecode(enc) == prefix + [lastVal];
            assert RunLengthDecode(enc) == s;
        }
    }
}

// Delta + RLE pipeline

// Encode a nonempty sequence: store the first element and RLE of the delta tail.
function {:induction false} DeltaRLEEncode(s: seq<int>) : (int, seq<(int, nat)>)
    requires !empty(s)
{
    var d := DeltaEncode(s);
    var head := d[0];
    var tail := d[1..];
    (head, RunLengthEncode(tail))
}

// Decode a (head, runs) pair back to the original sequence.
function {:induction false} DeltaRLEDecode(p: (int, seq<(int, nat)>)) : seq<int>
{
    var head := p.0;
    var runs := p.1;
    var tail := RunLengthDecode(runs);
    var d := [head] + tail;
    DeltaDecode(d)
}

lemma {:induction false} DeltaRLERoundTrip(s: seq<int>)
    requires !empty(s)
    ensures DeltaRLEDecode(DeltaRLEEncode(s)) == s
{
    // Show delta round-trip
    var d := DeltaEncode(s);
    DeltaRoundTrip(s);
    assert DeltaDecode(d) == s;

    // Show RLE round-trip on delta tail
    var head := d[0];
    var tail := d[1..];
    var runs := RunLengthEncode(tail);
    RLERoundTrip(tail);
    assert RunLengthDecode(runs) == tail;

    // Combine to show DeltaRLE round-trip
    assert DeltaRLEEncode(s) == (head, runs);
    assert DeltaRLEDecode(DeltaRLEEncode(s)) ==
             DeltaDecode([head] + RunLengthDecode(runs));
    assert DeltaRLEDecode(DeltaRLEEncode(s)) == DeltaDecode([head] + tail);
    assert [head] + tail == d;
    assert DeltaRLEDecode(DeltaRLEEncode(s)) == s;
}
