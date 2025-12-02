// For Learning the Basics of Sets

function {:induction false} TopDown (x : int) : set<int> {
    if (x <= 0) then {} else TopDown(x - 1) + {x}
}

function {:induction false} BottomUp (x : int, y : int) : set<int> {
    if (x <= 0) then {} else BottomUp(x - 1, y) + {y - x + 1}
}

method SetTest(x: int) {
  assert TopDown(x) == BottomUp(x, x) by {
    setEquivalenceProof(x);
  }
}
