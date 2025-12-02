// For Learning the Basics of Sets

function {:induction false} TopDown (x : int) : set<int> 
  requires x >= 0
{
    if (x == 0) then {} else TopDown(x - 1) + {x}
}

function {:induction false} BottomUp (x : int, y : int) : set<int> 
  requires x >= 0
{
    if (x == 0) then {} else BottomUp(x - 1, y) + {y - x + 1}
}

method SetTest(x: int) 
  requires x >= 0
{
  assert TopDown(x) == BottomUp(x, x) by {
    TopDownBottomUpEquivalence(x);
  }
}
