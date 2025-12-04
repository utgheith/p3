// For Learning the Basics of Sets

function {:induction false} TopDown (x : nat) : set<nat> 
{
    if (x == 0) then {} else TopDown(x - 1) + {x}
}

function {:induction false} BottomUp (x : nat, y : nat) : set<nat> 
{
    if (x == 0) then {} else BottomUp(x - 1, y) + {y - x + 1}
}

method {:induction false} SetTest(x: nat) 
{
  assert TopDown(x) == BottomUp(x, x) by {
    TopDownBottomUpEquivalence(x);
  }
}
