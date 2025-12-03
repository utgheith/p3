// Define a binary tree
datatype Tree = Leaf(val: int) | Node(left: Tree, val: int, right: Tree)

// Calculate size recursively
function {:induction false} Size(t: Tree): nat {
  match t
  case Leaf(_) => 1
  case Node(l, _, r) => Size(l) + 1 + Size(r)
}

// Flatten to a list (DFS)
function {:induction false} Flatten(t: Tree): seq<int> {
  match t
  case Leaf(v) => [v]
  case Node(l, v, r) => Flatten(l) + [v] + Flatten(r)
}

// Calculate sum of all values in tree recursively
function {:induction false} SumTree(t: Tree): int {
  match t
  case Leaf(v) => v
  case Node(l, v, r) => SumTree(l) + v + SumTree(r)
}

// Calculate sum of a seq
function {:induction false} Sum(s: seq<int>): int {
  if |s| == 0 then 0
  else s[0] + Sum(s[1..])
}

// Test 1: Size of tree == Size of flattened tree
method {:induction false} Test_TreeSize(t: Tree) {
  assert Size(t) == |Flatten(t)| by {
    Prove_TreeSize(t);
  }
}

// Test 2: Sum of tree == Sum of flattened tree
method {:induction false} Test_TreeSum(t: Tree) {
  assert SumTree(t) == Sum(Flatten(t)) by {
    Prove_TreeSum(t);
  }
}
