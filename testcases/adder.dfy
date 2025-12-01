function {:induction false} fold_right(f : (int, int) -> int, lst: seq<int>, init: int) : int
  decreases |lst|
{
  if |lst| == 0 then
    init
  else
    f(lst[0], fold_right(f, lst[1..], init))
}

function {:induction false} add_all_fold(lst: seq<int>, acc: int) : int
  decreases |lst|
{
  fold_right((x, y) => x + y, lst, acc)
}

function {:induction false} add_all_recursive(lst: seq<int>, acc: int) : int
  decreases |lst|
{
  if |lst| == 0 then
    acc
  else
    add_all_recursive(lst[1..], acc + lst[0])
}

method {:induction false} test_add_all(lst: seq<int>, acc: int) 
{
    assert add_all_fold(lst, acc) == add_all_recursive(lst, acc) by {
        AddAllProof(lst, acc);
    }
}