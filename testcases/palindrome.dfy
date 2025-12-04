// Palindrome.dfy
// Simple palindrome tester on sequences of ints.

// Recursive palindrome tester:
// A sequence is a palindrome if:
// - length 0 or 1: true
// - otherwise: first == last AND the middle part is a palindrome.
predicate {:induction false} IsPalindrome(a: seq<int>)
  decreases |a|
{
  |a| <= 1 ||
  (a[0] == a[|a| - 1] && IsPalindrome(a[1 .. |a| - 1]))
}


// Test 1:
// If a is a palindrome, then either its length is <= 1
// or its first and last elements are equal.
method {:induction false} Test_Palindrome_FirstLast(a: seq<int>)
{
  assert IsPalindrome(a) ==> (|a| <= 1 || a[0] == a[|a| - 1]) by {
    Palindrome_FirstLast(a);
  }
}

// Test 2:
// If a is a palindrome of length > 1, then its "middle"
// (drop first and last element) is also a palindrome.
method {:induction false} Test_Palindrome_Middle(a: seq<int>)
{
  assert IsPalindrome(a) && |a| > 1 ==> IsPalindrome(a[1 .. |a| - 1]) by {
    Palindrome_Middle(a);
  }
}
