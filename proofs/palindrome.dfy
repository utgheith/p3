// Proofs for Palindrome.dfy

// Lemma 1:
// If a is a palindrome, then either its length is <= 1
// or its first and last elements are equal.
lemma {:induction false} Palindrome_FirstLast(a: seq<int>)
  ensures IsPalindrome(a) ==> (|a| <= 1 || a[0] == a[|a| - 1])
{
  // TODO replace this with a real proof.
  assert false;
}

// Lemma 2:
// If a is a palindrome and has length > 1, then its middle part
// (drop first and last) is also a palindrome.
lemma {:induction false} Palindrome_Middle(a: seq<int>)
  ensures IsPalindrome(a) && |a| > 1 ==> IsPalindrome(a[1 .. |a| - 1])
{
  // TODO replace this with a real proof.
  assert false;
}

