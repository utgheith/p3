// Proof that a min heap binary tree implementation successfully implements its spec

// --------- Min Heap Specification ---------

// Written as predicates that check the output instead of functions that produce the output
// because it's surprising hard to find the min of a multiset in a function :(

// Size should give the size of the heap
ghost predicate CheckSizeSpec(heap: multiset<int>, result: int)
{
    result == |heap|
}

// Insert should add an element to the heap
ghost predicate CheckInsertSpec(heap: multiset<int>, x: int, result : multiset<int>)
{
    result == heap + multiset{x}
}

// Top should return the min element of the heap
ghost predicate CheckTopSpec(heap: multiset<int>, result: int)
    requires |heap| > 0
{
    forall x :: x in heap ==> result <= x
}

// Pop should remove the min element of the heap
ghost predicate CheckPopSpec(heap: multiset<int>, top: int, result:  multiset<int>)
    requires |heap| > 0
{
    CheckTopSpec(heap, top) && result == heap - multiset{top}
}


// --------- Min Heap Implementation ---------

// Uses a binary tree to organize the elements in descending order

// Every method requires and ensures the "heap invariant", and checks its output against the spec

datatype MinHeap = Nil | HeapNode(val: int, left: MinHeap, right: MinHeap)

// This converts the heap data structure to the theoretical/spec representation so we can check it
ghost function {:induction false} Contents(heap: MinHeap) : multiset<int>
    requires HeapInvariant(heap)

    ensures heap.HeapNode? ==> |Contents(heap)| > 0 && CheckTopSpec(Contents(heap), heap.val) // the root should be the "top"
{
    match heap
    case Nil => multiset{}
    case HeapNode(val, left, right) => multiset{val} + Contents(left) + Contents(right)
    
}

function {:induction false} Size(heap: MinHeap) : nat
    requires HeapInvariant(heap)
    ensures CheckSizeSpec(Contents(heap), Size(heap)) // check against spec
{
    match heap
    case Nil => 0
    case HeapNode(val, left, right) => 1 + Size(left) + Size(right)
}

method {:induction false} Insert(heapIn: MinHeap, x: int) returns (heapOut: MinHeap)
    requires HeapInvariant(heapIn)
    ensures HeapInvariant(heapOut)
    ensures CheckInsertSpec(Contents(heapIn), x, Contents(heapOut)) // check against spec

    ensures heapOut.HeapNode?
    ensures heapOut.val == x || (heapIn.HeapNode? && heapOut.val == heapIn.val)
{
    match heapIn
    case Nil => heapOut := HeapNode(x, Nil, Nil);
    case HeapNode(val, left, right) => {
        var lesser := if x < val then x else val;
        var greater := if x < val then val else x;

        if (Size(left) > Size(right)) {
            var newRight := Insert(right, greater);
            heapOut := HeapNode(lesser, left, newRight);
        } else {
            var newLeft := Insert(left, greater);
            heapOut := HeapNode(lesser, newLeft, right);
        }
    }
}

function {:induction false} Top(heap: MinHeap) : int
    requires HeapInvariant(heap)
    requires Size(heap) > 0
    ensures CheckTopSpec(Contents(heap), Top(heap)) // check against spec
{
    heap.val
}

method {:induction false} Pop(heapIn: MinHeap) returns (heapOut: MinHeap)
    requires HeapInvariant(heapIn)
    requires Size(heapIn) > 0

    ensures HeapInvariant(heapOut)
    ensures CheckPopSpec(Contents(heapIn), Top(heapIn), Contents(heapOut)) // check against spec
{
    if (heapIn.left.Nil?) {
        heapOut := heapIn.right;
    } else if (heapIn.right.Nil?) {
        heapOut := heapIn.left;
    } else if (heapIn.left.val <= heapIn.right.val) {
        var newVal := heapIn.left.val;
        var newLeft := Pop(heapIn.left);
        heapOut := HeapNode(newVal, newLeft, heapIn.right);
    } else {
        var newVal := heapIn.right.val;
        var newRight := Pop(heapIn.right);
        heapOut := HeapNode(newVal, heapIn.left, newRight);
    }
}
