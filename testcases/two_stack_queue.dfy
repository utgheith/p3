// Two-Stack Queue: Simulating a Queue with Two Stacks
// Proves that FIFO queue behavior can be implemented using two LIFO stacks

// Two-stack queue state
datatype TwoStackQueue<T> = Queue(inbox: seq<T>, outbox: seq<T>)

// Get the logical queue contents (FIFO order)
function {:induction false} QueueContents<T>(q: TwoStackQueue<T>): seq<T>
{
    q.outbox + Reverse(q.inbox)
}

// Reverse a sequence
function {:induction false} Reverse<T>(s: seq<T>): seq<T>
    decreases |s|
{
    if |s| == 0 then []
    else Reverse(s[1..]) + [s[0]]
}

// Enqueue: push to inbox stack
function {:induction false} Enqueue<T>(q: TwoStackQueue<T>, x: T): TwoStackQueue<T>
{
    Queue(q.inbox + [x], q.outbox)
}

// Transfer: move all elements from inbox to outbox (reverses order)
function {:induction false} Transfer<T>(q: TwoStackQueue<T>): TwoStackQueue<T>
    requires |q.outbox| == 0
{
    Queue([], Reverse(q.inbox))
}

// Dequeue: pop from outbox, transfer if needed
function {:induction false} Dequeue<T>(q: TwoStackQueue<T>): (TwoStackQueue<T>, T)
    requires |QueueContents(q)| > 0
{
    if |q.outbox| > 0 then
        (Queue(q.inbox, q.outbox[..|q.outbox|-1]), q.outbox[|q.outbox|-1])
    else
        var q' := Transfer(q);
        (Queue(q'.inbox, q'.outbox[..|q'.outbox|-1]), q'.outbox[|q'.outbox|-1])
}

// Test: Enqueue preserves FIFO semantics
method {:induction false} EnqueueTest<T>(q: TwoStackQueue<T>, x: T)
{
    var q' := Enqueue(q, x);
    assert QueueContents(q') == QueueContents(q) + [x] by {
        EnqueueCorrect(q, x);
    }
}

// Test: Dequeue removes from front
method {:induction false} DequeueTest<T>(q: TwoStackQueue<T>)
    requires |QueueContents(q)| > 0
{
    var (q', x) := Dequeue(q);
    var contents := QueueContents(q);
    assert x == contents[0] by {
        DequeueCorrect(q);
    }
    assert QueueContents(q') == contents[1..] by {
        DequeueCorrect(q);
    }
}

// Test: Transfer maintains queue contents
method {:induction false} TransferTest<T>(q: TwoStackQueue<T>)
    requires |q.outbox| == 0
{
    var q' := Transfer(q);
    assert QueueContents(q') == QueueContents(q) by {
        TransferCorrect(q);
    }
}
