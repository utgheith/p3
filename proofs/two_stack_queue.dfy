lemma {:induction false} EnqueueCorrect<T>(q: TwoStackQueue<T>, x: T)
    ensures QueueContents(Enqueue(q, x)) == QueueContents(q) + [x]
{

}

lemma {:induction false} DequeueCorrect<T>(q: TwoStackQueue<T>)
    requires |QueueContents(q)| > 0
    ensures var (q', x) := Dequeue(q);
            x == QueueContents(q)[0] && QueueContents(q') == QueueContents(q)[1..]
{

}

lemma {:induction false} TransferCorrect<T>(q: TwoStackQueue<T>)
    requires |q.outbox| == 0
    ensures QueueContents(Transfer(q)) == QueueContents(q)
{

}
