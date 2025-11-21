datatype StackNode = EmptyStack | StackElem(value: int, next: StackNode)

function {:induction false} Push(stack: StackNode, x: int): StackNode
{
    StackElem(x, stack)
}

function {:induction false} Pop(stack: StackNode): StackNode
    requires stack.StackElem?
{
    stack.next
}

function {:induction false} Top(stack: StackNode): int
    requires stack.StackElem?
{
    stack.value
}

method StackTest(values: seq<int>) {
    StackProof(values);
}
