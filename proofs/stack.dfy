ghost function {:induction false} AbstractView(stack: StackNode): seq<int>
{
    match stack
    case EmptyStack => []
    case StackElem(v, next) => [v] + AbstractView(next)
}

ghost function {:induction false} NodeCount(stack: StackNode): nat
{
    match stack
    case EmptyStack => 0
    case StackElem(_, next) => 1 + NodeCount(next)
}

ghost predicate {:induction false} ValidStack(stack: StackNode)
{
    NodeCount(stack) == |AbstractView(stack)|
}

function {:induction false} BuildStack(values: seq<int>): StackNode
    decreases |values|
{
    if |values| == 0 then EmptyStack
    else StackElem(values[0], BuildStack(values[1..]))
}

lemma {:induction false} NodeCountMatchesAbstractView(stack: StackNode)
    ensures ValidStack(stack)
{
    match stack
    case EmptyStack => {}
    case StackElem(v, next) => {
        NodeCountMatchesAbstractView(next);
        calc {
            NodeCount(stack);
        ==
            1 + NodeCount(next);
        ==
            1 + |AbstractView(next)|;
        ==
            |[v] + AbstractView(next)|;
        ==
            |AbstractView(stack)|;
        }
    }
}

lemma {:induction false} PushUpdatesAbstractView(stack: StackNode, value: int)
    ensures AbstractView(Push(stack, value)) == [value] + AbstractView(stack)
{
    calc {
        AbstractView(Push(stack, value));
    ==
        AbstractView(StackElem(value, stack));
    ==
        [value] + AbstractView(stack);
    }
}

lemma {:induction false} ValidStackAfterPush(stack: StackNode, value: int)
    ensures ValidStack(stack)
    ensures ValidStack(Push(stack, value))
{
    NodeCountMatchesAbstractView(stack);
    NodeCountMatchesAbstractView(Push(stack, value));
}

lemma {:induction false} TopMatchesAbstractView(stack: StackNode)
    requires stack.StackElem?
    ensures Top(stack) == AbstractView(stack)[0]
{
    match stack
    case StackElem(v, next) => {
        calc {
            |AbstractView(stack)|;
        ==
            |[v] + AbstractView(next)|;
        ==
            1 + |AbstractView(next)|;
        >
            0;
        }
        calc {
            AbstractView(stack)[0];
        ==
            ([v] + AbstractView(next))[0];
        ==
            v;
        ==
            Top(stack);
        }
    }
}

lemma {:induction false} PopUpdatesAbstractView(stack: StackNode)
    requires stack.StackElem?
    ensures AbstractView(Pop(stack)) == AbstractView(stack)[1..]
{
    match stack
    case StackElem(v, next) => {
        calc {
            AbstractView(Pop(stack));
        ==
            AbstractView(next);
        ==
            ([v] + AbstractView(next))[1..];
        ==
            AbstractView(stack)[1..];
        }
    }
}

lemma {:induction false} ValidStackAfterPop(stack: StackNode)
    requires stack.StackElem?
    ensures ValidStack(stack)
    ensures ValidStack(Pop(stack))
{
    NodeCountMatchesAbstractView(stack);
    NodeCountMatchesAbstractView(Pop(stack));
}

lemma {:induction false} PopPushInverse(stack: StackNode, value: int)
    ensures Pop(Push(stack, value)) == stack
    ensures AbstractView(Pop(Push(stack, value))) == AbstractView(stack)
{
    calc {
        Pop(Push(stack, value));
    ==
        Pop(StackElem(value, stack));
    ==
        stack;
    }
    PopUpdatesAbstractView(Push(stack, value));
    PushUpdatesAbstractView(stack, value);
    calc {
        AbstractView(Pop(Push(stack, value)));
    ==
        AbstractView(Push(stack, value))[1..];
    ==
        ([value] + AbstractView(stack))[1..];
    ==
        AbstractView(stack);
    }
}

lemma {:induction false} BuildStackAbstractView(values: seq<int>)
    ensures AbstractView(BuildStack(values)) == values
    decreases |values|
{
    if |values| == 0 {
    } else {
        BuildStackAbstractView(values[1..]);
        calc {
            AbstractView(BuildStack(values));
        ==
            AbstractView(StackElem(values[0], BuildStack(values[1..])));
        ==
            [values[0]] + AbstractView(BuildStack(values[1..]));
        ==
            [values[0]] + values[1..];
        ==
            values;
        }
    }
}

lemma {:induction false} BuildStackIsValid(values: seq<int>)
    ensures ValidStack(BuildStack(values))
{
    NodeCountMatchesAbstractView(BuildStack(values));
}

lemma {:induction false} StackProof(values: seq<int>)
{
    var stack := BuildStack(values);
    NodeCountMatchesAbstractView(stack);
    BuildStackAbstractView(values);
    BuildStackIsValid(values);

    if stack.StackElem? {
        TopMatchesAbstractView(stack);
        PopUpdatesAbstractView(stack);
        ValidStackAfterPop(stack);
    }

    var pushed := Push(stack, 0);
    ValidStackAfterPush(stack, 0);
    TopMatchesAbstractView(pushed);
    PopPushInverse(stack, 0);
}
