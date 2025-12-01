// Interpreter/Compiler for simple expression language
// transpiled from http://adam.chlipala.net/cpdt/html/Cpdt.StackMachine.html
// This was vibe-coded with the help of copilot

datatype exp = Const (v: int)
          | Plus (e1: exp, e2: exp)
          | Times (e1: exp, e2: exp)
datatype Maybe = Nothing | Just (s : seq<int>)

function {:induction false} expDenote(e: exp): int
{
    match e
    case Const(v) => v
    case Plus(e1, e2) => expDenote(e1) + expDenote(e2)
    case Times(e1, e2) => expDenote(e1) * expDenote(e2)
}

datatype instr = IConst (v: int)
               | IPlus
               | ITimes

function {:induction false} stackInterp(instrs: seq<instr>, stack: seq<int>): Maybe
{
    if |instrs| == 0 then Just(stack)
    else
        var first := instrs[0];
        var rest := instrs[1..];
        match first
        case IConst(v) =>
            stackInterp(rest, [v] + stack)
        case IPlus =>
            if |stack| < 2 then Nothing
            else
                var v1 := stack[0];
                var v2 := stack[1];
                stackInterp(rest, [v1 + v2] + stack[2..])
        case ITimes =>
            if |stack| < 2 then Nothing
            else
                var v1 := stack[0];
                var v2 := stack[1];
                stackInterp(rest, [v1 * v2] + stack[2..])
}

function {:induction false} compile(e : exp) : seq<instr>
{
    match e
    case Const(v) => [IConst(v)]
    case Plus(e1, e2) => compile(e2) + compile(e1) + [IPlus]
    case Times(e1, e2) => compile(e2) + compile(e1) + [ITimes]
}

method {:induction false} Test_StackInterp(e : exp) 
{
    assert stackInterp(compile(e), []) != Nothing by {
        CompilerDoesntFail(e);
    }
    assert stackInterp(compile(e), []) == Just([expDenote(e)]) by {
        CompilerIsCorrect(e);
    }
}