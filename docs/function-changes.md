## Extended function support

### Implementation details:
- Term.hs: added `Fun [String] Term` and `ApplyFun Term [Term]`
- Value.hs: added `ClosureVal [String] Term [(String, Value)]` (captures store applied args; no lexical capture yet)
- Small.hs:
  - `Fun` constructs a function value, reduces to `ClosureVal xs body []`
  - `ApplyFun` uses call-by-value, left-to-right (evaluate function, then each argument, then apply)
  - While stepping we keep the original call shape. When it’s time to run, we temporarily set each parameter name to its argument value, run the body once, then restore things to how they were.
  - Multi-parameter: we evaluate the function, then each argument left to right. Each argument fills the next parameter. If you provide fewer arguments, you get back a function that remembers what you gave. Once you’ve provided them all, we run the body once with those values.
  - Zero-parameter: there are no parameters to fill, so calling it just runs the body. Passing any arguments is an error. Calling a function that still expects parameters without supplying them is also an error.

### Here's an example:

```haskell
-- single parameter
let inc = Fun ["x"] (BinaryOps Add (Var "x") (Literal 1))
ApplyFun inc [Literal 41]  -- outputs 42
```

```haskell
-- zero parameters
let fortyTwo = Fun [] (Literal 42)
ApplyFun fortyTwo []  -- outputs 42
```

```haskell
-- partial application (multi-parameter)
let add = Fun ["x","y"] (BinaryOps Add (Var "x") (Var "y"))
let add2 = ApplyFun add [Literal 2]       -- returns a function waiting for y
ApplyFun add2 [Literal 40]                -- outputs 42
```

### Some notes and limitations:
- Multiple parameters supported directly via `[String]` and `[Term]`; fewer args => partial application
- Non-function application errors; too many args to zero-parameter errors; missing args errors
- Functions do not capture outer variables (scoping/lexical capture left for later)
