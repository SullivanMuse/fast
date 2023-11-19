# Fast

A rust implementation of the Fast programming language.

## Philosophy

A smaller language is an easier language. Easier language is faster to program correctly. The faster you can program something correctly, the faster one can iterate to make the algorithms faster.

## Features

- Literals
    - Integers `1234_5678`
    - Paren `(atom)`
    - Id `x`
    - Tuple `(), (x, y, z)`
    - Tag `:atom`
- Patterns
    - Ignore `_`
    - Ellipsis `..`
    - Tuple `(1, .., 3)`
    - Bind `x`
    - Bind equal `x=(1, 2, 3)`
    - Tag `:atom`

## `Rc<RefCell<T>>` cheatsheet

- `match` subjects should be used like `match *x.borrow_mut() { ... }`

## Grammar

```
assign = id '=' expr
statement = assign | expr
statements = (statement ';')* statement?

unit = '()'
int = (digit1 '_')* digit1
id = alpha ('_' | alnum1)*
do = '{' statements '}'
quote = ':' atomic
eval = '$' atomic
paren = '(' expr ')'
atomic = unit | int | id | quote | eval | do | paren

call = atomic+
fn = id '->' expr
expr = fn | call | atomic
```

## Todo

- Implement a Hindley-Milner type checker

## Done

- Implement case expressions
- Write parser
- Write tests for parser
- Need to revamp the environment, so that we can push and pop at appropriate times.
- Recursion
    - In order to implement recursion
        1. When the closure is created, check the environment for each capture. If a capture does not exist in the environment, add it as an Uninit
        2. During assignment statements, if the binding already exists in the environment and is Uninit, edit the inner value to be init
        3. When getting a value from the environment, if the value is Uninit, panic
