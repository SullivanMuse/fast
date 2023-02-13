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
int = digit+ ('_' digit+)*
kw = 'case' | 'of' | 'do' | 'end'
id = !kw alpha ('_' alnum)*
tag = ':' id

# Pattern
pname = id
ignore = '_' id                                     _hello
pint = int                                          1234_5678
ptag = tag                                          :x
pellipsis = '..' id?
pitem = patom | pellipsis
ptuple = (pitem ',')+ pitem?                        x, ..middle, z
pparen = '(' pattern ')'                            (x, y) (x)
punit = '(' ')'                                     ()
patom = pparen | punit | ptag | pint | pname | ignore      (x, y) () :x 1234 _hel
pinner = !papp (ptuple | patom)                     error if f(x) because function pattern must not appear inside another pattern
papp = patom ('(' pinner ')')*                      f(x)(y, z)
pattern = papp | ptuple | patom                     f(x); x, y; ()

# Expression
eint = int                                          1234_5678
etag = tag                                          :x
name = id                                           x

eellipsis = '..' eapp
eitem = expr | eellipsis
etuple = (eitem ',')+ eitem?                        x, ..f(x), y
eparen = '(' expr ')'                               (x)
eunit = '(' ')'                                     ()
eatom = eparen | eunit | etag | eint | ename        (()) () :x 1234_5678 x
eapp = eatom ('(' (eitem ',')+ eitem? ')')*         f(x, ..ys)(z)
arm = 'of' pattern '=' expr
case = 'case' expr arm* 'end'                       case x of x, y = x + y end
assign = pattern '=' expr
statement = (assign | expr) ';'
do = 'do' statement* expr? 'end'
expr = case | do | etuple | eapp
```

## Todo

## Done

- Write parser
- Write tests for parser
