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
