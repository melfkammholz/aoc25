# Advent of Code 2025

[Advent of Code 2025](https://adventofcode.com/2025) solutions in Haskell


## Setup

Before you run any program, you need to install all dependencies.
```hs
cabal build --only-dependencies
```


## Running Programs

You can run the solution for day `X` and part `Y` with the following command.
```sh
cabal exec runghc DayX/Y.hs < DayX/input.txt
```

You can also run `aoc.zsh` in your current shell to add the `aoc` command.
This command simplifies the previous command, allowing you to run the solution
for day `X` and part `Y` with `aoc X Y`. It installs any missing dependencies
and compiles the program.


## Challenges

I use Advent of Code to explore new concepts. These are some things, formulated
as challenges, that I would like to look into and use, even if they obfuscate my
code. After all, it is all about having fun.

- Implement some purely functional data structures ([Okasaki](https://www.cs.cmu.edu/~rwh/students/okasaki.pdf))
- If graph algorithms come up, look at [Structuring depth-first search algorithms in Haskell](https://dl.acm.org/doi/10.1145/199448.199530)
- Use type-level parameters at runtime
- Use [GHC language extensions](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts.html)
  - OverloadedLists
  - View Patterns, Pattern Synonyms

