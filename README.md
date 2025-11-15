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


