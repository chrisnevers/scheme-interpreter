# scheme-interpreter

This project is an implementation of a Scheme interpreter, as described by 
[Write Yourself a Scheme in 48 Hours](https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours).

## Running The Project

    cabal run

## Checklist

1. ~~Parser~~

    The grammar supported is as follows:
    ```
          Exp := 
            | Num           -- 3
            | Bool          -- #t
            | Atom          -- chris
            | String        -- "hello"
            | List          -- (1 2 3)
            | DottedList    -- (1 2 . 3)
    ```
    Current syntactic sugar supported:
    ```
    '(chris nevers)   => (quote chris nevers)
    #(1 "ubermensch") => (vector 1 "ubermensch")
    ```
2. ~~Eval (Part 1)~~

    Include support for some primitive operations:
    
        binary-ops   := + | - | * | / | mod | quotient | remainder
        type-testing := string? | number? | symbol?

3. ~~Errors~~

    Implement some error handling for encounterable errors.
