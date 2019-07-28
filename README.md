# scheme-interpreter

This project is an implementation of a Scheme interpreter, as described by 
[Write Yourself a Scheme in 48 Hours](https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours).

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
