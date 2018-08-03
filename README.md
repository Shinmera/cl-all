## About CL-ALL
This is a simple script to run a snippet in several lisp implementations. Requires at least SBCL.

## How To
You can either build a fixed SBCL binary with `make`, or symlink `cl-all.sh` to `cl-all`. Either way, once `cl-all` is in your path:

    cl-all '(print :hi)'
    cl-all sbcl ecl '(print "Oh man!")'
    cl-all --print '"Oh man"'
    cl-all --file "something.lisp"
    echo ":stdin" | cl-all --print

You can also load this system into your SBCL session and use it from the comfort of the REPL:

    (cl-all:eval-in-lisp :ecl "(print call-arguments-limit)")

## Example Output

    $ cl-all --print call-arguments-limit
     --> ABCL:     50
     --> Allegro:  16384
     --> CCL:      65536
    
     --> CLisp:    4096
    
     --> CMUCL:    536870911
     --> ECL:      65536
     --> MKCL:     2305843009213693951
     --> SBCL:     4611686018427387903

## CLI Reference

    cl-all (implementation | option | snippet)*
    
      implementation:
        The given implementation is run. If no implementations are
        explicitly specified, all known and available implementations
        are used.
    
      option:
        --print -p   Causes the last form's value to be printed.
        --file  -f   Uses the given file as input.
        --eval  -e   Evaluates the given expression.
        --lisps -l   Lists all known and available implementations.
        --           All further arguments are used as tokens to be evaluated,
                     concatenated by spaces.

      snippet:
        A lisp snippet to be evaluated.

      If no snippet, file, or eval option is given, the standard input is
      used to read forms from. Forms are read until EOF is encountered (C-d).
