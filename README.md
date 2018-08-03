## About CL-ALL
This is a simple script to run a snippet in several lisp implementations. Requires at least SBCL.

## How To
After putting `cl-all` into your path somehow, either by augmenting `PATH` or symlinking it:

    cl-all '(print :hi)'
    cl-all sbcl ecl '(print "Oh man!")'
    cl-all --print '"Oh man"'
    cl-all --file "something.lisp"
    echo ":stdin" | cl-all --print

## Reference

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
