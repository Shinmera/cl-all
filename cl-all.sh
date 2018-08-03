#!/usr/bin/env bash
## Discover source directory
SOURCE="${BASH_SOURCE[0]}"
while [ -h "$SOURCE" ]; do
  DIR="$( cd -P "$( dirname "$SOURCE" )" >/dev/null && pwd )"
  SOURCE="$(readlink "$SOURCE")"
  [[ $SOURCE != /* ]] && SOURCE="$DIR/$SOURCE"
done
DIR="$( cd -P "$( dirname "$SOURCE" )" >/dev/null && pwd )"

## Launch SBCL
SRC="$DIR/cl-all.lisp"
sbcl --noinform --no-userinit --load "$SRC" --eval "(cl-all:toplevel)" --quit --end-toplevel-options "$@" < "/dev/stdin"
