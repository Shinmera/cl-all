all:
	sbcl --no-userinit \
		--eval "(require :asdf)" \
		--load "cl-all.asd" \
		--eval "(asdf:make :cl-all)"
