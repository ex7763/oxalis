LISP ?= sbcl

all:
	$(LISP) --eval '(ql:quickload :oxalis)' \
			--eval '(quit)'
