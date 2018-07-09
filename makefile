LISP ?= sbcl

all:
	$(LISP) --eval '(ql:quickload :oxalis)' \
			--eval '(quit)'

add:
	git add -A

push:
	git push -u origin master
