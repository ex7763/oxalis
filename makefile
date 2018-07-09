LISP ?= sbcl

all:
	$(LISP) --eval '(ql:quickload :oxalis)' \
			--eval '(oxalis:main)' \
			--eval '(quit)'

add:
	git add -A

push:
	git push -u origin master
