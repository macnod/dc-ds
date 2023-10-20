TESTS_FILE="$(PWD)/dc-ds-tests.lisp"
LISP=/usr/bin/sbcl
# Reporter can be list dot tap or fiveam.
REPORTER=list
test:
	$(LISP) --eval "(require :prove)" \
	  --eval "(prove:run #P\"$(TESTS_FILE)\" :reporter :$(REPORTER))" \
	  --non-interactive
