EMACS ?= emacs
# Directory holding the ecss, ebox and etaf checkouts; test-etaf needs it.
ETAF_SRC ?=

CLASSIC_TESTS = test/grid-table-merge-test.el \
	test/grid-table-render-characterization-test.el \
	test/grid-table-vertical-model-test.el \
	test/grid-table-vertical-nav-test.el \
	test/grid-table-vertical-render-test.el \
	test/grid-table-formula-test.el
BATCH = $(EMACS) -Q --batch -L . -L plugins -L test
ETAF_PATH = -L $(ETAF_SRC)/ecss -L $(ETAF_SRC)/ebox -L $(ETAF_SRC)/etaf

.PHONY: test test-classic test-etaf

test: test-classic test-etaf

# The classic suite, with the model and formula tests, runs without ETAF on
# the load path, so every grid falls back to the classic renderer.
test-classic:
	$(BATCH) $(foreach file,$(CLASSIC_TESTS),-l $(file)) -f ert-run-tests-batch-and-exit

test-etaf:
	@test -n "$(ETAF_SRC)" || { echo "Set ETAF_SRC to the directory holding ecss, ebox and etaf"; exit 1; }
	$(BATCH) $(ETAF_PATH) -l test/grid-table-etaf-test.el -f ert-run-tests-batch-and-exit
