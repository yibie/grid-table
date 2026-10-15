EMACS ?= emacs
# Directory holding the ecss, ebox and etaf checkouts; test-etaf needs it.
ETAF_SRC ?=

MODEL_TESTS = test/grid-table-merge-test.el \
	test/grid-table-render-characterization-test.el \
	test/grid-table-vertical-model-test.el \
	test/grid-table-vertical-render-test.el \
	test/grid-table-formula-test.el
BATCH = $(EMACS) -Q --batch -L . -L plugins -L test
ETAF_PATH = -L $(ETAF_SRC)/ecss -L $(ETAF_SRC)/ebox -L $(ETAF_SRC)/etaf

.PHONY: test test-model test-etaf test-xlsx test-corpus corpus-fetch module module-dist

test: test-model test-etaf test-xlsx

# The model, the formulas and the plain-text table need no ETAF.
test-model:
	$(BATCH) $(foreach file,$(MODEL_TESTS),-l $(file)) -f ert-run-tests-batch-and-exit

test-etaf:
	@test -n "$(ETAF_SRC)" || { echo "Set ETAF_SRC to the directory holding ecss, ebox and etaf"; exit 1; }
	$(BATCH) $(ETAF_PATH) -l test/grid-table-etaf-test.el -f ert-run-tests-batch-and-exit

# The xlsx plugin's dynamic module; needs cargo.
module:
	cd plugins/xlsx && cargo build --release

# Refresh the prebuilt module kept in plugins/ for this platform.  On macOS
# it holds both architectures, which needs both rustup targets installed.
MODULE = grid-table-xlsx-module
MODULE_LIB = libgrid_table_xlsx_module
MAC_TARGETS = aarch64-apple-darwin x86_64-apple-darwin
module-dist:
ifeq ($(shell uname -s),Darwin)
	cd plugins/xlsx && $(foreach target,$(MAC_TARGETS),cargo build --release --target $(target) &&) true
	lipo -create $(foreach target,$(MAC_TARGETS),plugins/xlsx/target/$(target)/release/$(MODULE_LIB).dylib) \
		-output plugins/$(MODULE).dylib
else
	cd plugins/xlsx && cargo build --release
	cp plugins/xlsx/target/release/$(MODULE_LIB).so plugins/$(MODULE).so
endif

# Skips the tests that read or write a workbook when the module is not built.
test-xlsx:
	$(BATCH) $(if $(ETAF_SRC),$(ETAF_PATH)) -l test/grid-table-xlsx-test.el -f ert-run-tests-batch-and-exit

# Workbooks from other spreadsheet projects, opened, written and opened
# again; needs the module.  corpus-fetch gathers them into CORPUS_DIR.
CORPUS_DIR ?= $(HOME)/.cache/grid-table-corpus
CORPUS_RESULTS ?= $(CORPUS_DIR)/results

corpus-fetch:
	test/corpus/fetch.sh $(CORPUS_DIR)

test-corpus:
	EMACS=$(EMACS) test/corpus/run.sh $(CORPUS_DIR) $(CORPUS_RESULTS)
