EMACS ?= emacs
# Directory holding the ecss, ebox and etaf checkouts; test-etaf needs it.
ETAF_SRC ?=

MODEL_TESTS = test/exc-obsolete-test.el \
	test/exc-merge-test.el \
	test/exc-render-characterization-test.el \
	test/exc-vertical-model-test.el \
	test/exc-vertical-render-test.el \
	test/exc-formula-test.el
# Newer source wins over an older .elc, so `make compile' never leaves
# the tests running stale code.
BATCH = $(EMACS) -Q --batch --eval '(setq load-prefer-newer t)' -L . -L plugins -L test
ETAF_PATH = -L $(ETAF_SRC)/ecss -L $(ETAF_SRC)/ebox -L $(ETAF_SRC)/etaf

.PHONY: compile clean test test-model test-etaf test-xlsx test-corpus corpus-fetch module module-dist

test: test-model test-etaf test-xlsx

# Byte-compile exc and its plugins.  Uncompiled, moving around a
# grid is about ten times slower.
compile:
	@test -n "$(ETAF_SRC)" || { echo "Set ETAF_SRC to the directory holding ecss, ebox and etaf"; exit 1; }
	$(BATCH) $(ETAF_PATH) --eval '(setq byte-compile-warnings nil)' -f batch-byte-compile *.el plugins/*.el

clean:
	rm -f *.elc plugins/*.elc

# The model, the formulas and the plain-text table need no ETAF.
test-model:
	$(BATCH) $(foreach file,$(MODEL_TESTS),-l $(file)) -f ert-run-tests-batch-and-exit

test-etaf:
	@test -n "$(ETAF_SRC)" || { echo "Set ETAF_SRC to the directory holding ecss, ebox and etaf"; exit 1; }
	$(BATCH) $(ETAF_PATH) -l test/exc-etaf-test.el -f ert-run-tests-batch-and-exit

# The xlsx plugin's dynamic module; needs cargo.
module:
	cd plugins/xlsx && cargo build --release

# Refresh the prebuilt module kept in plugins/ for this platform.  On macOS
# it holds both architectures, which needs both rustup targets installed.
# The module in target/release is built too, since it is loaded before
# the prebuilt one and would otherwise be left stale.
MODULE = exc-xlsx-module
MODULE_LIB = libexc_xlsx_module
MAC_TARGETS = aarch64-apple-darwin x86_64-apple-darwin
module-dist:
ifeq ($(shell uname -s),Darwin)
	cd plugins/xlsx && $(foreach target,$(MAC_TARGETS),cargo build --release --target $(target) &&) cargo build --release
	lipo -create $(foreach target,$(MAC_TARGETS),plugins/xlsx/target/$(target)/release/$(MODULE_LIB).dylib) \
		-output plugins/$(MODULE).dylib
else
	cd plugins/xlsx && cargo build --release
	cp plugins/xlsx/target/release/$(MODULE_LIB).so plugins/$(MODULE).so
endif

# Skips the tests that read or write a workbook when the module is not built.
test-xlsx:
	$(BATCH) $(if $(ETAF_SRC),$(ETAF_PATH)) -l test/exc-xlsx-test.el -f ert-run-tests-batch-and-exit

# Workbooks from other spreadsheet projects, opened, written and opened
# again; needs the module.  corpus-fetch gathers them into CORPUS_DIR.
CORPUS_DIR ?= $(HOME)/.cache/grid-table-corpus
CORPUS_RESULTS ?= $(CORPUS_DIR)/results

corpus-fetch:
	test/corpus/fetch.sh $(CORPUS_DIR)

test-corpus:
	EMACS=$(EMACS) test/corpus/run.sh $(CORPUS_DIR) $(CORPUS_RESULTS)
