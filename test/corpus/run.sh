#!/bin/sh
# run.sh CORPUS-DIR RESULTS-DIR
#
# Run every workbook under CORPUS-DIR through exc-corpus.el, each
# in an Emacs of its own, JOBS at a time and for at most LIMIT seconds
# each, then print the report.  A workbook already recorded in RESULTS-DIR
# is skipped unless FRESH=1, which empties RESULTS-DIR first.  A workbook
# whose Emacs crashed or ran out of time still gets a record.  A workbook
# whose name matches the regexp CORPUS_DAY_FIRST, by default those of the
# formulas project, reads text dates with the day before the month, and
# one matching CORPUS_MAC, by default those too, numbers characters as
# Excel on a Mac does.  A workbook last saved by Excel 2007, or by Excel
# 2008 for the Mac, whose version is 12, is left out unless
# CORPUS_EXCEL_12=1: its cached values are those of a version too old to
# hold the rest to.
#
# run.sh --one RESULTS-DIR WORKBOOK runs a single workbook.
#
# The workbooks are read by a byte-compiled copy of the package in
# RESULTS-DIR/.build, so the times recorded are those of the package in
# use, and editing the sources during a run changes nothing.  A run of
# the whole corpus makes the copy again when a source is newer; --one
# makes it only when there is none.

set -u
here=$(cd "$(dirname "$0")" && pwd)
repo=$(cd "$here/../.." && pwd)
: "${EMACS:=emacs}" "${JOBS:=8}" "${LIMIT:=120}" "${FRESH:=0}"
: "${CORPUS_DAY_FIRST=/formulas/}" "${CORPUS_MAC=/formulas/}" "${CORPUS_EXCEL_12:=0}"
export EMACS JOBS LIMIT CORPUS_DAY_FIRST CORPUS_MAC CORPUS_EXCEL_12

limit() {
  if command -v timeout >/dev/null; then timeout -k 5 "$LIMIT" "$@"
  elif command -v gtimeout >/dev/null; then gtimeout -k 5 "$LIMIT" "$@"
  else "$@"
  fi
}

# build RESULTS-DIR [again]: make the compiled copy when there is none,
# or with "again" also when a source is newer than it.
build() {
  dir=$1/.build waited=0
  until mkdir "$dir.lock" 2>/dev/null; do
    waited=$((waited + 1))
    if [ "$waited" -gt 600 ]; then
      echo "run.sh: $dir.lock is still there; remove it if no run is going" >&2
      exit 1
    fi
    sleep 1
  done
  if [ ! -f "$dir/stamp" ] ||
       { [ "${2:-}" = again ] &&
         [ -n "$(find "$repo" "$repo/plugins" -maxdepth 1 \
                   \( -name '*.el' -o -name 'exc-xlsx-module.*' \) \
                   -newer "$dir/stamp" | head -1)" ]; }; then
    rm -rf "$dir"
    mkdir -p "$dir/plugins"
    cp "$repo"/*.el "$dir"
    cp "$repo"/plugins/*.el "$repo"/plugins/exc-xlsx-module.* \
       "$dir/plugins" 2>/dev/null
    touch "$dir/stamp"
    # A file that does not compile, as exc-etaf.el without etaf,
    # is loaded as source.
    (cd "$dir" && "$EMACS" -Q --batch -L . -L plugins \
       -f batch-byte-compile ./*.el plugins/*.el >"$dir/compile.log" 2>&1)
  fi
  rmdir "$dir.lock"
}

if [ "${1:-}" = --one ]; then
  results=$2 file=$3
  build "$results"
  lisp=$results/.build
  id=$(printf '%s' "$file" | shasum | cut -c1-16)
  record=$results/$id.json
  [ -s "$record" ] && exit 0
  if [ "$CORPUS_EXCEL_12" != 1 ] &&
       unzip -p "$file" docProps/app.xml 2>/dev/null |
         grep -Eq '<([A-Za-z0-9]+:)?AppVersion>12\.'; then
    exit 0
  fi
  start=$(date +%s)
  limit "$EMACS" -Q --batch -L "$lisp" -L "$lisp/plugins" \
    -l "$here/exc-corpus.el" -f exc-corpus-run-file \
    "$file" "$record" >"$results/$id.log" 2>&1
  code=$?
  if [ ! -s "$record" ]; then
    stage=crash
    [ "$code" = 124 ] && stage=timeout
    tail -c 400 "$results/$id.log" >"$results/$id.tail"
    CORPUS_FILE=$file CORPUS_STAGE=$stage CORPUS_EXIT=$code \
    CORPUS_SECONDS=$(( $(date +%s) - start )) CORPUS_TAIL=$results/$id.tail \
    "$EMACS" -Q --batch --eval '
(progn
  (require (quote json))
  (princ (json-encode
          (list :file (getenv "CORPUS_FILE")
                :failed-stage (getenv "CORPUS_STAGE")
                :exit (string-to-number (getenv "CORPUS_EXIT"))
                :seconds (string-to-number (getenv "CORPUS_SECONDS"))
                :error (with-temp-buffer
                         (insert-file-contents (getenv "CORPUS_TAIL"))
                         (buffer-string))))))' >"$record"
    rm -f "$results/$id.tail"
  fi
  rm -f "$results/$id.log"
  # Any other status would stop xargs.
  exit 0
fi

corpus=${1:?usage: run.sh CORPUS-DIR RESULTS-DIR}
results=${2:?usage: run.sh CORPUS-DIR RESULTS-DIR}
[ "$FRESH" = 1 ] && rm -rf "$results"
mkdir -p "$results"
results=$(cd "$results" && pwd)
build "$results" again

find "$corpus" -path '*/.git' -prune -o -type f \( -iname '*.xlsx' -o -iname '*.xlsm' \) -print0 |
  xargs -0 -P "$JOBS" -n 1 "$0" --one "$results"

"$EMACS" -Q --batch -L "$results/.build" -L "$results/.build/plugins" \
  -l "$here/exc-corpus.el" -f exc-corpus-summarize "$corpus" "$results" 2>/dev/null
