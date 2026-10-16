#!/bin/sh
# run.sh CORPUS-DIR RESULTS-DIR
#
# Run every workbook under CORPUS-DIR through grid-table-corpus.el, each
# in an Emacs of its own, JOBS at a time and for at most LIMIT seconds
# each, then print the report.  A workbook already recorded in RESULTS-DIR
# is skipped unless FRESH=1, which empties RESULTS-DIR first.  A workbook
# whose Emacs crashed or ran out of time still gets a record.
#
# run.sh --one RESULTS-DIR WORKBOOK runs a single workbook.

set -u
here=$(cd "$(dirname "$0")" && pwd)
repo=$(cd "$here/../.." && pwd)
: "${EMACS:=emacs}" "${JOBS:=8}" "${LIMIT:=120}" "${FRESH:=0}"
export EMACS JOBS LIMIT

limit() {
  if command -v timeout >/dev/null; then timeout -k 5 "$LIMIT" "$@"
  elif command -v gtimeout >/dev/null; then gtimeout -k 5 "$LIMIT" "$@"
  else "$@"
  fi
}

if [ "${1:-}" = --one ]; then
  results=$2 file=$3
  id=$(printf '%s' "$file" | shasum | cut -c1-16)
  record=$results/$id.json
  [ -s "$record" ] && exit 0
  start=$(date +%s)
  limit "$EMACS" -Q --batch -L "$repo" -L "$repo/plugins" \
    -l "$here/grid-table-corpus.el" -f grid-table-corpus-run-file \
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

find "$corpus" -path '*/.git' -prune -o -type f \( -iname '*.xlsx' -o -iname '*.xlsm' \) -print0 |
  xargs -0 -P "$JOBS" -n 1 "$0" --one "$results"

"$EMACS" -Q --batch -L "$repo" -L "$repo/plugins" -l "$here/grid-table-corpus.el" \
  -f grid-table-corpus-summarize "$corpus" "$results" 2>/dev/null
