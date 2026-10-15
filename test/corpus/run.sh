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
    "$EMACS" -Q --batch --eval '
(require (quote json))
(let ((args command-line-args-left))
  (setq command-line-args-left nil)
  (princ (json-encode
          (list :file (nth 0 args) :failed-stage (nth 1 args)
                :exit (string-to-number (nth 2 args))
                :seconds (string-to-number (nth 3 args))
                :error (with-temp-buffer
                         (insert-file-contents (nth 4 args))
                         (buffer-string))))))' \
      "$file" "$stage" "$code" $(( $(date +%s) - start )) "$results/$id.tail" >"$record"
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
