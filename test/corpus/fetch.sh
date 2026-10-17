#!/bin/sh
# fetch.sh CORPUS-DIR
#
# Gather the workbooks other spreadsheet projects test themselves with,
# each project in a directory of its own under CORPUS-DIR.  Only the xlsx
# and xlsm files of each repository are checked out.  A project already
# there is left as it is; delete its directory to fetch it again.
#
# The files keep their projects' licenses, which is why they stay out of
# this repository.

set -u
dest=${1:?usage: fetch.sh CORPUS-DIR}
mkdir -p "$dest"

fetch() {
  name=$1 url=$2
  if [ -d "$dest/$name" ]; then
    echo "have  $name"
    return
  fi
  if ! git clone -q --depth 1 --filter=blob:none --no-checkout "$url" "$dest/$name"; then
    echo "FAIL  $name"
    return
  fi
  git -C "$dest/$name" sparse-checkout set --no-cone '*.xlsx' '*.xlsm' '*.XLSX' >/dev/null 2>&1
  if ! git -C "$dest/$name" checkout -q; then
    # Else the next run would take the project as fetched.
    rm -rf "${dest:?}/$name"
    echo "FAIL  $name"
    return
  fi
  echo "got   $name $(find "$dest/$name" -type f \( -iname '*.xlsx' -o -iname '*.xlsm' \) | wc -l | tr -d ' ') workbooks"
}

fetch umya https://github.com/MathNya/umya-spreadsheet.git &
fetch poi https://github.com/apache/poi.git &
fetch xlsxwriter https://github.com/jmcnamara/XlsxWriter.git &
fetch rust_xlsxwriter https://github.com/jmcnamara/rust_xlsxwriter.git &
fetch calamine https://github.com/tafia/calamine.git &
fetch pandas https://github.com/pandas-dev/pandas.git &
fetch exceljs https://github.com/exceljs/exceljs.git &
fetch libreoffice https://github.com/LibreOffice/core.git &
# Projects that compute formulas, whose test workbooks Excel saved with
# its own values.
fetch ironcalc https://github.com/ironcalc/IronCalc.git &
fetch phpspreadsheet https://github.com/PHPOffice/PhpSpreadsheet.git &
fetch closedxml https://github.com/ClosedXML/ClosedXML.git &
fetch epplus https://github.com/EPPlusSoftware/EPPlus.git &
fetch xlcalculator https://github.com/bradbase/xlcalculator.git &
fetch formulas https://github.com/vinci1it2000/formulas.git &
fetch pycel https://github.com/dgorissen/pycel.git &
fetch excelize https://github.com/xuri/excelize.git &
wait
