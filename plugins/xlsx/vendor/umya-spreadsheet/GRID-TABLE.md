# umya-spreadsheet 3.1.1, as grid-table uses it

This is umya-spreadsheet 3.1.1 from crates.io (MIT, see LICENSE), without
its tests, examples and images.  `plugins/xlsx/Cargo.toml` puts it in
place of the published crate with `[patch.crates-io]`.

Each change is marked in the source with a `grid-table:` comment.  They
were found by opening, writing and opening again the workbooks of other
spreadsheet projects (`make test-corpus`).

| File | Change | Seen in |
|------|--------|---------|
| `structs/raw/raw_relationship.rs` | A target such as `#Sheet2!A1` is a place in the workbook, not a part to load | POI `ConditionalFormattingSamples.xlsx`, `50154.xlsx` |
| `structs/raw/raw_file.rs` | A part the package lacks is skipped when a sheet is read lazily, as it is when the workbook is read at once | POI `47504.xlsx`, xlsxwriter `hyperlink38.xlsx` |
| `structs/writer_manager.rs` | A part the source lacked is not copied on write | as above |
| `structs/page_setup.rs` | Printer settings a sheet points at but does not have are left out | POI `57181.xlsm`, xlsxwriter `chart_bar18.xlsx` |
| `reader/xlsx/worksheet.rs` | A hyperlink over a range is given to every cell of the range (its first cell only when the range is open-ended or over 65,536 cells) | POI `sharedhyperlink.xlsx` |
| `structs/drawing/charts/area_chart_series.rs` | Error bars are skipped, so their `c:val` is not read as the series values | xlsxwriter `chart_errorbars04.xlsx` |
| `helper/address.rs` | A quoted sheet name has its doubled apostrophes undone; a name that reads as a cell or an R1C1 reference is quoted | xlsxwriter `quote_name01.xlsx`, `quote_name03.xlsx` |
| `structs/address.rs` | An address quotes its sheet name whenever a formula needs it, not only for a space | as above |
| `structs/drawing/charts/formula.rs` | A chart's `c:f` is escaped, so `Sheet&8` stays well-formed XML | as above |
| `structs/cell.rs`, `structs/row.rs` | A cell whose `r` leaves out its row, or is missing, takes the place its row and order give it, as POI reads it | POI `invalid_cell_reference.xlsx` |
| `structs/table.rs`, `reader/xlsx/table.rs`, `writer/xlsx/table.rs` | A table keeps `headerRowCount`, so one without a header row stays without | xlsxwriter `table07.xlsx`, POI `SingleCellTable.xlsx` |
| `structs/cell_formula.rs` | An array formula is written with `t="array"` and the `ref` it fills; upstream wrote it as a plain formula | grid-table spills, CSE formulas in POI files |
| `structs/cell.rs`, `structs/workbook.rs`, `writer/xlsx/metadata.rs` (new), `writer/xlsx.rs`, `writer/xlsx/workbook_rels.rs`, `structs/writer_manager.rs`, `helper/const_str.rs` | A cell can be marked a dynamic array: it is written with `cm="1"` and the workbook gets the `xl/metadata.xml` Excel writes for one, with its relationship and content type. Upstream wrote no `cm` at all; now only an array formula's is written, so a `cm` read from a file never points into a part that is not kept | xlsxwriter `dynamic_array01.xlsx` (the metadata is the same, byte for byte) |
| `helper/formula.rs`, `structs/workbook.rs` | Follow `split_address` returning the name it unquoted | |

To move to a newer umya-spreadsheet, check whether each of these is
fixed there, carry over the ones that are not, and run `make test-corpus`.
