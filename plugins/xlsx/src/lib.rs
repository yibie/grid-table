//! Emacs dynamic module that reads and writes xlsx files for grid-table.
//!
//! The module knows nothing about grid-table's model.  Reading returns a
//! printed Lisp form describing one sheet; writing takes a flat text of
//! records.  `plugins/grid-table-xlsx.el` does the mapping on both sides.

use emacs::{defun, Env, Result};
use std::fmt::Write;
use std::path::Path;
use umya_spreadsheet::{
    reader, writer, CellFormulaValues, CellRawValue, Color, ConditionalFormatValues, ConditionalFormatting,
    ConditionalFormattingOperatorValues, ConditionalFormattingRule, DataValidation,
    Comment, DataValidationValues, DataValidations, DefinedName, Formula, Hyperlink,
    HorizontalAlignmentValues, Pane,
    PaneStateValues, PaneValues, PatternValues, Selection, SheetView, ShowColumn, ShowStripes,
    Style, Table, TableColumn, TableStyleInfo, Workbook, Worksheet,
};
use umya_spreadsheet::structs::{AutoFilter, FilterColumn};
use umya_spreadsheet::helper::coordinate::{index_from_coordinate, string_from_column_index};
use umya_spreadsheet::structs::drawing::Theme;
use umya_spreadsheet::structs::drawing::charts::{
    AreaChartSeries, BarDirectionValues, CategoryAxisData, ChartText, GroupingValues, NumericValue,
    ScatterStyleValues, Smooth, StringReference,
};
use umya_spreadsheet::structs::drawing::spreadsheet::MarkerType;
use umya_spreadsheet::structs::{Chart, ChartType};

emacs::plugin_is_GPL_compatible!();

/// Separates the records of a write payload.
const RECORD: char = '\u{1e}';
/// Separates the fields of a record.
const FIELD: char = '\u{1f}';
/// Separates the items of a list inside a field.
const ITEM: char = '\u{1d}';

#[emacs::module(name = "grid-table-xlsx-module")]
fn init(_: &Env) -> Result<()> {
    Ok(())
}

fn fail(what: impl std::fmt::Debug) -> emacs::Error {
    emacs::Error::msg(format!("{:?}", what))
}

fn quote(out: &mut String, text: &str) {
    out.push('"');
    for ch in text.chars() {
        match ch {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            _ => out.push(ch),
        }
    }
    out.push('"');
}

/// TEXT from XML with its line breaks as XML reads them: a carriage
/// return written as itself, alone or before a line feed, is a line feed.
fn xml_lines(text: &str) -> String {
    text.replace("\r\n", "\n").replace('\r', "\n")
}

/// TEXT with each _xHHHH_ escape, as OOXML writes a line break in a table
/// column's name, taken back to its character.  Its line breaks are first
/// read as XML reads them, so only _x000D_ stays a carriage return.
fn unescape_x(text: &str) -> String {
    let text = xml_lines(text);
    let mut out = String::new();
    let mut rest = text.as_str();
    while let Some(at) = rest.find("_x") {
        let code = rest.get(at + 2..at + 6);
        let ok = code.filter(|c| c.chars().all(|c| c.is_ascii_hexdigit()))
            .and_then(|c| u32::from_str_radix(c, 16).ok())
            .and_then(char::from_u32)
            .filter(|_| rest.get(at + 6..at + 7) == Some("_"));
        match ok {
            Some(c) => {
                out.push_str(&rest[..at]);
                out.push(c);
                rest = &rest[at + 7..];
            }
            None => {
                out.push_str(&rest[..at + 2]);
                rest = &rest[at + 2..];
            }
        }
    }
    out.push_str(rest);
    out
}

fn keyword(out: &mut String, name: &str, text: &str) {
    out.push(' ');
    out.push_str(name);
    out.push(' ');
    quote(out, text);
}

/// The RRGGBB of COLOR, or nothing when it resolves to no color.
fn rgb(color: &Color, theme: &Theme) -> Option<String> {
    let argb = color.argb_with_theme(theme);
    let hex = argb.trim_start_matches('#');
    let start = hex.len().checked_sub(6)?;
    let rgb = &hex[start..];
    rgb.chars()
        .all(|ch| ch.is_ascii_hexdigit())
        .then(|| rgb.to_ascii_uppercase())
}

/// Append the parts of STYLE grid-table has a counterpart for.
/// A differential style (of a conditional format) keeps its fill in the
/// pattern's background color; a cell style keeps it in the foreground.
fn push_style(out: &mut String, style: &Style, theme: &Theme, differential: bool) {
    if let Some(font) = style.font() {
        if font.bold() {
            out.push_str(" :bold t");
        }
        if font.italic() {
            out.push_str(" :italic t");
        }
        if let Some(color) = rgb(font.color(), theme) {
            keyword(out, ":color", &color);
        }
    }
    if let Some(pattern) = style.fill().and_then(|fill| fill.pattern_fill()) {
        let front = pattern.foreground_color().and_then(|c| rgb(c, theme));
        let back = pattern.background_color().and_then(|c| rgb(c, theme));
        let fill = if differential {
            back.or(front)
        } else if *pattern.pattern_type() == PatternValues::None {
            None
        } else {
            front
        };
        if let Some(fill) = fill {
            keyword(out, ":fill", &fill);
        }
    }
    if let Some(alignment) = style.alignment() {
        let align = match alignment.horizontal() {
            HorizontalAlignmentValues::Left => Some("left"),
            HorizontalAlignmentValues::Center => Some("center"),
            HorizontalAlignmentValues::Right => Some("right"),
            _ => None,
        };
        if let Some(align) = align {
            keyword(out, ":align", align);
        }
    }
}

fn push_sheet(out: &mut String, sheet: &Worksheet, theme: &Theme) -> Result<()> {
    out.push_str("(:cells (");
    for cell in sheet.cells_sorted() {
        let at = cell.coordinate();
        write!(out, "({} {} ", at.row_num() - 1, at.col_num() - 1)?;
        // A data table or a spilled array has no formula of its own to
        // give, only the value it shows.
        if cell.is_formula() && !cell.formula().is_empty() {
            quote(out, &format!("={}", xml_lines(cell.formula())));
            // An array formula computes over whole ranges and fills the
            // cells of its reference; a plain one takes one cell of each.
            if let Some(formula) = cell.formula_obj() {
                if *formula.formula_type() == CellFormulaValues::Array {
                    keyword(out, ":array", formula.reference());
                    // A dynamic array, which spills, rather than {=...}.
                    if cell.is_dynamic_array() {
                        out.push_str(" :dynamic t");
                    }
                }
            }
            // What the writer last computed for the formula, to check
            // grid-table's own result against.
            match cell.raw_value() {
                CellRawValue::Numeric(number) if number.is_finite() => {
                    write!(out, " :cached {number:?}")?;
                }
                CellRawValue::Bool(truth) => {
                    out.push_str(if *truth { " :cached :true" } else { " :cached :false" });
                }
                CellRawValue::Error(error) => {
                    out.push_str(" :cached (:error ");
                    quote(out, &error.to_string());
                    out.push(')');
                }
                CellRawValue::String(_) | CellRawValue::RichText(_) => {
                    keyword(out, ":cached", &unescape_x(&cell.value()));
                }
                _ => {}
            }
        } else {
            // Text keeps a control character as _xHHHH_.
            let text = matches!(cell.data_type(), "s" | "str" | "inlineStr");
            let value = cell.value();
            quote(out, &if text { unescape_x(&value) } else { value.to_string() });
            // Text that reads as something else, such as 007 or =A1,
            // must stay text; grid-table decides which text that is.
            if text {
                keyword(out, ":text", "1");
            }
        }
        if let Some(format) = cell.style().number_format() {
            let code = format.format_code();
            if code != "General" {
                keyword(out, ":format", code);
            }
        }
        push_style(out, cell.style(), theme, false);
        // A link inside the workbook is written #Sheet2!A1, as grid-table
        // keeps it.
        if let Some(link) = cell.hyperlink() {
            if link.location() {
                keyword(out, ":link", &format!("#{}", link.url()));
            } else if !link.url().is_empty() {
                keyword(out, ":link", link.url());
            }
        }
        out.push(')');
    }
    out.push_str(") :notes (");
    for comment in sheet.comments() {
        let at = comment.coordinate();
        let text = match (comment.text().text(), comment.text().rich_text()) {
            (Some(text), _) => text.value().to_string(),
            (None, Some(rich)) => rich.text().to_string(),
            (None, None) => String::new(),
        };
        if !text.is_empty() {
            write!(out, "({} {} ", at.row_num() - 1, at.col_num() - 1)?;
            quote(out, &text);
            out.push_str(") ");
        }
    }
    out.push_str(") :merges (");
    for merge in sheet.merge_cells() {
        quote(out, &merge.range());
        out.push(' ');
    }
    out.push_str(") :conditionals (");
    for formatting in sheet.conditional_formatting_collection() {
        let range = formatting.sequence_of_references().get_sqref();
        for rule in formatting.conditional_collection() {
            out.push('(');
            quote(out, &range);
            write!(out, " \"{:?}\" \"{:?}\" ", rule.get_type(), rule.operator())?;
            quote(
                out,
                &rule.formula().map(|f| f.address_str()).unwrap_or_default(),
            );
            out.push(' ');
            quote(out, rule.text());
            write!(out, " {}", rule.priority())?;
            if let Some(style) = rule.style() {
                push_style(out, style, theme, true);
            }
            out.push(')');
        }
    }
    out.push_str(") :widths (");
    for column in sheet.column_dimensions() {
        // 8.38 is what the library gives a column nobody sized.
        if !column.hidden() && column.width() > 0.0 && (column.width() - 8.38).abs() > 1e-9 {
            write!(out, "({} . {}) ", column.col_num() - 1, column.width())?;
        }
    }
    out.push_str(") :hidden-rows (");
    for row in sheet.row_dimensions() {
        if row.hidden() {
            write!(out, "{} ", row.row_num() - 1)?;
        }
    }
    out.push_str(") :hidden-cols (");
    for column in sheet.column_dimensions() {
        if column.hidden() {
            write!(out, "{} ", column.col_num() - 1)?;
        }
    }
    // Only list validations: the others have nowhere to go in grid-table.
    out.push_str(") :validations (");
    if let Some(validations) = sheet.data_validations() {
        for validation in validations.data_validation_list() {
            if matches!(validation.get_type(), DataValidationValues::List) {
                out.push('(');
                quote(out, &validation.sequence_of_references().get_sqref());
                out.push(' ');
                quote(out, validation.formula1());
                out.push(')');
            }
        }
    }
    out.push(')');
    out.push_str(" :charts (");
    for chart in sheet.chart_collection() {
        if let Some(info) = chart_info(chart, sheet.name()) {
            out.push('(');
            for field in [&info.kind, &info.title] {
                quote(out, field);
                out.push(' ');
            }
            write!(
                out,
                "{} {} {} {} (",
                info.from.0, info.from.1, info.to.0, info.to.1
            )?;
            for (role, reference) in &info.refs {
                write!(out, "({} ", role)?;
                quote(out, reference);
                out.push(')');
                out.push(' ');
            }
            // Each series' name: nil, its cells, or (text TEXT).
            out.push_str(") (");
            for name in &info.names {
                match name {
                    SeriesName::None => out.push_str("nil"),
                    SeriesName::Cells(range) => quote(out, range),
                    SeriesName::Text(text) => {
                        out.push_str("(text ");
                        quote(out, text);
                        out.push(')');
                    }
                }
                out.push(' ');
            }
            out.push_str("))");
        }
    }
    out.push(')');
    // Tables, for formulas that name them, as Table1[Qty]: each is
    // (NAME TOP LEFT BOTTOM RIGHT HEADER-ROWS TOTALS-ROWS (COLUMN...)),
    // zero-based and inclusive.
    out.push_str(" :tables (");
    for table in sheet.tables() {
        let (start, end) = table.area();
        out.push('(');
        quote(out, table.display_name());
        write!(
            out,
            " {} {} {} {} {} {} (",
            start.row_num() - 1,
            start.col_num() - 1,
            end.row_num() - 1,
            end.col_num() - 1,
            table.header_row_count(),
            table.totals_row_count()
        )?;
        for column in table.columns() {
            quote(out, &unescape_x(column.name()));
            out.push(' ');
        }
        out.push_str("))");
    }
    out.push(')');
    // Filtered ranges, the sheet's and its tables': (TOP LEFT BOTTOM
    // RIGHT) each, zero-based and inclusive.  A hidden row under a
    // range's header is one the filter left out.  A range filtering on
    // no column only shows its buttons, and its hidden rows are hidden
    // by hand.
    out.push_str(" :filters (");
    let ranges = sheet
        .auto_filter()
        .into_iter()
        .chain(sheet.tables().iter().map(|table| table.auto_filter()))
        .filter(|filter| !filter.filter_columns().is_empty())
        .map(|filter| filter.range().range());
    for range in ranges {
        if let Some((top, left, bottom, right)) = area_of(&range) {
            write!(out, "({top} {left} {bottom} {right})")?;
        }
    }
    out.push(')');
    if let Some(pane) = frozen_pane(sheet) {
        write!(
            out,
            " :frozen ({} {})",
            pane.vertical_split() as u32,
            pane.horizontal_split() as u32
        )?;
    }
    out.push(')');
    Ok(())
}

/// The rows and columns of RANGE, such as A1:B7, zero-based and
/// inclusive: (TOP LEFT BOTTOM RIGHT).
fn area_of(range: &str) -> Option<(u32, u32, u32, u32)> {
    let mut ends = range.split(':').map(|end| index_from_coordinate(end.to_uppercase()));
    let (Some(left), Some(top), ..) = ends.next()? else { return None };
    let (right, bottom) = match ends.next() {
        Some((Some(right), Some(bottom), ..)) => (right, bottom),
        Some(_) => return None,
        None => (left, top),
    };
    (left > 0 && top > 0).then(|| (top - 1, left - 1, bottom - 1, right - 1))
}

/// The pane that keeps rows and columns of SHEET in place, if it has one.
fn frozen_pane(sheet: &Worksheet) -> Option<&Pane> {
    let view = sheet.sheets_views().sheet_view_list().first()?;
    let pane = view.pane()?;
    matches!(
        pane.state(),
        PaneStateValues::Frozen | PaneStateValues::FrozenSplit
    )
    .then_some(pane)
}

/// Keep the first ROWS rows and COLS columns of SHEET in place, or let
/// everything scroll when both are 0.  A view split without being frozen
/// is left as it is unless something is to be frozen.
fn freeze(sheet: &mut Worksheet, rows: u32, cols: u32) {
    if rows == 0 && cols == 0 {
        if frozen_pane(sheet).is_none() {
            return;
        }
        // A pane cannot be taken out of a view: the view is made anew
        // with what it said besides.
        let views = sheet.sheet_views_mut().sheet_view_list_mut();
        let old = views[0].clone();
        let mut view = SheetView::default();
        view.set_workbook_view_id(old.workbook_view_id());
        if old.tab_selected() {
            view.set_tab_selected(true);
        }
        if old.zoom_scale() != 0 && old.zoom_scale() != 100 {
            view.set_zoom_scale(old.zoom_scale());
        }
        views[0] = view;
        return;
    }
    let active = match (rows > 0, cols > 0) {
        (true, true) => PaneValues::BottomRight,
        (true, false) => PaneValues::BottomLeft,
        _ => PaneValues::TopRight,
    };
    let mut pane = Pane::default();
    if cols > 0 {
        pane.set_horizontal_split(cols as f64);
    }
    if rows > 0 {
        pane.set_vertical_split(rows as f64);
    }
    pane.top_left_cell_mut().set_col_num(cols + 1).set_row_num(rows + 1);
    pane.set_active_pane(active.clone());
    pane.set_state(PaneStateValues::Frozen);
    let views = sheet.sheet_views_mut().sheet_view_list_mut();
    if views.is_empty() {
        views.push(SheetView::default());
    }
    let view = &mut views[0];
    view.set_pane(pane);
    // Selections name the panes they are in; the old ones no longer fit.
    view.selection_mut().clear();
    let mut selection = Selection::default();
    selection.set_pane(active);
    view.set_selection(selection);
}

/// Whether BOOK, which may not have loaded its sheets, has a sheet NAME.
fn has_sheet(book: &Workbook, name: &str) -> bool {
    book.sheet_collection_no_check()
        .iter()
        .any(|sheet| sheet.name() == name)
}

/// Return the names of the sheets of the xlsx file PATH, as a printed list.
#[defun]
fn sheets(path: String) -> Result<String> {
    let book = reader::xlsx::lazy_read(Path::new(&path)).map_err(fail)?;
    let mut out = String::from("(");
    for sheet in book.sheet_collection_no_check() {
        quote(&mut out, sheet.name());
        out.push(' ');
    }
    out.push(')');
    Ok(out)
}

/// Return whether the dates of the xlsx file PATH count from 1904.
#[defun]
fn date1904(path: String) -> Result<bool> {
    let book = reader::xlsx::lazy_read(Path::new(&path)).map_err(fail)?;
    Ok(book.date1904())
}

/// Return sheet SHEET of the xlsx file PATH as a printed plist.
/// :cells holds (ROW COL RAW . STYLE), :merges A1 ranges and :conditionals
/// (RANGE TYPE OPERATOR FORMULA TEXT PRIORITY . STYLE).
#[defun]
fn read(path: String, sheet: String) -> Result<String> {
    let mut book = reader::xlsx::lazy_read(Path::new(&path)).map_err(fail)?;
    if !has_sheet(&book, &sheet) {
        return Err(fail(format!("no sheet {} in {}", sheet, path)));
    }
    book.read_sheet_by_name(&sheet);
    let mut out = String::new();
    push_sheet(
        &mut out,
        book.sheet_by_name(&sheet).map_err(fail)?,
        book.theme(),
    )?;
    Ok(out)
}

fn argb(rgb: &str) -> String {
    format!("FF{}", rgb)
}

/// Give STYLE the look of FIELDS: bold, italic, color, fill, align and
/// number format code.
fn apply_style(style: &mut Style, fields: &[&str], differential: bool) {
    let field = |index: usize| fields.get(index).copied().unwrap_or("");
    if field(0) == "1" {
        style.font_mut().set_bold(true);
    }
    if field(1) == "1" {
        style.font_mut().set_italic(true);
    }
    if !field(2).is_empty() {
        style.font_mut().color_mut().set_argb_str(argb(field(2)));
    }
    if !field(3).is_empty() {
        if differential {
            let mut color = Color::default();
            color.set_argb_str(argb(field(3)));
            let pattern = style.fill_mut().pattern_fill_mut();
            pattern.set_pattern_type(PatternValues::Solid);
            pattern.set_background_color(color);
        } else {
            style.set_background_color(argb(field(3)));
        }
    }
    let align = match field(4) {
        "left" => Some(HorizontalAlignmentValues::Left),
        "center" => Some(HorizontalAlignmentValues::Center),
        "right" => Some(HorizontalAlignmentValues::Right),
        _ => None,
    };
    if let Some(align) = align {
        style.alignment_mut().set_horizontal(align);
    }
    if !differential && !field(5).is_empty() {
        style.number_format_mut().set_format_code(field(5));
    }
}

fn conditional(fields: &[&str], priority: i32) -> Result<ConditionalFormatting> {
    let field = |index: usize| fields.get(index).copied().unwrap_or("");
    let mut rule = ConditionalFormattingRule::default();
    let operator = match field(1) {
        ">" => Some(ConditionalFormattingOperatorValues::GreaterThan),
        "<" => Some(ConditionalFormattingOperatorValues::LessThan),
        ">=" => Some(ConditionalFormattingOperatorValues::GreaterThanOrEqual),
        "<=" => Some(ConditionalFormattingOperatorValues::LessThanOrEqual),
        "=" => Some(ConditionalFormattingOperatorValues::Equal),
        "<>" => Some(ConditionalFormattingOperatorValues::NotEqual),
        _ => None,
    };
    match (field(1), operator) {
        (_, Some(operator)) => {
            rule.set_type(ConditionalFormatValues::CellIs);
            rule.set_operator(operator);
        }
        ("contains", _) => {
            rule.set_type(ConditionalFormatValues::ContainsText);
            rule.set_operator(ConditionalFormattingOperatorValues::ContainsText);
            rule.set_text(field(3));
        }
        ("empty", _) => {
            rule.set_type(ConditionalFormatValues::ContainsBlanks);
        }
        ("error", _) => {
            rule.set_type(ConditionalFormatValues::ContainsErrors);
        }
        (other, _) => return Err(fail(format!("unknown conditional operator {}", other))),
    }
    let mut formula = Formula::default();
    formula.set_string_value(field(2));
    rule.set_formula(formula);
    rule.set_priority(priority);
    let mut style = Style::default();
    apply_style(&mut style, &fields[4.min(fields.len())..], true);
    rule.set_style(style);
    let mut formatting = ConditionalFormatting::default();
    formatting.sequence_of_references_mut().set_sqref(field(0));
    formatting.add_conditional_collection(rule);
    Ok(formatting)
}

/// Empty SHEET of what a payload replaces: cells, merges, conditionals.
fn clear(sheet: &mut Worksheet) {
    let filled: Vec<(u32, u32)> = sheet
        .cells()
        .iter()
        .map(|cell| (cell.coordinate().col_num(), cell.coordinate().row_num()))
        .collect();
    for at in filled {
        sheet.remove_cell(at);
    }
    sheet.merge_cells_mut().clear();
    sheet.comments_mut().clear();
    // Charts grid-table reads in are written back from it; the others
    // stay as they are.
    let name = sheet.name().to_string();
    sheet
        .chart_collection_mut()
        .retain(|chart| chart_info(chart, &name).is_none());
    sheet.set_conditional_formatting_collection(Vec::new());
    sheet.remove_data_validations();
    for row in sheet.row_dimensions_mut() {
        row.set_hidden(false);
    }
    for column in sheet.column_dimensions_mut() {
        column.set_hidden(false);
    }
}

/// Give SHEET the tables PAYLOAD lists, when it lists them: after a U
/// record, T NAME TOP LEFT BOTTOM RIGHT HEADER-ROWS TOTALS-ROWS COLUMN...
/// for each, zero-based and inclusive.  A table the sheet has keeps what
/// grid-table does not know of it, such as its style and its columns'
/// totals; one not listed goes, and one the sheet lacks is added.
fn tables(sheet: &mut Worksheet, payload: &str) {
    let records: Vec<Vec<&str>> = payload
        .split(RECORD)
        .map(|record| record.split(FIELD).collect::<Vec<&str>>())
        .collect();
    if !records.iter().any(|fields| fields[0] == "U") {
        return;
    }
    let mut kept = Vec::new();
    let mut old = std::mem::take(sheet.tables_mut());
    for fields in records.iter().filter(|fields| fields[0] == "T" && fields.len() >= 9) {
        let number = |index: usize| fields[index].parse::<u32>().unwrap_or(0);
        let (top, left, bottom, right) = (number(2), number(3), number(4), number(5));
        let (headers, totals) = (number(6), number(7));
        let area = ((left + 1, top + 1), (right + 1, bottom + 1));
        let mut table = match old.iter().position(|table| table.display_name().eq_ignore_ascii_case(fields[1])) {
            Some(index) => old.remove(index),
            None => {
                let mut table = Table::new(fields[1], area);
                table.set_style_info(Some(TableStyleInfo::new(
                    "TableStyleMedium2",
                    ShowColumn::Hide,
                    ShowColumn::Hide,
                    ShowStripes::Show,
                    ShowStripes::Hide,
                )));
                table
            }
        };
        let had_filter = !table.auto_filter().range().range().is_empty();
        table.set_area(area);
        table.set_header_row_count(headers);
        table.set_totals_row_count(totals);
        table.set_totals_row_shown(totals > 0);
        let mut columns: Vec<TableColumn> = table.take_columns();
        let named: Vec<TableColumn> = fields[8..]
            .iter()
            .map(|name| match columns
                .iter()
                .position(|column| unescape_x(column.name()) == *name)
            {
                Some(index) => columns.remove(index),
                None => TableColumn::new(name),
            })
            .collect();
        table.set_columns(named);
        // The filter covers the table but its totals row.
        if had_filter && headers > 0 {
            let last = bottom + 1 - totals;
            table.auto_filter_mut().set_range(format!(
                "{}{}:{}{}",
                string_from_column_index(left + 1),
                top + 1,
                string_from_column_index(right + 1),
                last
            ));
        }
        kept.push(table);
    }
    *sheet.tables_mut() = kept;
}

/// Give SHEET and its tables the filtered ranges PAYLOAD lists, as
/// A TOP LEFT BOTTOM RIGHT records, zero-based and inclusive.  A range a
/// table starts at is the table's; another is the sheet's, which has
/// one.  A filter already there keeps the columns it filters on; a new
/// one filters on its first, for its hidden rows to read as filtered.
fn filters(sheet: &mut Worksheet, payload: &str) {
    fn filtering(filter: &AutoFilter, range: &str) -> bool {
        filter.range().range() == range && !filter.filter_columns().is_empty()
    }
    fn filter(range: String) -> AutoFilter {
        let mut filter = AutoFilter::default();
        filter.set_range(range);
        let mut column = FilterColumn::default();
        column.set_font(0);
        filter.add_filter_column(column);
        filter
    }
    let mut sheet_filter = None;
    for fields in payload
        .split(RECORD)
        .map(|record| record.split(FIELD).collect::<Vec<&str>>())
        .filter(|fields| fields[0] == "A" && fields.len() >= 5)
    {
        let number = |index: usize| fields[index].parse::<u32>().unwrap_or(0);
        let range = format!(
            "{}{}:{}{}",
            string_from_column_index(number(2) + 1),
            number(1) + 1,
            string_from_column_index(number(4) + 1),
            number(3) + 1
        );
        let table = sheet.tables_mut().iter_mut().find(|table| {
            let (start, end) = table.area();
            start.row_num() == number(1) + 1
                && start.col_num() == number(2) + 1
                && end.col_num() == number(4) + 1
        });
        match table {
            Some(table) => {
                if !filtering(table.auto_filter(), &range) {
                    *table.auto_filter_mut() = filter(range);
                }
            }
            None => sheet_filter = Some(range),
        }
    }
    if let Some(range) = sheet_filter {
        if !sheet.auto_filter().is_some_and(|old| filtering(old, &range)) {
            sheet.set_auto_filter(range.clone());
            if let Some(old) = sheet.auto_filter_mut() {
                *old = filter(range);
            }
        }
    }
}

fn fill(sheet: &mut Worksheet, payload: &str) -> Result<()> {
    let mut conditionals = Vec::new();
    let mut validations = DataValidations::default();
    for record in payload.split(RECORD) {
        let fields: Vec<&str> = record.split(FIELD).collect();
        match fields[0] {
            // C row col kind text bold italic color fill align format
            // KIND is f, a:RANGE, d:RANGE, n, b, e, s or t, empty text.
            "C" if fields.len() >= 5 => {
                let row: u32 = fields[1].parse()?;
                let col: u32 = fields[2].parse()?;
                let at = (col + 1, row + 1);
                let text = fields[4];
                if fields[3] == "t" {
                    sheet.cell_mut(at).set_value_string("");
                } else if !text.is_empty() {
                    let cell = sheet.cell_mut(at);
                    match fields[3] {
                        "f" => {
                            cell.set_formula(text);
                        }
                        // An array formula, a:RANGE with the cells it
                        // fills; d:RANGE for a dynamic one, which spills.
                        kind if kind.starts_with("a:") || kind.starts_with("d:") => {
                            let mut formula = umya_spreadsheet::CellFormula::default();
                            formula.set_text(text);
                            formula.set_formula_type(CellFormulaValues::Array);
                            formula.set_reference(&kind[2..]);
                            cell.cell_value_mut().set_formula_obj(formula);
                            cell.set_dynamic_array(kind.starts_with("d:"));
                        }
                        "n" => {
                            cell.set_value_number(text.parse::<f64>()?);
                        }
                        "b" => {
                            cell.set_value_bool(text == "TRUE");
                        }
                        // An error value, as #N/A; one a workbook has no
                        // code for stays text.
                        "e" => {
                            cell.set_error(text);
                        }
                        _ => {
                            // grid-table gives text with _xHHHH_ escapes already.
                            cell.set_value_string(text);
                        }
                    }
                }
                if fields[5..].iter().any(|field| !field.is_empty()) {
                    apply_style(sheet.style_mut(at), &fields[5..], false);
                }
            }
            "M" if fields.len() >= 2 => {
                sheet.add_merge_cells(fields[1]);
            }
            // N row col text: a note on a cell
            "N" if fields.len() >= 4 => {
                let row: u32 = fields[1].parse()?;
                let col: u32 = fields[2].parse()?;
                let mut comment = Comment::default();
                comment
                    .new_comment((col + 1, row + 1))
                    .set_author("grid-table")
                    .set_text_string(fields[3]);
                sheet.add_comments(comment);
            }
            // L row col link: a web address or file, or #Sheet!A1 inside
            "L" if fields.len() >= 4 => {
                let row: u32 = fields[1].parse()?;
                let col: u32 = fields[2].parse()?;
                let mut link = Hyperlink::default();
                match fields[3].strip_prefix('#') {
                    Some(place) => link.set_url(place).set_location(true),
                    None => link.set_url(fields[3]),
                };
                sheet.cell_mut((col + 1, row + 1)).set_hyperlink(link);
            }
            "F" if fields.len() >= 4 => conditionals.push(&record[2..]),
            // The workbook's date system, which `write' has taken.
            "Y" => {}
            // U and T, the sheet's tables, which `tables' has taken.
            "U" | "T" => {}
            // A, the filtered ranges, which `filters' has taken.
            "A" => {}
            // G type from to title names categories ref...: a chart
            "G" if fields.len() >= 8 => {
                sheet.add_chart(chart(&fields)?);
            }
            // W col width: a column sized by hand, in characters
            "W" if fields.len() >= 3 => {
                let col: u32 = fields[1].parse()?;
                sheet
                    .column_dimension_by_number_mut(col + 1)
                    .set_width(fields[2].parse::<f64>()?);
            }
            // P rows cols: the rows and columns kept in place
            "P" if fields.len() >= 3 => {
                freeze(sheet, fields[1].parse()?, fields[2].parse()?);
            }
            // H r|c index: a hidden row or column
            "H" if fields.len() >= 3 => {
                let index: u32 = fields[2].parse()?;
                if fields[1] == "r" {
                    sheet.row_dimension_mut(index + 1).set_hidden(true);
                } else {
                    sheet.column_dimension_by_number_mut(index + 1).set_hidden(true);
                }
            }
            // V range formula: the cells of RANGE take the values of a list
            "V" if fields.len() >= 3 => {
                let mut validation = DataValidation::default();
                validation.set_type(DataValidationValues::List);
                validation.set_allow_blank(true);
                validation.set_show_error_message(true);
                validation.sequence_of_references_mut().set_sqref(fields[1]);
                validation.set_formula1(fields[2]);
                validations.add_data_validation_list(validation);
            }
            "" => {}
            other => return Err(fail(format!("malformed record {}", other))),
        }
    }
    if !validations.data_validation_list().is_empty() {
        sheet.set_data_validations(validations);
    }
    // grid-table lets a later rule win; in a workbook priority 1 wins.
    let count = conditionals.len() as i32;
    for (index, record) in conditionals.iter().enumerate() {
        let fields: Vec<&str> = record.split(FIELD).collect();
        sheet.add_conditional_formatting_collection(conditional(&fields, count - index as i32)?);
    }
    Ok(())
}

/// What grid-table takes from a chart of a workbook.
struct ChartInfo {
    /// The grid-table type, such as column or bar-stacked
    kind: String,
    title: String,
    /// The zero-based (ROW, COL) of the cells the chart's corners sit on
    from: (u32, u32),
    to: (u32, u32),
    /// The ranges the chart reads, each with what it holds: v values,
    /// x the x values of a scatter chart, c categories, n a series name
    refs: Vec<(char, String)>,
    /// Each series' name as the chart gives it: its cells, its text, or
    /// none
    names: Vec<SeriesName>,
}

/// The name of a series of a chart.
enum SeriesName {
    None,
    Cells(String),
    Text(String),
}

/// Describe CHART of the sheet named SHEET, or None when grid-table
/// cannot hold it: a type it does not draw, or data on another sheet.
fn chart_info(chart: &Chart, sheet: &str) -> Option<ChartInfo> {
    let space = chart.chart_space().chart();
    let plot = space.plot_area();
    let (kind, list) = if let Some(bar) = plot.bar_chart() {
        let horizontal = matches!(bar.bar_direction().val(), BarDirectionValues::Bar);
        let stacked = matches!(bar.grouping().val(), GroupingValues::Stacked);
        let kind = match (horizontal, stacked) {
            (false, false) => "column",
            (false, true) => "column-stacked",
            (true, false) => "bar",
            (true, true) => "bar-stacked",
        };
        (kind, bar.area_chart_series_list())
    } else if let Some(line) = plot.line_chart() {
        ("line", line.area_chart_series_list())
    } else if let Some(area) = plot.area_chart() {
        ("area", area.area_chart_series_list())
    } else if let Some(pie) = plot.pie_chart() {
        ("pie", pie.area_chart_series_list())
    } else if let Some(doughnut) = plot.doughnut_chart() {
        ("doughnut", doughnut.area_chart_series_list())
    } else if let Some(scatter) = plot.scatter_chart() {
        ("scatter", scatter.area_chart_series_list())
    } else {
        return None;
    };
    let mut refs = Vec::new();
    let mut names = Vec::new();
    for series in list.area_chart_series() {
        let text = series.chart_text();
        names.push(
            if let Some(reference) = text.and_then(|t| t.string_reference()) {
                SeriesName::Cells(reference.formula().address().range().range())
            } else if let Some(value) = text.and_then(|t| t.numeric_value()) {
                SeriesName::Text(value.text().to_string())
            } else if let Some(rich) = text.and_then(|t| t.rich_text()) {
                SeriesName::Text(
                    rich.paragraph()
                        .iter()
                        .flat_map(|p| p.run().iter().map(|r| r.text().to_string()))
                        .collect(),
                )
            } else {
                SeriesName::None
            },
        );
        let formulas = [
            ('v', series.values().map(|v| v.number_reference().formula())),
            ('x', series.x_values().map(|v| v.number_reference().formula())),
            ('v', series.y_values().map(|v| v.number_reference().formula())),
            (
                'c',
                series
                    .category_axis_data()
                    .and_then(|c| c.string_reference())
                    .map(|r| r.formula()),
            ),
            (
                'c',
                series
                    .category_axis_data()
                    .and_then(|c| c.number_reference())
                    .map(|r| r.formula()),
            ),
            (
                'n',
                series
                    .chart_text()
                    .and_then(|t| t.string_reference())
                    .map(|r| r.formula()),
            ),
        ];
        for (role, formula) in formulas {
            let Some(formula) = formula else { continue };
            // A defined name or a constant: the workbook keeps the chart.
            if !umya_spreadsheet::helper::address::is_address(formula.address_str()) {
                return None;
            }
            let owner = formula.address().sheet_name();
            if !owner.is_empty() && owner != sheet {
                return None;
            }
            refs.push((role, formula.address().range().range()));
        }
    }
    if refs.is_empty() {
        return None;
    }
    let title = space
        .title()
        .and_then(|t| t.chart_text())
        .and_then(|t| t.rich_text())
        .map(|rich| {
            rich.paragraph()
                .iter()
                .flat_map(|p| p.run().iter().map(|r| r.text().to_string()))
                .collect::<String>()
        })
        .unwrap_or_default();
    let anchor = chart.two_cell_anchor();
    Some(ChartInfo {
        kind: kind.to_string(),
        title,
        from: (anchor.from_marker().row(), anchor.from_marker().col()),
        to: (anchor.to_marker().row(), anchor.to_marker().col()),
        refs,
        names,
    })
}

/// The names of the sheets CHART reads cells from.
fn chart_sheets(chart: &Chart) -> Vec<String> {
    let mut names = Vec::new();
    let plot = chart.chart_space().chart().plot_area();
    let lists = [
        plot.bar_chart().map(|c| c.area_chart_series_list()),
        plot.line_chart().map(|c| c.area_chart_series_list()),
        plot.area_chart().map(|c| c.area_chart_series_list()),
        plot.pie_chart().map(|c| c.area_chart_series_list()),
        plot.doughnut_chart().map(|c| c.area_chart_series_list()),
        plot.scatter_chart().map(|c| c.area_chart_series_list()),
        plot.radar_chart().map(|c| c.area_chart_series_list()),
    ];
    for list in lists.into_iter().flatten() {
        for series in list.area_chart_series() {
            let formulas = [
                series.values().map(|v| v.number_reference().formula()),
                series.x_values().map(|v| v.number_reference().formula()),
                series.y_values().map(|v| v.number_reference().formula()),
                series
                    .category_axis_data()
                    .and_then(|c| c.string_reference())
                    .map(|r| r.formula()),
            ];
            for formula in formulas.into_iter().flatten() {
                let name = formula.address().sheet_name().to_string();
                if !name.is_empty() && !names.contains(&name) {
                    names.push(name);
                }
            }
        }
    }
    names
}

/// Build the chart a G record describes.
///
/// FIELDS are: G, the grid-table type, the cells its top left and bottom
/// right corners sit on, its title, the cells naming the series (items
/// of a list) and the range of the category labels, then the data ranges as the
/// chart type takes
/// them: one per series, or x and y ranges in turn for a scatter chart,
/// labels and values in turn for each ring of a doughnut.
fn chart(fields: &[&str]) -> Result<Chart> {
    let kind = fields[1];
    let chart_type = match kind {
        "column" | "column-stacked" | "bar" | "bar-stacked" => ChartType::BarChart,
        "line" => ChartType::LineChart,
        "area" => ChartType::AreaChart,
        "pie" => ChartType::PieChart,
        "doughnut" => ChartType::DoughnutChart,
        "scatter" => ChartType::ScatterChart,
        other => return Err(fail(format!("unknown chart type {}", other))),
    };
    let mut from = MarkerType::default();
    from.set_coordinate(fields[2]);
    let mut to = MarkerType::default();
    to.set_coordinate(fields[3]);
    let mut chart = Chart::default();
    chart.new_chart(&chart_type, from, to, fields[7..].to_vec());
    let stacked = kind.ends_with("stacked");
    let plot = chart.plot_area_mut();
    if let Some(bar) = plot.bar_chart_mut() {
        bar.bar_direction_mut().set_val(if kind.starts_with("bar") {
            BarDirectionValues::Bar
        } else {
            BarDirectionValues::Column
        });
        // The library has no clustered grouping; standard with no
        // overlap draws the series side by side.
        bar.grouping_mut().set_val(if stacked {
            GroupingValues::Stacked
        } else {
            GroupingValues::Standard
        });
        bar.overlap_mut().set_val(if stacked { 100 } else { 0 });
    }
    if let Some(line) = plot.line_chart_mut() {
        line.grouping_mut().set_val(GroupingValues::Standard);
        for series in line.area_chart_series_list_mut().area_chart_series_mut() {
            if let Some(smooth) = series.smooth_mut() {
                smooth.set_val(false);
            }
        }
    }
    if let Some(area) = plot.area_chart_mut() {
        area.grouping_mut().set_val(GroupingValues::Standard);
    }
    if let Some(scatter) = plot.scatter_chart_mut() {
        scatter.scatter_style_mut().set_val(ScatterStyleValues::LineMarker);
        // Without the element a reader may smooth the lines.
        for series in scatter.area_chart_series_list_mut().area_chart_series_mut() {
            let mut smooth = Smooth::default();
            smooth.set_val(false);
            series.set_smooth(smooth);
        }
    }
    let items = |field: &str| -> Vec<String> {
        if field.is_empty() {
            Vec::new()
        } else {
            field.split(ITEM).map(String::from).collect()
        }
    };
    // The series names are read from their cells too; an empty item is
    // a series with no name, and one starting with " a name of its own.
    let names = items(fields[5]);
    let list = chart.area_chart_series_list_mut().area_chart_series_mut();
    for (series, name) in list.iter_mut().zip(names.iter()) {
        let mut text = ChartText::default();
        if name.is_empty() {
            continue;
        } else if let Some(literal) = name.strip_prefix('"') {
            let mut value = NumericValue::default();
            value.set_text(literal);
            text.set_numeric_value(value);
        } else {
            let mut reference = StringReference::default();
            reference.formula_mut().set_address_str(name);
            text.set_string_reference(reference);
        }
        series.set_chart_text(text);
    }
    // A doughnut is built from labels and values in turn; with no labels
    // its values stood in for them, and its series keep only the values.
    if kind == "doughnut" && fields[6].is_empty() {
        for series in chart.area_chart_series_list_mut().area_chart_series_mut() {
            let mut bare = AreaChartSeries::default();
            if let Some(values) = series.values() {
                bare.set_values(values.clone());
            }
            if let Some(text) = series.chart_text() {
                bare.set_chart_text(text.clone());
            }
            bare.index_mut().set_val(series.index().val());
            bare.order_mut().set_val(series.order().val());
            *series = bare;
        }
    }
    // The labels are read from their cells, as the values are.
    if !fields[6].is_empty() && kind != "doughnut" && kind != "scatter" {
        for series in chart.area_chart_series_list_mut().area_chart_series_mut() {
            let mut reference = StringReference::default();
            reference.formula_mut().set_address_str(fields[6]);
            let mut labels = CategoryAxisData::default();
            labels.set_string_reference(reference);
            series.set_category_axis_data(labels);
        }
    }
    if !fields[4].is_empty() {
        chart.set_title(fields[4]);
    }
    Ok(chart)
}

/// Whether NAME is one the workbook uses for itself, as a print area.
fn builtin_name(name: &DefinedName) -> bool {
    name.name().starts_with("_xlnm.") || name.hidden() || name.has_local_sheet_id()
}

/// Return the names defined in the workbook of the xlsx file PATH, as a
/// printed list of (NAME DEFINITION SHEET): SHEET is the name of the sheet
/// a name belongs to, or nil for one of the whole workbook.  The names the
/// workbook keeps for itself, as print areas, and hidden ones are left out.
#[defun]
fn names(path: String) -> Result<String> {
    let book = reader::xlsx::lazy_read(Path::new(&path)).map_err(fail)?;
    let sheets = book.sheet_collection_no_check();
    let mut out = String::from("(");
    let mut push = |name: &DefinedName, sheet: Option<&str>| {
        if name.name().starts_with("_xlnm.") || name.hidden() {
            return;
        }
        out.push('(');
        quote(&mut out, name.name());
        out.push(' ');
        quote(&mut out, &name.address());
        match sheet {
            Some(sheet) => {
                out.push(' ');
                quote(&mut out, sheet);
            }
            None => out.push_str(" nil"),
        }
        out.push(')');
    };
    // umya keeps a name with the sheet its reference points into, so
    // whether a name belongs to one sheet is told by its localSheetId,
    // not by where umya keeps it.
    let names = book
        .defined_names()
        .iter()
        .chain(sheets.iter().flat_map(|sheet| sheet.defined_names().iter()));
    for name in names {
        if name.has_local_sheet_id() {
            match sheets.get(name.local_sheet_id() as usize) {
                Some(sheet) => push(name, Some(sheet.name())),
                None => continue,
            }
        } else {
            push(name, None);
        }
    }
    out.push(')');
    Ok(out)
}

/// Give the xlsx file PATH the workbook names of PAYLOAD, records of NAME
/// and REFERENCE fields, in place of the ones it had.
#[defun]
fn set_names(path: String, payload: String) -> Result<()> {
    let mut book = reader::xlsx::read(Path::new(&path)).map_err(fail)?;
    book.defined_names_mut().retain(|name| builtin_name(name));
    for sheet in book.sheet_collection_mut() {
        sheet.defined_names_mut().retain(|name| builtin_name(name));
    }
    for record in payload.split(RECORD) {
        let fields: Vec<&str> = record.split(FIELD).collect();
        if fields.len() >= 2 && !fields[0].is_empty() {
            let mut name = DefinedName::default();
            name.set_name(fields[0]);
            name.set_address(fields[1]);
            book.add_defined_names(name);
        }
    }
    writer::xlsx::write(&book, Path::new(&path)).map_err(fail)?;
    Ok(())
}

/// Remove from the xlsx file PATH every sheet not named in NAMES, a list
/// of names separated by U+001E.  At least one sheet is always kept.
#[defun]
fn prune(path: String, names: String) -> Result<()> {
    let keep: Vec<&str> = names.split(RECORD).collect();
    let mut book = reader::xlsx::read(Path::new(&path)).map_err(fail)?;
    let present: Vec<String> = book
        .sheet_collection()
        .iter()
        .map(|sheet| sheet.name().to_string())
        .collect();
    let mut left = present.len();
    for name in present {
        if left > 1 && !keep.contains(&name.as_str()) {
            book.remove_sheet_by_name(&name).map_err(fail)?;
            left -= 1;
        }
    }
    writer::xlsx::write(&book, Path::new(&path)).map_err(fail)?;
    Ok(())
}

/// Write PAYLOAD as sheet SHEET of the xlsx file PATH.
/// With BASE, an existing xlsx file, the result is that workbook with
/// SHEET replaced or added and its other sheets kept.  PAYLOAD is records
/// separated by U+001E with fields separated by U+001F:
/// C ROW COL KIND TEXT BOLD ITALIC COLOR FILL ALIGN, M RANGE and
/// F RANGE OP FORMULA TEXT BOLD ITALIC COLOR FILL, and Y 1900 or Y 1904
/// for the workbook's date system.
#[defun]
fn write(path: String, base: Option<String>, sheet: String, payload: String) -> Result<()> {
    let mut book: Workbook = match &base {
        Some(base) => {
            let mut book = reader::xlsx::lazy_read(Path::new(base)).map_err(fail)?;
            if has_sheet(&book, &sheet) {
                book.read_sheet_by_name(&sheet);
                // Writing a chart reads the cells it draws, so the sheets
                // the kept charts read from must be loaded too.
                let others: Vec<String> = book
                    .sheet_by_name(&sheet)
                    .map(|target| {
                        target
                            .chart_collection()
                            .iter()
                            .flat_map(chart_sheets)
                            .collect()
                    })
                    .unwrap_or_default();
                for other in others {
                    if other != sheet && has_sheet(&book, &other) {
                        book.read_sheet_by_name(&other);
                    }
                }
            }
            book
        }
        None => {
            let mut book = umya_spreadsheet::new_file();
            if !has_sheet(&book, &sheet) {
                book.set_sheet_name(0, sheet.as_str()).map_err(fail)?;
            }
            book
        }
    };
    if !has_sheet(&book, &sheet) {
        book.new_sheet(sheet.as_str()).map_err(fail)?;
    }
    // Y 1904 or Y 1900 gives the workbook its date system.
    for record in payload.split(RECORD) {
        let fields: Vec<&str> = record.split(FIELD).collect();
        if fields.len() >= 2 && fields[0] == "Y" {
            book.set_date1904(fields[1] == "1904");
        }
    }
    let target = book.sheet_by_name_mut(&sheet).map_err(fail)?;
    clear(target);
    fill(target, &payload)?;
    tables(target, &payload);
    filters(target, &payload);
    writer::xlsx::write(&book, Path::new(&path)).map_err(fail)?;
    Ok(())
}
