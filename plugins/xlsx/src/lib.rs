//! Emacs dynamic module that reads and writes xlsx files for grid-table.
//!
//! The module knows nothing about grid-table's model.  Reading returns a
//! printed Lisp form describing one sheet; writing takes a flat text of
//! records.  `plugins/grid-table-xlsx.el` does the mapping on both sides.

use emacs::{defun, Env, Result};
use std::fmt::Write;
use std::path::Path;
use umya_spreadsheet::{
    reader, writer, Color, ConditionalFormatValues, ConditionalFormatting,
    ConditionalFormattingOperatorValues, ConditionalFormattingRule, Formula,
    HorizontalAlignmentValues, PatternValues, Style, Workbook, Worksheet,
};
use umya_spreadsheet::structs::drawing::Theme;

emacs::plugin_is_GPL_compatible!();

/// Separates the records of a write payload.
const RECORD: char = '\u{1e}';
/// Separates the fields of a record.
const FIELD: char = '\u{1f}';

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
        if cell.is_formula() {
            quote(out, &format!("={}", cell.formula()));
        } else {
            let value = cell.value();
            quote(out, &value);
            let style = cell.style();
            if let Some(format) = style.number_format() {
                let code = format.format_code();
                if code != "General" && !value.is_empty() {
                    keyword(out, ":format", code);
                }
            }
        }
        push_style(out, cell.style(), theme, false);
        out.push(')');
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
    out.push_str("))");
    Ok(())
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

/// Give STYLE the look of FIELDS: bold, italic, color, fill, align.
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
    sheet.set_conditional_formatting_collection(Vec::new());
}

fn fill(sheet: &mut Worksheet, payload: &str) -> Result<()> {
    let mut conditionals = Vec::new();
    for record in payload.split(RECORD) {
        let fields: Vec<&str> = record.split(FIELD).collect();
        match fields[0] {
            // C row col kind text bold italic color fill align
            "C" if fields.len() >= 5 => {
                let row: u32 = fields[1].parse()?;
                let col: u32 = fields[2].parse()?;
                let at = (col + 1, row + 1);
                let text = fields[4];
                if !text.is_empty() {
                    let cell = sheet.cell_mut(at);
                    match fields[3] {
                        "f" => {
                            cell.set_formula(text);
                        }
                        "n" => {
                            cell.set_value_number(text.parse::<f64>()?);
                        }
                        "b" => {
                            cell.set_value_bool(text == "TRUE");
                        }
                        _ => {
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
            "F" if fields.len() >= 4 => conditionals.push(&record[2..]),
            "" => {}
            other => return Err(fail(format!("malformed record {}", other))),
        }
    }
    // grid-table lets a later rule win; in a workbook priority 1 wins.
    let count = conditionals.len() as i32;
    for (index, record) in conditionals.iter().enumerate() {
        let fields: Vec<&str> = record.split(FIELD).collect();
        sheet.add_conditional_formatting_collection(conditional(&fields, count - index as i32)?);
    }
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
/// F RANGE OP FORMULA TEXT BOLD ITALIC COLOR FILL.
#[defun]
fn write(path: String, base: Option<String>, sheet: String, payload: String) -> Result<()> {
    let mut book: Workbook = match &base {
        Some(base) => {
            let mut book = reader::xlsx::lazy_read(Path::new(base)).map_err(fail)?;
            if has_sheet(&book, &sheet) {
                book.read_sheet_by_name(&sheet);
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
    let target = book.sheet_by_name_mut(&sheet).map_err(fail)?;
    clear(target);
    fill(target, &payload)?;
    writer::xlsx::write(&book, Path::new(&path)).map_err(fail)?;
    Ok(())
}
