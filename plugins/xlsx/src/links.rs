//! External links, which umya-spreadsheet has no model for.
//!
//! A workbook's external links are parts of their own, named in the order
//! `xl/workbook.xml` gives them, `<externalReferences>` holding one
//! `<externalReference r:id>` for each, and each `r:id` a relationship in
//! `xl/_rels/workbook.xml.rels` to a part under `xl/externalLinks/`.
//!
//! Reading a workbook into umya and writing it out loses all of that,
//! while the `[1]`, `[2]` its formulas and names hold stay with it, so a
//! written workbook is given the links of the one it was written from
//! here: the parts are copied as they were, in the order the source named
//! them, and numbered anew among the relationships the write made, as the
//! write numbers those from `rId1` again.

use std::fmt::Write as _;
use std::fs::{self, File};
use std::io::Read;
use std::io::Write as _;
use std::path::{Path, PathBuf};

use quick_xml::events::Event;
use quick_xml::Reader;
use umya_spreadsheet::helper::coordinate::index_from_coordinate;

use crate::quote;

const WORKBOOK: &str = "xl/workbook.xml";
const WORKBOOK_RELS: &str = "xl/_rels/workbook.xml.rels";
const CONTENT_TYPES: &str = "[Content_Types].xml";
/// The relationships an external link may have.
const EXTERNAL_LINK_RELATIONSHIP: &str =
    "http://schemas.openxmlformats.org/officeDocument/2006/relationships/externalLink";
const EXTERNAL_LINK_CONTENT_TYPE: &str =
    "application/vnd.openxmlformats-officedocument.spreadsheetml.externalLink+xml";

/// A part of the package, as it was read.
struct Part {
    name: String,
    bytes: Vec<u8>,
}

/// One external link of a workbook.
struct Link {
    /// Its part, as the workbook's relationship names it: externalLink1.xml.
    part: Part,
    /// The relationship part of that part, which is copied too.
    rels: Option<Part>,
}

/// The external links of a workbook, in the order it names them.
pub(crate) struct Links {
    links: Vec<Link>,
    /// Parts of xl/externalLinks the workbook names no more, kept as they
    /// are rather than dropped.
    others: Vec<Part>,
}

impl Links {
    fn is_empty(&self) -> bool {
        self.links.is_empty() && self.others.is_empty()
    }

    /// Return the target of each link, as its relationship gives it.
    pub(crate) fn targets(&self) -> Vec<String> {
        self.links
            .iter()
            .map(|link| {
                link.part
                    .name
                    .strip_prefix("xl/")
                    .unwrap_or(&link.part.name)
                    .to_string()
            })
            .collect()
    }
}

/// Give WRITTEN, just written from SOURCE, the external links SOURCE had.
pub(crate) fn keep(source: &Path, written: &Path) -> Result<(), String> {
    splice(&read(source)?, written)
}

/// Return the external links of the xlsx file PATH, in the order it names them.
pub(crate) fn read(path: &Path) -> Result<Links, String> {
    let file = File::open(path).map_err(|error| format!("{path:?}: {error}"))?;
    let mut zip = zip::ZipArchive::new(file).map_err(|error| format!("{path:?}: {error}"))?;
    let mut links = Links { links: Vec::new(), others: Vec::new() };
    let workbook = match entry(&mut zip, WORKBOOK)? {
        Some(workbook) => workbook,
        None => return Ok(links),
    };
    let ids = external_reference_ids(&workbook);
    if ids.is_empty() {
        return Ok(links);
    }
    let rels = match entry(&mut zip, WORKBOOK_RELS)? {
        Some(rels) => rels,
        None => return Err(format!("{path:?}: no {WORKBOOK_RELS}")),
    };
    let relationships = relationships(&rels);
    for id in ids {
        let target = relationships
            .iter()
            .find(|(other, kind, _)| other == &id && kind == EXTERNAL_LINK_RELATIONSHIP)
            .map(|(_, _, target)| target.clone())
            .ok_or_else(|| format!("{path:?}: no external link for {id}"))?;
        let name = part_name(&target);
        let bytes = entry(&mut zip, &name)?
            .ok_or_else(|| format!("{path:?}: no part {name}"))?;
        let rels_name = relationship_part(&name);
        let rels = entry(&mut zip, &rels_name)?
            .map(|bytes| Part { name: rels_name.clone(), bytes });
        links.links.push(Link { part: Part { name, bytes }, rels });
    }
    // A part of xl/externalLinks the workbook names no more is kept too.
    let named: Vec<String> = links
        .links
        .iter()
        .flat_map(|link| {
            let mut names = vec![link.part.name.clone()];
            if let Some(rels) = &link.rels {
                names.push(rels.name.clone());
            }
            names
        })
        .collect();
    let mut names: Vec<String> = zip
        .file_names()
        .filter(|name| name.starts_with("xl/externalLinks/") && name.ends_with(".xml"))
        .map(ToOwned::to_owned)
        .collect();
    names.sort();
    for name in names {
        if named.contains(&name) {
            continue;
        }
        if let Some(bytes) = entry(&mut zip, &name)? {
            links.others.push(Part { name, bytes });
        }
    }
    Ok(links)
}

/// Return the bytes of the part NAME of ZIP, when it has one.
fn entry<R: Read + std::io::Seek>(
    zip: &mut zip::ZipArchive<R>,
    name: &str,
) -> Result<Option<Vec<u8>>, String> {
    let index = match zip.index_for_name(name) {
        Some(index) => index,
        None => return Ok(None),
    };
    let mut file = zip
        .by_index(index)
        .map_err(|error| format!("{name}: {error}"))?;
    let mut bytes = Vec::new();
    file.read_to_end(&mut bytes)
        .map_err(|error| format!("{name}: {error}"))?;
    Ok(Some(bytes))
}

/// Return the name of the part TARGET names, relative to the workbook.
fn part_name(target: &str) -> String {
    let target = target.strip_prefix('/').unwrap_or(target);
    if target.starts_with("xl/") {
        target.to_string()
    } else {
        format!("xl/{target}")
    }
}

/// Return the name of the relationship part of PART.
fn relationship_part(part: &str) -> String {
    match part.rsplit_once('/') {
        Some((dir, file)) => format!("{dir}/_rels/{file}.rels"),
        None => format!("_rels/{part}.rels"),
    }
}

/// Return the r:ids of the `<externalReference>`s of a workbook.xml, in order.
fn external_reference_ids(xml: &[u8]) -> Vec<String> {
    let mut reader = Reader::from_reader(xml);
    reader.config_mut().trim_text(true);
    let mut ids = Vec::new();
    let mut inside = false;
    let mut buf = Vec::new();
    loop {
        match reader.read_event_into(&mut buf) {
            Ok(Event::Start(ref event)) => {
                if event.local_name().as_ref() == b"externalReferences" {
                    inside = true;
                }
            }
            Ok(Event::End(ref event)) => {
                if event.local_name().as_ref() == b"externalReferences" {
                    inside = false;
                }
            }
            Ok(Event::Empty(ref event)) => {
                if inside && event.local_name().as_ref() == b"externalReference" {
                    if let Some(id) = attribute(event, b"id") {
                        ids.push(id);
                    }
                }
            }
            Ok(Event::Eof) | Err(_) => break,
            _ => {}
        }
        buf.clear();
    }
    ids
}

/// Return the (Id, Type, Target) of each relationship of a rels part.
fn relationships(xml: &[u8]) -> Vec<(String, String, String)> {
    let mut reader = Reader::from_reader(xml);
    reader.config_mut().trim_text(true);
    let mut found = Vec::new();
    let mut buf = Vec::new();
    loop {
        match reader.read_event_into(&mut buf) {
            Ok(Event::Start(ref event)) | Ok(Event::Empty(ref event)) => {
                if event.local_name().as_ref() == b"Relationship" {
                    if let (Some(id), Some(kind), Some(target)) = (
                        attribute(event, b"Id"),
                        attribute(event, b"Type"),
                        attribute(event, b"Target"),
                    ) {
                        found.push((id, kind, target));
                    }
                }
            }
            Ok(Event::Eof) | Err(_) => break,
            _ => {}
        }
        buf.clear();
    }
    found
}

/// Return the value of the attribute LOCAL of EVENT, prefixes aside.
fn attribute(event: &quick_xml::events::BytesStart, local: &[u8]) -> Option<String> {
    event
        .attributes()
        .flatten()
        .find(|attribute| attribute.key.local_name().as_ref() == local)
        .map(|attribute| String::from_utf8_lossy(&attribute.value).into_owned())
}

/// Return the greatest rIdN number of a rels part, 0 when it has none.
fn greatest_id(xml: &str) -> usize {
    relationships(xml.as_bytes())
        .iter()
        .filter_map(|(id, _, _)| id.strip_prefix("rId").and_then(|number| number.parse().ok()))
        .max()
        .unwrap_or(0)
}

/// Write the external links LINKS into WRITTEN, a workbook just written.
pub(crate) fn splice(links: &Links, written: &Path) -> Result<(), String> {
    if links.is_empty() {
        return Ok(());
    }
    let temporary = named_beside(written, ".links.tmp");
    match splice_into(links, written, &temporary) {
        Ok(()) => fs::rename(&temporary, written)
            .map_err(|error| format!("{temporary:?}: {error}")),
        Err(error) => {
            let _ = fs::remove_file(&temporary);
            Err(error)
        }
    }
}

/// Return a name beside PATH, PATH's own with SUFFIX on it.
fn named_beside(path: &Path, suffix: &str) -> PathBuf {
    let mut name = path.file_name().unwrap_or_default().to_os_string();
    name.push(suffix);
    path.with_file_name(name)
}

/// Write PATH's parts into TEMPORARY, with the external links LINKS put in.
fn splice_into(links: &Links, path: &Path, temporary: &Path) -> Result<(), String> {
    let input = File::open(path).map_err(|error| format!("{path:?}: {error}"))?;
    let mut zip = zip::ZipArchive::new(input).map_err(|error| format!("{path:?}: {error}"))?;
    let workbook = entry(&mut zip, WORKBOOK)?.ok_or_else(|| format!("{path:?}: no {WORKBOOK}"))?;
    let rels = entry(&mut zip, WORKBOOK_RELS)?
        .ok_or_else(|| format!("{path:?}: no {WORKBOOK_RELS}"))?;
    let types = entry(&mut zip, CONTENT_TYPES)?
        .ok_or_else(|| format!("{path:?}: no {CONTENT_TYPES}"))?;
    let workbook = String::from_utf8(workbook).map_err(|error| format!("{WORKBOOK}: {error}"))?;
    let rels = String::from_utf8(rels).map_err(|error| format!("{WORKBOOK_RELS}: {error}"))?;
    let types = String::from_utf8(types).map_err(|error| format!("{CONTENT_TYPES}: {error}"))?;
    // A workbook that keeps its own links already wants no more.
    if workbook.contains("<externalReferences") {
        return Ok(());
    }
    let first = greatest_id(&rels) + 1;
    let mut references = String::new();
    let mut relationships = String::new();
    let mut overrides = String::new();
    for (offset, link) in links.links.iter().enumerate() {
        let id = first + offset;
        let _ = write!(references, "<externalReference r:id=\"rId{id}\"/>");
        let _ = write!(
            relationships,
            "<Relationship Id=\"rId{id}\" Type=\"{EXTERNAL_LINK_RELATIONSHIP}\" \
             Target=\"{}\"/>",
            xml_escape(&link.part.name["xl/".len()..])
        );
    }
    for part in links.links.iter().map(|link| &link.part).chain(&links.others) {
        let _ = write!(
            overrides,
            "<Override PartName=\"/{}\" ContentType=\"{EXTERNAL_LINK_CONTENT_TYPE}\"/>",
            xml_escape(&part.name)
        );
    }
    // The schema puts <externalReferences> just after the sheets.
    let workbook = insert_after(
        &workbook,
        "</sheets>",
        &format!("<externalReferences>{references}</externalReferences>"),
        &["<calcPr", "<definedNames", "</workbook>"],
    )?;
    let rels = insert_before(&rels, "</Relationships>", &relationships, &[])?;
    let types = insert_before(&types, "</Types>", &overrides, &[])?;
    let workbook = workbook.into_bytes();
    let rels = rels.into_bytes();
    let types = types.into_bytes();

    let output = File::create(temporary).map_err(|error| format!("{temporary:?}: {error}"))?;
    let mut writer = zip::ZipWriter::new(output);
    let mut written: Vec<String> = Vec::new();
    for index in 0..zip.len() {
        let mut file = zip
            .by_index(index)
            .map_err(|error| format!("{path:?}: {error}"))?;
        if file.is_dir() {
            continue;
        }
        let name = file.name().to_string();
        let method = file.compression();
        let mut bytes = Vec::new();
        file.read_to_end(&mut bytes)
            .map_err(|error| format!("{name}: {error}"))?;
        drop(file);
        let bytes = match name.as_str() {
            WORKBOOK => workbook.clone(),
            WORKBOOK_RELS => rels.clone(),
            CONTENT_TYPES => types.clone(),
            _ => bytes,
        };
        add(&mut writer, &name, &bytes, method)?;
        written.push(name);
    }
    let mut parts: Vec<&Part> = Vec::new();
    for link in &links.links {
        parts.push(&link.part);
        parts.extend(link.rels.as_ref());
    }
    parts.extend(&links.others);
    for part in parts {
        if !written.contains(&part.name) {
            add(
                &mut writer,
                &part.name,
                &part.bytes,
                zip::CompressionMethod::Deflated,
            )?;
            written.push(part.name.clone());
        }
    }
    writer
        .finish()
        .map_err(|error| format!("{temporary:?}: {error}"))?;
    Ok(())
}

/// Add the part NAME of BYTES, as METHOD compresses it, to WRITER.
fn add<W: std::io::Write + std::io::Seek>(
    writer: &mut zip::ZipWriter<W>,
    name: &str,
    bytes: &[u8],
    method: zip::CompressionMethod,
) -> Result<(), String> {
    let options = zip::write::SimpleFileOptions::default().compression_method(method);
    writer
        .start_file(name, options)
        .map_err(|error| format!("{name}: {error}"))?;
    writer
        .write_all(bytes)
        .map_err(|error| format!("{name}: {error}"))
}

/// Return XML, INSERT put in just after the first of AFTER, or else
/// before the first of ELSE_BEFORE.
fn insert_after(
    xml: &str,
    after: &str,
    insert: &str,
    else_before: &[&str],
) -> Result<String, String> {
    let at = match xml.find(after) {
        Some(at) => at + after.len(),
        None => match else_before.iter().filter_map(|other| xml.find(other)).min() {
            Some(at) => at,
            None => {
                return Err(format!(
                    "no {after:?} nor {else_before:?} to put {insert:?} at"
                ))
            }
        },
    };
    let mut out = String::with_capacity(xml.len() + insert.len());
    out.push_str(&xml[..at]);
    out.push_str(insert);
    out.push_str(&xml[at..]);
    Ok(out)
}

/// Return XML, INSERT put in before the first of CLOSE.
fn insert_before(
    xml: &str,
    close: &str,
    insert: &str,
    else_where: &[&str],
) -> Result<String, String> {
    let mut places: Vec<usize> = else_where
        .iter()
        .filter_map(|other| xml.find(other))
        .collect();
    match xml.find(close) {
        Some(at) => places.push(at),
        None if places.is_empty() => {
            return Err(format!("no {close:?} to put {insert:?} before"));
        }
        None => {}
    }
    let at = places.into_iter().min().unwrap();
    let mut out = String::with_capacity(xml.len() + insert.len());
    out.push_str(&xml[..at]);
    out.push_str(insert);
    out.push_str(&xml[at..]);
    Ok(out)
}

/// Return TEXT with what XML reads as markup written as itself.
fn xml_escape(text: &str) -> String {
    let mut out = String::with_capacity(text.len());
    for ch in text.chars() {
        match ch {
            '&' => out.push_str("&amp;"),
            '<' => out.push_str("&lt;"),
            '>' => out.push_str("&gt;"),
            '"' => out.push_str("&quot;"),
            _ => out.push(ch),
        }
    }
    out
}

//;----------------------------------------------------------------------
//; What the links hold, for reading their cached values
//;----------------------------------------------------------------------

/// A cell of a link's cached data: its row and column, zero-based, and
/// its kind, n for a number, s for a string, b for a boolean and e for an
/// error, with the value as the link writes it.
struct Cached {
    row: usize,
    col: usize,
    kind: char,
    value: String,
}

/// A name of another workbook the link keeps, as its externalLink part does:
/// the name, the sheet of the other workbook it belongs to, or nil for one
/// of the whole workbook, and what it is defined as, as the part writes it,
/// nil when the link keeps no definition, as a macro's name does.
struct NameDef {
    name: String,
    sheet: Option<String>,
    text: Option<String>,
}

/// The cached sheets of one external link, in the order it names them.
struct CachedSheets {
    sheets: Vec<(String, Vec<Cached>)>,
}

/// The links of a workbook as a Lisp form: (:links ((1 "target") ...)),
/// in the order the workbook counts them from 1.
pub(crate) fn link_list(path: &Path) -> Result<String, String> {
    let file = File::open(path).map_err(|error| format!("{path:?}: {error}"))?;
    let mut zip = zip::ZipArchive::new(file).map_err(|error| format!("{path:?}: {error}"))?;
    let mut out = String::from("(:links (");
    for (index, part) in referenced_parts(&mut zip, path)?.iter().enumerate() {
        // The file it points at is the first target of its own relationships.
        let target = entry(&mut zip, &relationship_part(part))?
            .map(|bytes| relationships(&bytes))
            .and_then(|found| found.into_iter().next().map(|(_, _, target)| target))
            .unwrap_or_default();
        if index > 0 {
            out.push(' ');
        }
        let _ = write!(out, "({} ", index + 1);
        quote(&mut out, &target);
        out.push(')');
    }
    out.push_str("))");
    Ok(out)
}

/// The cached sheets of the link numbered ORDER of the workbook at PATH, as
/// a Lisp form: (:sheets (("Sheet1" ((0 0 n 2.0) ...)) ...)).  A cell is
/// its row and column, zero-based, its kind and its value; a sheet is left
/// out whose refresh failed and that cached no cell, as `cached_sheets`
/// says.
pub(crate) fn link_sheets(path: &Path, order: usize) -> Result<String, String> {
    let file = File::open(path).map_err(|error| format!("{path:?}: {error}"))?;
    let mut zip = zip::ZipArchive::new(file).map_err(|error| format!("{path:?}: {error}"))?;
    let parts = referenced_parts(&mut zip, path)?;
    let part = order
        .checked_sub(1)
        .and_then(|at| parts.get(at))
        .ok_or_else(|| format!("{path:?}: no external link {order}"))?;
    let bytes = entry(&mut zip, part)?.ok_or_else(|| format!("{path:?}: no part {part}"))?;
    let cached = cached_sheets(&bytes);
    let mut out = String::from("(:sheets (");
    for (index, (name, cells)) in cached.sheets.iter().enumerate() {
        if index > 0 {
            out.push(' ');
        }
        out.push('(');
        quote(&mut out, name);
        out.push_str(" (");
        for (at, cell) in cells.iter().enumerate() {
            if at > 0 {
                out.push(' ');
            }
            let _ = write!(out, "({} {} {} ", cell.row, cell.col, cell.kind);
            match cell.kind {
                'n' => match cell.value.parse::<f64>() {
                    Ok(number) => {
                        let _ = write!(out, "{number:?}");
                    }
                    Err(_) => quote(&mut out, &cell.value),
                },
                'b' => out.push_str(if cell.value == "1" { ":true" } else { ":false" }),
                _ => quote(&mut out, &cell.value),
            }
            out.push(')');
        }
        out.push_str("))");
    }
    out.push_str("))");
    Ok(out)
}

/// The kind of the link numbered ORDER of the workbook at PATH, and the
/// names it defines, as a Lisp form: (:kind externalBook :names (("X" "Sheet"
/// "='Sheet'!$A$1") ...)).  A name's sheet is nil for one of the whole
/// workbook, and its text nil when the link keeps no definition.
pub(crate) fn link_names(path: &Path, order: usize) -> Result<String, String> {
    let file = File::open(path).map_err(|error| format!("{path:?}: {error}"))?;
    let mut zip = zip::ZipArchive::new(file).map_err(|error| format!("{path:?}: {error}"))?;
    let parts = referenced_parts(&mut zip, path)?;
    let part = order
        .checked_sub(1)
        .and_then(|at| parts.get(at))
        .ok_or_else(|| format!("{path:?}: no external link {order}"))?;
    let bytes = entry(&mut zip, part)?.ok_or_else(|| format!("{path:?}: no part {part}"))?;
    let (kind, names) = book_names(&bytes);
    let mut out = String::from("(:kind ");
    out.push_str(kind);
    out.push_str(" :names (");
    for (at, name) in names.iter().enumerate() {
        if at > 0 {
            out.push(' ');
        }
        out.push('(');
        quote(&mut out, &name.name);
        out.push(' ');
        match &name.sheet {
            Some(sheet) => quote(&mut out, sheet),
            None => out.push_str("nil"),
        }
        out.push(' ');
        match &name.text {
            Some(text) => quote(&mut out, text),
            None => out.push_str("nil"),
        }
        out.push(')');
    }
    out.push_str("))");
    Ok(out)
}

/// Return what kind of link the part XML holds, and the names it defines.
///
/// A link is an externalBook, whose names stand in a `<definedNames>` as
/// `<definedName name="X" refersTo="='Sheet1'!$A$1" sheetId="0"/>`, where
/// sheetId counts the `<sheetName>`s of the link from zero when the name
/// belongs to one sheet; a ddeLink or an oleLink has no names a formula
/// reads, and a sheet it names neither.
fn book_names(xml: &[u8]) -> (&'static str, Vec<NameDef>) {
    let mut reader = Reader::from_reader(xml);
    reader.config_mut().trim_text(true);
    let mut kind = "externalBook";
    let mut sheets: Vec<String> = Vec::new();
    let mut names: Vec<NameDef> = Vec::new();
    let mut text: Option<String> = None;
    let mut buf = Vec::new();
    loop {
        match reader.read_event_into(&mut buf) {
            Ok(Event::Start(ref event)) | Ok(Event::Empty(ref event)) => {
                match event.local_name().as_ref() {
                    b"externalBook" => kind = "externalBook",
                    b"ddeLink" => kind = "ddeLink",
                    b"oleLink" => kind = "oleLink",
                    b"sheetName" => sheets.push(attribute(event, b"val").unwrap_or_default()),
                    b"definedName" => {
                        let refers = attribute(event, b"refersTo");
                        let sheet = attribute(event, b"sheetId")
                            .and_then(|id| id.parse::<usize>().ok())
                            .and_then(|id| sheets.get(id).cloned());
                        names.push(NameDef {
                            name: attribute(event, b"name").unwrap_or_default(),
                            sheet,
                            text: refers,
                        });
                        text = Some(String::new());
                    }
                    _ => {}
                }
            }
            Ok(Event::Text(ref event)) => {
                if let Some(value) = text.as_mut() {
                    if let Ok(decoded) = event.decode() {
                        value.push_str(&decoded);
                    }
                }
            }
            Ok(Event::End(ref event)) => {
                if event.local_name().as_ref() == b"definedName" {
                    // A part that writes the definition as its text, as the
                    // workbook does, rather than as refersTo.
                    if let Some(value) = text.take() {
                        if let Some(name) = names.last_mut() {
                            if name.text.as_deref().unwrap_or_default().is_empty()
                                && !value.is_empty()
                            {
                                name.text = Some(value);
                            }
                        }
                    }
                }
            }
            Ok(Event::Eof) | Err(_) => break,
            _ => {}
        }
        buf.clear();
    }
    (kind, names)
}

/// Return the parts of the external links of ZIP, in the order it names them.
fn referenced_parts<R: Read + std::io::Seek>(
    zip: &mut zip::ZipArchive<R>,
    path: &Path,
) -> Result<Vec<String>, String> {
    let workbook = match entry(zip, WORKBOOK)? {
        Some(workbook) => workbook,
        None => return Ok(Vec::new()),
    };
    let ids = external_reference_ids(&workbook);
    if ids.is_empty() {
        return Ok(Vec::new());
    }
    let rels = match entry(zip, WORKBOOK_RELS)? {
        Some(rels) => rels,
        None => return Err(format!("{path:?}: no {WORKBOOK_RELS}")),
    };
    let relationships = relationships(&rels);
    ids.iter()
        .map(|id| {
            relationships
                .iter()
                .find(|(other, kind, _)| other == id && kind == EXTERNAL_LINK_RELATIONSHIP)
                .map(|(_, _, target)| part_name(target))
                .ok_or_else(|| format!("{path:?}: no external link for {id}"))
        })
        .collect()
}

/// Return the cached sheets of one external link part.
///
/// A sheet's cells stand in a `<sheetData sheetId="N">` of the link's
/// `<sheetDataSet>`, N counting the `<sheetName>`s from zero, and each is a
/// `<cell r="C3" t="str"><v>TEXT</v></cell>`, no t meaning a number.  The
/// link holds the cells it was last told of, the ones read by formulas, and
/// none of the rest: those are blank here.  A sheet whose `refreshError`
/// says the refresh failed, and that cached no cell at all, is a sheet the
/// workbook no longer has, and is left out: Excel reads it as #REF!.
fn cached_sheets(xml: &[u8]) -> CachedSheets {
    let mut reader = Reader::from_reader(xml);
    reader.config_mut().trim_text(true);
    let mut names: Vec<String> = Vec::new();
    let mut cells: Vec<Vec<Cached>> = Vec::new();
    // A sheet whose refresh failed and that cached no cell is one the
    // workbook no longer has: Excel reads it as #REF!.
    let mut failed: Vec<bool> = Vec::new();
    let mut sheet: Option<usize> = None;
    let mut row = 0;
    let mut col = 0;
    let mut kind = 'n';
    let mut text: Option<String> = None;
    let mut buf = Vec::new();
    loop {
        match reader.read_event_into(&mut buf) {
            Ok(Event::Start(ref event)) | Ok(Event::Empty(ref event)) => {
                match event.local_name().as_ref() {
                    b"sheetName" => {
                        names.push(attribute(event, b"val").unwrap_or_default());
                    }
                    b"sheetData" => {
                        let id = attribute(event, b"sheetId")
                            .and_then(|id| id.parse().ok())
                            .unwrap_or(names.len());
                        sheet = Some(id);
                        while cells.len() <= id {
                            cells.push(Vec::new());
                            failed.push(false);
                        }
                        if attribute(event, b"refreshError").as_deref() == Some("1") {
                            failed[id] = true;
                        }
                    }
                    b"row" => {
                        row = attribute(event, b"r")
                            .and_then(|r| r.parse::<usize>().ok())
                            .map(|r| r.saturating_sub(1))
                            .unwrap_or(row + 1);
                        if row == 0 {
                            row = 1;
                        }
                        col = 0;
                    }
                    b"cell" => {
                        if let Some(at) = attribute(event, b"r") {
                            let (column, number, _, _) = index_from_coordinate(&at);
                            if let (Some(column), Some(number)) = (column, number) {
                                col = column as usize - 1;
                                row = number as usize - 1;
                            }
                        }
                        kind = match attribute(event, b"t").as_deref() {
                            Some("str") => 's',
                            Some("b") => 'b',
                            Some("e") => 'e',
                            _ => 'n',
                        };
                        text = Some(String::new());
                    }
                    b"v" => {}
                    _ => {}
                }
            }
            Ok(Event::Text(ref event)) => {
                if let Some(value) = text.as_mut() {
                    if let Ok(decoded) = event.decode() {
                        match quick_xml::escape::unescape(&decoded) {
                            Ok(unescaped) => value.push_str(&unescaped),
                            Err(_) => value.push_str(&decoded),
                        }
                    }
                }
            }
            Ok(Event::End(ref event)) => {
                match event.local_name().as_ref() {
                    b"cell" => {
                        let value = text.take().unwrap_or_default();
                        if let Some(id) = sheet {
                            if (!value.is_empty() || kind != 'n') && id < cells.len() {
                                cells[id].push(Cached {
                                    row,
                                    col,
                                    kind,
                                    value,
                                });
                            }
                        }
                        col += 1;
                    }
                    b"sheetData" => sheet = None,
                    _ => {}
                }
            }
            Ok(Event::Eof) | Err(_) => break,
            _ => {}
        }
        buf.clear();
    }
    while cells.len() < names.len() {
        cells.push(Vec::new());
    }
    while failed.len() < names.len() {
        failed.push(false);
    }
    CachedSheets {
        sheets: names
            .into_iter()
            .zip(cells)
            .zip(failed)
            .filter(|((_, cells), failed)| !(*failed && cells.is_empty()))
            .map(|((name, cells), _)| (name, cells))
            .collect(),
    }
}
