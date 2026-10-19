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
