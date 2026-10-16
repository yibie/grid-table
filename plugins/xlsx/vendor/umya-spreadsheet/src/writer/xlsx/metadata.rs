// grid-table: the metadata part that marks dynamic arrays.  Excel
// tells a dynamic array from a legacy {=...} one by cm="1" on its
// cell, pointing at the XLDAPR entry written here.

use std::io;

use quick_xml::{
    Writer,
    events::{
        BytesDecl,
        Event,
    },
};

use super::{
    XlsxError,
    driver::{
        write_end_tag,
        write_new_line,
        write_start_tag,
    },
};
use crate::{
    helper::const_str::{
        DYNAMIC_ARRAY_NS,
        PKG_METADATA,
        SHEET_MAIN_NS,
    },
    structs::{
        Workbook,
        WriterManager,
    },
};

pub(crate) fn write<W: io::Seek + io::Write>(
    wb: &Workbook,
    writer_mng: &mut WriterManager<W>,
) -> Result<(), XlsxError> {
    if !wb.has_dynamic_arrays() {
        return Ok(());
    }

    let mut writer = Writer::new(io::Cursor::new(Vec::new()));
    writer
        .write_event(Event::Decl(BytesDecl::new(
            "1.0",
            Some("UTF-8"),
            Some("yes"),
        )))
        .unwrap();
    write_new_line(&mut writer);

    write_start_tag(
        &mut writer,
        "metadata",
        vec![
            ("xmlns", SHEET_MAIN_NS).into(),
            ("xmlns:xda", DYNAMIC_ARRAY_NS).into(),
        ],
        false,
    );

    write_start_tag(&mut writer, "metadataTypes", vec![("count", "1").into()], false);
    write_start_tag(
        &mut writer,
        "metadataType",
        vec![
            ("name", "XLDAPR").into(),
            ("minSupportedVersion", "120000").into(),
            ("copy", "1").into(),
            ("pasteAll", "1").into(),
            ("pasteValues", "1").into(),
            ("merge", "1").into(),
            ("splitFirst", "1").into(),
            ("rowColShift", "1").into(),
            ("clearFormats", "1").into(),
            ("clearComments", "1").into(),
            ("assign", "1").into(),
            ("coerce", "1").into(),
            ("cellMeta", "1").into(),
        ],
        true,
    );
    write_end_tag(&mut writer, "metadataTypes");

    write_start_tag(
        &mut writer,
        "futureMetadata",
        vec![("name", "XLDAPR").into(), ("count", "1").into()],
        false,
    );
    write_start_tag(&mut writer, "bk", vec![], false);
    write_start_tag(&mut writer, "extLst", vec![], false);
    write_start_tag(
        &mut writer,
        "ext",
        vec![("uri", "{bdbb8cdc-fa1e-496e-a857-3c3f30c029c3}").into()],
        false,
    );
    write_start_tag(
        &mut writer,
        "xda:dynamicArrayProperties",
        vec![("fDynamic", "1").into(), ("fCollapsed", "0").into()],
        true,
    );
    write_end_tag(&mut writer, "ext");
    write_end_tag(&mut writer, "extLst");
    write_end_tag(&mut writer, "bk");
    write_end_tag(&mut writer, "futureMetadata");

    write_start_tag(&mut writer, "cellMetadata", vec![("count", "1").into()], false);
    write_start_tag(&mut writer, "bk", vec![], false);
    write_start_tag(
        &mut writer,
        "rc",
        vec![("t", "1").into(), ("v", "0").into()],
        true,
    );
    write_end_tag(&mut writer, "bk");
    write_end_tag(&mut writer, "cellMetadata");

    write_end_tag(&mut writer, "metadata");
    writer_mng.add_writer(PKG_METADATA, writer)
}
