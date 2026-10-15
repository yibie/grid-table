use crate::helper::utils::compile_regex;

#[must_use]
pub fn split_address(address: &str) -> (std::borrow::Cow<'_, str>, &str) {
    address
        .rsplit_once('!')
        .map_or((std::borrow::Cow::Borrowed(""), address), |(sheet_name, range)| {
            // grid-table: a quoted name writes its apostrophes twice.
            let quoted = sheet_name.len() >= 2
                && sheet_name.starts_with('\'')
                && sheet_name.ends_with('\'');
            let sheet_name = sheet_name.trim_matches(&['\'', '"'][..]);
            if quoted && sheet_name.contains("''") {
                (std::borrow::Cow::Owned(sheet_name.replace("''", "'")), range)
            } else {
                (std::borrow::Cow::Borrowed(sheet_name), range)
            }
        })
}

#[must_use]
pub fn join_address(sheet_name: &str, address: &str) -> String {
    if sheet_name.is_empty() {
        return address.to_string();
    }
    if sheet_name_needs_quoting(sheet_name) {
        let escaped = sheet_name.replace('\'', "''");
        return format!("'{escaped}'!{address}");
    }
    format!("{sheet_name}!{address}")
}

/// A sheet name can appear unquoted in a formula only when it is a bare
/// identifier: it starts with a letter or underscore and otherwise contains
/// only letters, digits, underscores or periods. Anything else (spaces,
/// punctuation such as `(`/`)`, a leading digit, …) must be wrapped in single
/// quotes when re-serialized.
pub(crate) fn sheet_name_needs_quoting(sheet_name: &str) -> bool {
    let mut chars = sheet_name.chars();
    match chars.next() {
        None => false,
        Some(first) => {
            if !(first.is_ascii_alphabetic() || first == '_') {
                return true;
            }
            // grid-table: a name that reads as a cell, such as A1, or as an
            // R1C1 reference, such as R or C2, is quoted too.
            let upper = sheet_name.to_ascii_uppercase();
            let letters = upper.trim_end_matches(|c: char| c.is_ascii_digit());
            let reads_as_cell = letters.len() < upper.len()
                && letters.len() <= 3
                && letters.chars().all(|c| c.is_ascii_alphabetic());
            let reads_as_r1c1 = {
                let rest = upper.trim_start_matches('R');
                let rest = rest.trim_start_matches(|c: char| c.is_ascii_digit());
                let rest = rest.strip_prefix('C').unwrap_or(rest);
                rest.chars().all(|c| c.is_ascii_digit())
                    && (upper.starts_with('R') || upper.starts_with('C'))
            };
            reads_as_cell
                || reads_as_r1c1
                || sheet_name
                    .chars()
                    .any(|c| !(c.is_ascii_alphanumeric() || c == '_' || c == '.'))
        }
    }
}

/// Checks if the given input string is a valid address format.
///
/// The address format is defined by the following regular expression:
/// `^([^\:\\\?$$$$\/\*]+\!)?(\$?[A-Z]{1,3}\$?[0-9]+)(\:\$?[A-Z]{1,3}\$?[0-9]+)?
/// $`.
///
/// # Parameters
///
/// - `input`: A string slice that can be converted to a string reference. This
///   is the input string to be checked against the address format.
///
/// # Returns
///
/// Returns `true` if the input string matches the address format, and `false`
/// otherwise.
///
/// # Panics
///
/// This function may panic if the regular expression fails to compile. However,
/// since the regular expression is initialized only once and is hardcoded, this
/// is unlikely to occur unless there is a bug in the regex itself. The panic
/// will occur during the first call to this function if the regex is invalid.
///
/// # Examples
///
/// ```rust
/// # use umya_spreadsheet::helper::address::is_address;
/// let valid_address = "$A1";
/// assert!(is_address(valid_address));
///
/// let invalid_address = "invalid_address";
/// assert!(!is_address(invalid_address));
/// ```
pub fn is_address<S: AsRef<str>>(input: S) -> bool {
    let regex = compile_regex!(
        r"^([^\:\\\?\[\]\/\*]+\!)?(\$?[A-Z]{1,3}\$?[0-9]+)(\:\$?[A-Z]{1,3}\$?[0-9]+)?$"
    );
    regex.is_match(input.as_ref()).unwrap()
}

#[test]
fn split_address_test() {
    assert_eq!(split_address("A1"), ("".into(), "A1"));
    assert_eq!(split_address("A1:B2"), ("".into(), "A1:B2"));
    assert_eq!(split_address("sheet1!A1:B2"), ("sheet1".into(), "A1:B2"));
    assert_eq!(split_address("'she!et1'!A1:B2"), ("she!et1".into(), "A1:B2"));
    assert_eq!(split_address(r#"'she"et1'!A1:B2"#), (r#"she"et1"#.into(), "A1:B2"));
    assert_eq!(split_address("'it''s'!A1"), ("it's".into(), "A1"));
    assert_eq!(join_address("it's", "A1"), "'it''s'!A1");
    assert_eq!(join_address("Sheet&8", "A1"), "'Sheet&8'!A1");
    assert_eq!(join_address("A1", "B2"), "'A1'!B2");
    assert_eq!(join_address("R1C1", "B2"), "'R1C1'!B2");
    assert_eq!(join_address("Sheet1", "B2"), "Sheet1!B2");
}

#[test]
fn is_address_test() {
    assert!(is_address("A1"));
    assert!(is_address("$A1"));
    assert!(is_address("A$1"));
    assert!(is_address("$A$1"));

    assert!(is_address("A1:B2"));
    assert!(is_address("$A1:B2"));
    assert!(is_address("$A$1:B2"));
    assert!(is_address("$A$1:$B2"));
    assert!(is_address("$A$1:$B$2"));

    assert!(is_address("Sheet1!A1"));
    assert!(is_address("Sheet1!$A1"));
    assert!(is_address("Sheet1!A$1"));
    assert!(is_address("Sheet1!A$1"));

    assert!(is_address("Sheet1!A1:B2"));
    assert!(is_address("Sheet1!$A1:B2"));
    assert!(is_address("Sheet1!$A$1:B2"));
    assert!(is_address("Sheet1!$A$1:$B2"));
    assert!(is_address("Sheet1!$A$1:$B$2"));
    assert!(is_address("New Sheet!$H$7:$H$10"));

    assert!(!is_address("(Sheet1!A1:B2)"));
    assert!(!is_address("Sheet1!A1:"));
    assert!(!is_address("Sheet1!A1:B"));
    assert!(!is_address("Sheet1!A:B2"));
    assert!(!is_address("Sheet1"));
}
