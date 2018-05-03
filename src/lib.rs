/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

//! This is a basic recursive-descent parser for ipdl. It intentionally doesn't
//! have some featres which most languages would have, such as unicode support
//! in identifiers, as it is unnecessary for our usecase.

use std::fmt;
use std::error;

#[macro_use]
mod util;
use util::*;

struct ErrorInner {
    line: usize,
    column: usize,
    message: String,
}

/// Public error type for messages with resolved type information.
pub struct Error(Box<ErrorInner>);

impl Error {
    /// Get the line and column of the parsing error.
    pub fn line_column(&self) -> (usize, usize) {
        (self.0.line, self.0.column)
    }
}

impl error::Error for Error {
    fn description(&self) -> &str { &self.0.message }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.message.fmt(f)
    }
}

impl fmt::Debug for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Error")
            .field("line", &self.0.line)
            .field("column", &self.0.column)
            .finish()
    }
}

#[derive(Debug)]
pub struct CxxInclude {
    pub file: String,
}

fn cxx_include(i: In) -> PResult<CxxInclude> {
    let (i, _) = kw(i, "include")?;
    let (i, file) = string(i)?;
    commit! {
        let (i, _) = punct(i, ";")?;
        Ok((i, CxxInclude { file }))
    }
}

#[derive(Debug)]
pub struct Include {
    pub protocol: bool,
    pub id: String,
}

fn include(i: In) -> PResult<Include> {
    let (i, _) = kw(i, "include")?;
    let (i, pcol) = maybe(i, kw(i, "protocol"))?;

    // We don't commit before the identifier because at this point we could be
    // looking at a C++ include.
    let (i, id) = ident(i)?;
    commit! {
        let (i, _) = punct(i, ";")?;
        Ok((i, Include {
            protocol: pcol.is_some(),
            id: id.to_owned(),
        }))
    }
}

#[derive(Debug)]
pub enum CxxTypeKind {
    Class,
    Struct,
    None,
}

#[derive(Debug)]
pub struct CxxPathSeg {
    pub id: String,
    pub args: Vec<String>,
}

fn template_args(i: In) -> PResult<Vec<String>> {
    let (i, _) = punct(i, "<")?;
    commit! {
        let (i, args) = sep(i, ident, ",")?;
        let (i, _) = punct(i, ">")?;
        Ok((i, args))
    }
}

fn cxx_path_seg(i: In) -> PResult<CxxPathSeg> {
    let (i, id) = ident(i)?;
    let (i, args) = maybe(i, template_args(i))?;
    Ok((i, CxxPathSeg {
        id: id,
        args: args.unwrap_or(Vec::new()),
    }))
}

#[derive(Debug)]
pub struct CxxPath {
    pub segs: Vec<CxxPathSeg>,
}

fn cxx_path(i: In) -> PResult<CxxPath> {
    let (i, segs) = sep(i, cxx_path_seg, "::")?;
    Ok((i, CxxPath { segs }))
}

#[derive(Debug)]
pub struct Using {
    pub refcounted: bool,
    pub kind: CxxTypeKind,
    pub ty: CxxPath,
    pub file: String,
}

fn using(i: In) -> PResult<Using> {
    let (i, _) = kw(i, "using")?;
    commit! {
        let (i, refcounted) = maybe(i, kw(i, "refcounted"))?;
        let refcounted = refcounted.is_some();
        let (i, kind) = any!(
            i, "struct or class keyword",
            kw(i, "struct") => CxxTypeKind::Struct,
            kw(i, "class") => CxxTypeKind::Class,
            Ok((i, ())) => CxxTypeKind::None,
        )?;

        let (i, ty) = cxx_path(i)?;
        let (i, _) = kw(i, "from")?;
        let (i, file) = string(i)?;
        let (i, _) = punct(i, ";")?;

        Ok((i, Using { refcounted, kind, ty, file }))
    }
}

#[derive(Debug)]
pub struct Type {
    pub is_nullable: bool,
    pub name: CxxPathSeg,
    pub is_array: bool,
}

fn ty(i: In) -> PResult<Type> {
    let (i, nullable) = maybe(i, kw(i, "nullable"))?;
    let (i, name) = cxx_path_seg(i)?;
    let (i, array) = maybe(i, (|| {
        let (i, _) = punct(i, "[")?;
        commit! { punct(i, "]") }
    })())?;

    Ok((i, Type {
        is_nullable: nullable.is_some(),
        name,
        is_array: array.is_some(),
    }))
}

fn component(i: In) -> PResult<Type> {
    let (i, ty) = ty(i)?;
    commit! {
        let (i, _) = punct(i, ";")?;
        Ok((i, ty))
    }
}

#[derive(Debug)]
pub struct UnionItem {
    pub path: Vec<String>,
    pub components: Vec<Type>,
}

fn union_item(i: In) -> PResult<UnionItem> {
    let (i, _) = kw(i, "union")?;
    commit! {
        let (i, name) = ident(i)?;
        let (i, _) = punct(i, "{")?;
        let (i, components) = many(i, component)?;
        let (i, _) = punct(i, "}")?;
        let (i, _) = punct(i, ";")?;

        Ok((i, UnionItem {
            path: vec![name],
            components,
        }))
    }
}

#[derive(Debug)]
pub struct Field {
    pub ty: Type,
    pub name: String,
}

fn field(i: In) -> PResult<Field> {
    let (i, ty) = ty(i)?;
    commit! {
        let (i, name) = ident(i)?;
        let (i, _) = punct(i, ";")?;
        Ok((i, Field { ty, name }))
    }
}

#[derive(Debug)]
pub struct StructItem {
    pub path: Vec<String>,
    pub fields: Vec<Field>,
}

fn struct_item(i: In) -> PResult<StructItem> {
    let (i, _) = kw(i, "struct")?;
    commit! {
        let (i, name) = ident(i)?;
        let (i, _) = punct(i, "{")?;
        let (i, fields) = many(i, field)?;
        let (i, _) = punct(i, "}")?;
        let (i, _) = punct(i, ";")?;

        Ok((i, StructItem {
            path: vec![name],
            fields,
        }))
    }
}

#[derive(Debug)]
pub enum Nesting {
    None,
    InsideSync,
    InsideCpow,
}

fn nesting(i: In) -> PResult<Nesting> {
    any!(
        i, "nesting specifier (not, inside_sync, or inside_cpow)",
        kw(i, "not") => Nesting::None,
        kw(i, "inside_sync") => Nesting::InsideSync,
        kw(i, "inside_cpow") => Nesting::InsideCpow,
    )
}

#[derive(Debug)]
pub enum Priority {
    Normal,
    High,
    Input,
}

fn priority(i: In) -> PResult<Priority> {
    any!(
        i, "priority specifier (normal, high, or input)",
        kw(i, "normal") => Priority::Normal,
        kw(i, "high") => Priority::High,
        kw(i, "input") => Priority::Input,
    )
}

#[derive(Debug)]
pub enum SendSemantics {
    Async,
    Sync,
    Intr,
}

#[derive(Debug)]
pub enum MessageModifier {
    Verify,
    Compress,
    CompressAll,
}

fn message_modifier(i: In) -> PResult<MessageModifier> {
    any!(
        i, "message modifier (verify, compress, or compressall)",
        kw(i, "verify") => MessageModifier::Verify,
        kw(i, "compress") => MessageModifier::Compress,
        kw(i, "compressall") => MessageModifier::CompressAll,
    )
}

#[derive(Debug)]
pub struct Param {
    pub ty: Type,
    pub name: String,
}

fn param(i: In) -> PResult<Param> {
    let (i, ty) = ty(i)?;
    commit! {
        let (i, name) = ident(i)?;
        Ok((i, Param { ty, name }))
    }
}

#[derive(Debug)]
pub struct MessageDecl {
    pub nested: Nesting,
    pub priority: Priority,
    pub send_semantics: SendSemantics,
    pub name: String,
    pub params: Vec<Param>,
    pub returns: Vec<Param>,
    pub modifiers: Vec<MessageModifier>,
}

fn returns(i: In) -> PResult<Vec<Param>> {
    let (i, _) = kw(i, "returns")?;
    commit! {
        let (i, _) = punct(i, "(")?;
        let (i, p) = sep(i, param, ",")?;
        let (i, _) = punct(i, ")")?;
        Ok((i, p))
    }
}

fn message_nested(i: In) -> PResult<Nesting> {
    let (i, _) = kw(i, "nested")?;
    commit! {
        let (i, _) = punct(i, "(")?;
        let (i, nested) = nesting(i)?;
        let (i, _) = punct(i, ")")?;
        Ok((i, nested))
    }
}

fn message_prio(i: In) -> PResult<Priority> {
    let (i, _) = kw(i, "prio")?;
    commit! {
        let (i, _) = punct(i, "(")?;
        let (i, prio) = priority(i)?;
        let (i, _) = punct(i, ")")?;
        Ok((i, prio))
    }
}

fn message_decl(i: In) -> PResult<MessageDecl> {
    // XXX(nika): This is really gross, maybe clean it up?
    let mut nested = Nesting::None;
    let mut priority = Priority::Normal;
    drive!(i, any!(
        i, "message prefix",
        message_prio(i) => |p| priority = p,
        message_nested(i) => |n| nested = n,
    ));

    let (i, send_semantics) = any!(
        i, "send semantics (async, sync, or intr)",
        kw(i, "async") => SendSemantics::Async,
        kw(i, "sync") => SendSemantics::Sync,
        kw(i, "intr") => SendSemantics::Intr,
    )?;

    commit! {
        let (i, name) = ident(i)?;
        let (i, _) = punct(i, "(")?;
        let (i, params) = sep(i, param, ",")?;
        let (i, _) = punct(i, ")")?;
        let (i, returns) = maybe(i, returns(i))?;
        let (i, modifiers) = many(i, message_modifier)?;
        let (i, _) = punct(i, ";")?;

        Ok((i, MessageDecl {
            nested,
            priority,
            send_semantics,
            name,
            params,
            returns: returns.unwrap_or(Vec::new()),
            modifiers,
        }))
    }
}

#[derive(Debug)]
pub enum Direction {
    ToChild,
    ToParent,
    Both,
}

fn direction(i: In) -> PResult<Direction> {
    any!(
        i, "direction (child, parent, or both)",
        kw(i, "child") => Direction::ToChild,
        kw(i, "parent") => Direction::ToParent,
        kw(i, "both") => Direction::Both,
    )
}

#[derive(Debug)]
pub struct MessageGroup {
    pub direction: Direction,
    pub decls: Vec<MessageDecl>,
}

fn message_group(i: In) -> PResult<MessageGroup> {
    let (i, direction) = direction(i)?;
    commit! {
        let (i, _) = punct(i, ":")?;
        let (i, decls) = many(i, message_decl)?;

        Ok((i, MessageGroup {
            direction,
            decls,
        }))
    }
}

#[derive(Debug)]
pub struct ProtocolItem {
    pub path: Vec<String>,
    pub nested: Nesting,
    pub send_semantics: SendSemantics,
    pub managers: Vec<String>,
    pub manages: Vec<String>,
    pub groups: Vec<MessageGroup>,
}

fn protocol_nested(i: In) -> PResult<(Nesting, SendSemantics)> {
    let (i, _) = kw(i, "nested")?;
    commit! {
        let (i, _) = punct(i, "(")?;
        let (i, _) = kw(i, "upto")?;
        let (i, n) = nesting(i)?;
        let (i, _) = punct(i, ")")?;
        let (i, ss) = any!(
            i, "send semantics (async or sync)",
            kw(i, "async") => SendSemantics::Async,
            kw(i, "sync") => SendSemantics::Sync,
        )?;
        Ok((i, (n, ss)))
    }
}

fn managers(i: In) -> PResult<Vec<String>> {
    if let Ok((i, _)) = punct(i, "manager") {
        commit! {
            let (i, managers) = sep(i, ident, "or")?;
            let (i, _) = punct(i, ";")?;
            Ok((i, managers))
        }
    } else {
        Ok((i, Vec::new()))
    }
}

fn manages(i: In) -> PResult<String> {
    let (i, _) = kw(i, "manages")?;
    commit! {
        let (i, name) = ident(i)?;
        let (i, _) = punct(i, ";")?;
        Ok((i, name))
    }
}

fn protocol_item(i: In) -> PResult<ProtocolItem> {
    let (i, (nested, send_semantics)) = any!(
        i, "protocol item prefixes",
        kw(i, "async") => |_x| (Nesting::None, SendSemantics::Async),
        kw(i, "sync") => |_x| (Nesting::None, SendSemantics::Sync),
        kw(i, "intr") => |_x| (Nesting::None, SendSemantics::Intr),
        protocol_nested(i) => |x| x,
        Ok((i, ())) => |_x| (Nesting::None, SendSemantics::Async),
    )?;

    let (i, _) = kw(i, "protocol")?;
    commit! {
        let (i, name) = ident(i)?;
        let (i, _) = punct(i, "{")?;
        let (i, managers) = managers(i)?;
        let (i, manages) = many(i, manages)?;
        let (i, groups) = many(i, message_group)?;
        let (i, _) = punct(i, "}")?;
        let (i, _) = punct(i, ";")?;

        Ok((i, ProtocolItem {
            send_semantics,
            nested,
            path: vec![name],
            managers,
            manages,
            groups,
        }))
    }
}

#[derive(Debug)]
pub enum Item {
    Struct(StructItem),
    Union(UnionItem),
    Protocol(ProtocolItem),
}

fn namespace(i: In) -> PResult<Vec<Item>> {
    let (i, _) = kw(i, "namespace")?;

    commit! {
        let (i, name) = ident(i)?;
        let (i, _) = punct(i, "{")?;
        let (i, mut items) = items(i)?;
        let (i, _) = punct(i, "}")?;
        for it in &mut items {
            match *it  {
                Item::Struct(ref mut i) =>
                    i.path.insert(0, name.clone()),
                Item::Union(ref mut i) =>
                    i.path.insert(0, name.clone()),
                Item::Protocol(ref mut i) =>
                    i.path.insert(0, name.clone()),
            }
        }
        Ok((i, items))
    }
}

fn items(i: In) -> PResult<Vec<Item>> {
    let mut v = Vec::new();
    drive!(i, any!(
        i, "item (struct, union, protocol, or namespace)",
        struct_item(i) => |x| v.push(Item::Struct(x)),
        union_item(i) => |x| v.push(Item::Union(x)),
        protocol_item(i) => |x| v.push(Item::Protocol(x)),
        namespace(i) => |x| v.extend(x),
    ));
    Ok((i, v))
}

#[derive(Debug)]
pub struct TranslationUnit {
    pub cxx_includes: Vec<CxxInclude>,
    pub includes: Vec<Include>,
    pub usings: Vec<Using>,
    pub items: Vec<Item>,
}

fn translation_unit(i: In) -> PResult<TranslationUnit> {
    // Prelude.
    let mut usings = Vec::new();
    let mut includes = Vec::new();
    let mut cxx_includes = Vec::new();
    drive!(i, any!(
        i, "include or using declaration",
        using(i) => |u| usings.push(u),
        include(i) => |u| includes.push(u),
        cxx_include(i) => |u| cxx_includes.push(u),
    ));

    // Body.
    let (i, items) = items(i)?;

    // Make sure we're at EOF
    let i = skip_ws(i)?;
    if !i.rest().is_empty() {
        return i.expected("item (struct, union, protocol, or namespace)");
    }

    Ok((i, TranslationUnit {
        cxx_includes,
        includes,
        usings,
        items,
    }))
}

/// Entry point - parses a whole translation unit.
pub fn parse(src: &str) -> Result<TranslationUnit, Error> {
    match translation_unit(In::new(src)) {
        Ok((_, v)) => Ok(v),
        Err(err) => {
            assert!(err.offset() <= src.len());

            // Get the line/column and line where the error occurred.
            let mut text = src;
            let mut line = 1;
            let mut column = err.offset();
            while let Some(idx) = text.find('\n') {
                if column < idx {
                    // Trim after the newline character
                    text = &text[..idx];
                    break;
                }
                line += 1;
                column -= idx + 1;
                text = &text[idx + 1..];
            }

            // Format the final error message.
            let message = format!("Parse Error @ {}:{}: Expected {}\n\
                                   | {}\n{:~>off$}^\n",
                                  line, column, err.expected,
                                  text, "", off=column + 2);
            Err(Error(Box::new(ErrorInner { line, column, message })))
        }
    }
}
