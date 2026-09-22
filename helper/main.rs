//! Persistent Typst conversion service. Stdout is JSON-RPC only.
mod latex;
use serde::Deserialize;
use serde_json::{Value, json};
use std::collections::HashMap;
use std::io::{self, BufRead, Write};
use std::path::PathBuf;
use std::sync::Mutex;
use typst::diag::{FileError, FileResult, SourceDiagnostic};
use typst::foundations::{Bytes, Datetime, Duration};
use typst::syntax::{FileId, RootedPath, Source, VirtualPath, VirtualRoot};
use typst::text::{Font, FontBook};
use typst::utils::LazyHash;
use typst::{Feature, Library, LibraryExt, World, WorldExt};
use typst_html::{HtmlDocument, HtmlElement, HtmlNode, tag};
use typst_kit::fonts::{self, FontStore};

#[derive(Deserialize)]
struct Batch {
    root: PathBuf,
    #[serde(default)]
    preamble: String,
    items: Vec<Item>,
    #[serde(default)]
    target: Target,
}

#[derive(Default, Deserialize, PartialEq)]
#[serde(rename_all = "lowercase")]
enum Target {
    #[default]
    Mathml,
    Latex,
    Svg,
}

#[derive(Deserialize)]
struct Item {
    id: String,
    source: String,
    #[serde(default)]
    display: bool,
    #[serde(default)]
    foreground: Option<String>,
}

struct Engine {
    library: LazyHash<Library>,
    fonts: FontStore,
}

impl Engine {
    fn new() -> Self {
        let mut fonts = FontStore::new();
        fonts.extend(fonts::embedded());
        Self {
            library: LazyHash::new(
                Library::builder()
                    .with_features([Feature::Html].into_iter().collect())
                    .build(),
            ),
            fonts,
        }
    }

    fn convert(&self, batch: Batch) -> Value {
        self.convert_inner(batch, false)
    }

    fn render(&self, mut batch: Batch) -> Value {
        batch.target = Target::Svg;
        self.convert_inner(batch, true)
    }

    fn convert_inner(&self, batch: Batch, content: bool) -> Value {
        let mut world = MathWorld {
            engine: self,
            root: batch.root.clone(),
            main: Source::new(
                RootedPath::new(
                    VirtualRoot::Project,
                    VirtualPath::new("/__org_math__.typ").unwrap(),
                )
                .intern(),
                String::new(),
            ),
            files: Mutex::new(HashMap::new()),
        };
        let results: Vec<_> = batch
            .items
            .iter()
            .map(|item| {
                let layout = if batch.target == Target::Svg {
                    format!("#set page(width: auto, height: auto, margin: 2pt, fill: none)\n#set text(size: 12pt, top-edge: \"bounds\", bottom-edge: \"bounds\", fill: rgb({}))\n",
                        serde_json::to_string(item.foreground.as_deref().unwrap_or("#000000")).unwrap())
                } else { String::new() };
                let prefix = if content {
                    format!("{}\n{layout}", batch.preamble)
                } else {
                    format!("{}\n{layout}#math.equation(block: {}, $", batch.preamble, item.display)
                };
                world
                    .main
                    .replace(&format!("{prefix}{}{}", item.source,
                        if content { "" } else { "$.body)" }));
                if batch.target == Target::Svg {
                    let output = typst::compile::<typst_layout::PagedDocument>(&world);
                    let diagnose = |d: &SourceDiagnostic| {
                        world.diagnostic(d, batch.preamble.len(), prefix.len(), item.source.len())
                    };
                    let mut diagnostics: Vec<_> = output.warnings.iter().map(diagnose).collect();
                    return match output.output {
                        Ok(doc) => json!({"id": item.id,
                            "artifact": {"svg": typst_svg::svg_merged(&doc, &Default::default(), typst::layout::Abs::zero())},
                            "diagnostics": diagnostics}),
                        Err(errors) => {
                            diagnostics.extend(errors.iter().map(diagnose));
                            json!({"id": item.id, "diagnostics": diagnostics})
                        }
                    };
                }
                let output = typst::compile::<HtmlDocument>(&world);
                let diagnose = |d: &SourceDiagnostic| {
                    world.diagnostic(d, batch.preamble.len(), prefix.len(), item.source.len())
                };
                let mut diagnostics: Vec<_> = output.warnings.iter().map(diagnose).collect();
                let artifact = output.output.and_then(|mut doc| {
                    if batch.target == Target::Latex {
                        return latex::convert(
                            &batch.preamble,
                            &item.source,
                            &batch.root,
                            item.display,
                        )
                        .map_err(|message| {
                            typst::ecow::eco_vec![SourceDiagnostic::error(
                                world.main.root().span(),
                                message,
                            )]
                        });
                    }
                    let mut maths = Vec::new();
                    collect(doc.root(), tag::mathml::math, &mut maths);
                    let math = maths.last().cloned().cloned();
                    let mut styles = Vec::new();
                    collect(doc.root(), tag::style, &mut styles);
                    let css: String = styles
                        .iter()
                        .flat_map(|s| s.children.iter())
                        .filter_map(|n| {
                            if let HtmlNode::Text(text, _) = n {
                                Some(text.as_str())
                            } else {
                                None
                            }
                        })
                        .collect();
                    if let Some(math) = math {
                        *doc.root_mut() = math.with_attr(
                            typst_html::HtmlAttr::intern("xmlns").unwrap(),
                            "http://www.w3.org/1998/Math/MathML",
                        );
                        typst_html::html(&doc, &Default::default()).map(|html| {
                        json!({"mathml": html.trim_start_matches("<!DOCTYPE html>"), "css": css})
                    })
                    } else {
                        Err(typst::ecow::eco_vec![SourceDiagnostic::error(
                            world.main.root().span(),
                            "formula produced no MathML",
                        )])
                    }
                });
                match artifact {
                    Ok(artifact) => {
                        json!({"id": item.id, "artifact": artifact, "diagnostics": diagnostics})
                    }
                    Err(errors) => {
                        diagnostics.extend(errors.iter().map(diagnose));
                        json!({"id": item.id, "diagnostics": diagnostics})
                    }
                }
            })
            .collect();
        // Retain recent compiler memoization without accumulating every past edit.
        typst::comemo::evict(10);
        json!({"items": results})
    }
}

fn collect<'a>(node: &'a HtmlElement, tag: typst_html::HtmlTag, out: &mut Vec<&'a HtmlElement>) {
    if node.tag == tag {
        out.push(node);
    }
    for child in &node.children {
        if let HtmlNode::Element(element) = child {
            collect(element, tag, out);
        }
    }
}

struct MathWorld<'a> {
    engine: &'a Engine,
    root: PathBuf,
    main: Source,
    // A fresh file snapshot for each batch; imports are reread on the next export.
    files: Mutex<HashMap<FileId, Bytes>>,
}

impl World for MathWorld<'_> {
    fn library(&self) -> &LazyHash<Library> {
        &self.engine.library
    }
    fn book(&self) -> &LazyHash<FontBook> {
        self.engine.fonts.book()
    }
    fn main(&self) -> FileId {
        self.main.id()
    }
    fn font(&self, index: usize) -> Option<Font> {
        self.engine.fonts.font(index)
    }
    fn today(&self, _: Option<Duration>) -> Option<Datetime> {
        None
    }
    fn source(&self, id: FileId) -> FileResult<Source> {
        if id == self.main.id() {
            return Ok(self.main.clone());
        }
        let bytes = self.file(id)?;
        let text = std::str::from_utf8(&bytes).map_err(|_| FileError::InvalidUtf8)?;
        Ok(Source::new(id, text.into()))
    }
    fn file(&self, id: FileId) -> FileResult<Bytes> {
        if id == self.main.id() {
            return Ok(Bytes::from_string(self.main.text().to_owned()));
        }
        let mut files = self.files.lock().unwrap();
        if let Some(bytes) = files.get(&id) {
            return Ok(bytes.clone());
        }
        let root = if let VirtualRoot::Package(spec) = id.root() {
            let relative = format!("{}/{}/{}", spec.namespace, spec.name, spec.version);
            package_paths().into_iter().map(|path| path.join(&relative))
                .find(|path| path.is_dir())
                .ok_or_else(|| FileError::Other(Some(format!(
                    "Package {spec} is not installed; populate the Typst package cache or set TYPST_PACKAGE_PATH"
                ).into())))?
        } else {
            self.root.clone()
        };
        let path = id.vpath().realize(&root).map_err(FileError::Realize)?;
        let bytes = Bytes::new(std::fs::read(&path).map_err(|e| FileError::from_io(e, &path))?);
        files.insert(id, bytes.clone());
        Ok(bytes)
    }
}

// Follow Typst's local package layout without performing network I/O in the
// rendering process. Nix can supply immutable package roots through this path.
fn package_paths() -> Vec<PathBuf> {
    let mut paths: Vec<_> = std::env::var_os("TYPST_PACKAGE_PATH")
        .map(|value| std::env::split_paths(&value).collect())
        .unwrap_or_default();
    let home = std::env::var_os("HOME").map(PathBuf::from);
    for (variable, fallback) in [
        ("XDG_DATA_HOME", ".local/share"),
        ("XDG_CACHE_HOME", ".cache"),
    ] {
        if let Some(root) = std::env::var_os(variable)
            .map(PathBuf::from)
            .or_else(|| home.as_ref().map(|path| path.join(fallback)))
        {
            paths.push(root.join("typst/packages"));
        }
    }
    paths
}

impl MathWorld<'_> {
    fn diagnostic(
        &self,
        d: &SourceDiagnostic,
        preamble_len: usize,
        prefix_len: usize,
        source_len: usize,
    ) -> Value {
        let mut result = json!({
            "severity": format!("{:?}", d.severity).to_lowercase(),
            "message": d.message.as_str(),
            "hints": d.hints.iter().map(|h| h.v.as_str()).collect::<Vec<_>>(),
            "trace": d.trace.iter().map(|point| {
                self.diagnostic(
                    &SourceDiagnostic::error(point.span, point.v.to_string()),
                    preamble_len, prefix_len, source_len,
                )
            }).collect::<Vec<_>>(),
        });
        if let Some(id) = d.span.id()
            && let Ok(source) = self.source(id)
            && let Some(range) = self.range(d.span)
        {
            let (origin, start, end) = if id != self.main.id() {
                ("file", range.start, range.end)
            } else if range.start < preamble_len {
                ("preamble", range.start, range.end.min(preamble_len))
            } else {
                (
                    "fragment",
                    range.start.saturating_sub(prefix_len).min(source_len),
                    range.end.saturating_sub(prefix_len).min(source_len),
                )
            };
            result["origin"] = json!(origin);
            result["start"] = json!(start);
            result["end"] = json!(end);
            if origin == "file" {
                result["file"] = json!(id.vpath().realize(&self.root).ok());
                result["line"] = json!(
                    source.text()[..range.start]
                        .bytes()
                        .filter(|b| *b == b'\n')
                        .count()
                        + 1
                );
                result["column"] = json!(
                    source.text()[..range.start]
                        .rsplit('\n')
                        .next()
                        .unwrap_or("")
                        .chars()
                        .count()
                        + 1
                );
            }
        }
        result
    }
}

fn read_message(input: &mut impl BufRead) -> io::Result<Option<Value>> {
    let mut length = None;
    loop {
        let mut line = String::new();
        if input.read_line(&mut line)? == 0 {
            return Ok(None);
        }
        if line.trim().is_empty() {
            break;
        }
        if let Some((name, value)) = line.split_once(':')
            && name.eq_ignore_ascii_case("Content-Length")
        {
            length = Some(value.trim().parse::<usize>().map_err(io::Error::other)?);
        }
    }
    let mut bytes = vec![0; length.ok_or_else(|| io::Error::other("missing Content-Length"))?];
    input.read_exact(&mut bytes)?;
    serde_json::from_slice(&bytes)
        .map(Some)
        .map_err(io::Error::other)
}

fn main() -> io::Result<()> {
    let engine = Engine::new();
    let mut input = io::stdin().lock();
    let mut output = io::stdout().lock();
    while let Some(request) = read_message(&mut input)? {
        let method = request["method"].as_str().unwrap_or("");
        if method == "exit" {
            break;
        }
        let Some(id) = request.get("id") else {
            continue;
        };
        let result = match method {
            "initialize" => Ok(
                json!({"protocolVersion": 1, "targets": ["mathml", "latex", "svg"],
                    "methods": ["math/convert", "typst/render"],
                    "version": env!("CARGO_PKG_VERSION")}),
            ),
            "math/convert" => serde_json::from_value(request["params"].clone())
                .map(|batch| engine.convert(batch))
                .map_err(|e| json!({"code": -32602, "message": e.to_string()})),
            "typst/render" => serde_json::from_value(request["params"].clone())
                .map(|batch| engine.render(batch))
                .map_err(|e| json!({"code": -32602, "message": e.to_string()})),
            "shutdown" => Ok(Value::Null),
            _ => Err(json!({"code": -32601, "message": format!("Unknown method: {method}")})),
        };
        let response = match result {
            Ok(value) => json!({"jsonrpc": "2.0", "id": id, "result": value}),
            Err(error) => json!({"jsonrpc": "2.0", "id": id, "error": error}),
        };
        let body = serde_json::to_vec(&response)?;
        write!(output, "Content-Length: {}\r\n\r\n", body.len())?;
        output.write_all(&body)?;
        output.flush()?;
    }
    Ok(())
}
