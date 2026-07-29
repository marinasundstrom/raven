# **RavenDoc** — Documentation Generator

RavenDoc is Raven’s built-in documentation generator. It produces a static
HTML documentation site from Markdown attached to source symbols or loaded
from a compiled library's adjacent `.docs` sidecar.

The core idea is simple:
**documentation lives with the code, in Markdown, and is rendered as-is**.

---

## For whom is RavenDoc intended?

RavenDoc is intended for developers who:

* want documentation colocated with source code
* prefer Markdown over external documentation systems
* don’t need a separate authoring pipeline
* want documentation generated as part of compilation or tooling

If your documentation needs are satisfied by writing Markdown directly in the source code, RavenDoc is a good fit.

---

## Documentation comments in Raven

Raven supports two kinds of documentation content:

### XML documentation

* Structural and machine-readable
* Intended for external tools (e.g. DocFX-style pipelines)
* Focuses on data extraction rather than presentation

### Markdown documentation

* Presentation-focused
* What you write is what gets rendered
* Supports headings, tables, lists, code blocks, etc.

**RavenDoc prefers Markdown documentation.** XML documentation remains a
compatibility input for .NET libraries that do not publish Raven Markdown.

---

## Documentation comment syntax

Documentation comments are attached to declaration syntax nodes as **leading trivia**.

```raven
/// ## Hello
///
/// **Test**
public func Foo() { }
```

Notes:

* Each line is prefixed with `///`
* The prefix is stripped before Markdown processing
* Blank documentation lines must still use `///`

### Markdown doc tags

Raven Markdown documentation also supports a lightweight block-tag layer for
structured member data.

Example:

```raven
/// Parses a widget title.
///
/// @param text Input text to parse.
/// @returns The parsed title.
/// @remarks This is culture-invariant.
func ParseTitle(text: string) -> string
```

Supported tags currently include:

* `@param name`
* `@typeparam name`
* `@returns`
* `@value`
* `@remarks`
* `@example`
* `@exception TypeName`
* `@see SymbolOrLink`
* `@seealso SymbolOrLink`
* `@inheritdoc`

These tags are parsed structurally by the compiler while still preserving the
original Markdown content as authored.

---

## Accessing documentation from symbols

Documentation comments can be retrieved from symbols, both for:

* source-defined symbols
* metadata symbols (when available)

```raven
let comment = symbol.GetDocumentationComment()

let content = comment?.Content // Markdown, without "///"
let rawText = comment?.RawText // Original text, with "///"
```

RavenDoc uses the processed Markdown content (`Content`) for rendering.

## Generating a site

RavenDoc accepts a Raven project, one Raven source file, a directory containing
Raven source files, or a compiled library:

```bash
dotnet run --project src/RavenDoc -- \
  samples/projects/markdown-docs/library/MarkdownDocs.Library.rvnproj \
  --output artifacts/markdown-docs-site
```

To publish from a library, keep its `.docs` sidecar adjacent to the assembly:

```bash
dotnet run --project src/RavenDoc -- \
  artifacts/library/MarkdownDocs.Library.dll \
  --output artifacts/markdown-docs-library-site
```

Use `--framework <tfm>` when the input targets something other than `net10.0`.
When `--output` is omitted, RavenDoc writes `_site` next to the input.
Source-directory input can add assembly dependencies with repeatable
`--reference <assembly>` options. Repeatable `--nav <label=url>` options add
links to related documentation sites in the generated header.

### Injecting build values

RavenDoc can replace explicit placeholders in Markdown with values supplied by
the build or publishing workflow. Pass `--value name=value` once for each
value:

```bash
dotnet run --project src/RavenDoc -- \
  src/Raven.Core/Raven.Core.rvnproj \
  --output artifacts/raven-core-api \
  --value version=1.4.0 \
  --value apiRoot=../api/
```

Use the values in documentation Markdown with `{{name}}`:

```raven
/// Available since Raven {{version}}.
///
/// See the [complete API reference]({{apiRoot}}).
public func Parse(text: string) -> SyntaxTree
```

Values are substituted as plain Markdown before HTML rendering. This supports
paths, package or compiler versions, commit identifiers, and version stamps
without making RavenDoc responsible for discovering that build metadata.
Whitespace inside a placeholder is optional. Value names may contain letters,
digits, `_`, `-`, and `.`, and must begin with a letter or `_`. Repeating a
name uses its last supplied value. Placeholders without a supplied value remain
visible in the generated documentation.

Namespace functions are organized under their Raven namespace. Their pages
also identify the emitted CLR container so consumers using C#, reflection, or
another .NET language can locate the metadata member. Namespace-level
`macro func` declarations are listed separately as macros and do not require a
fabricated containing type in the Raven-facing reference.

## Relationship to metadata sidecars

Raven’s compiler and IDE load Markdown documentation for metadata references
from assembly-adjacent `.docs/` sidecars. The built-in compiler comment emitter
produces that structure for Markdown output. RavenDoc consumes the same content
either directly from Raven source symbols or indirectly from a compiled
library and its Markdown sidecar.

Important separation:

* Markdown sidecars are authored presentation content.
* XML sidecars are structured interoperability data.
* RavenDoc consumes Markdown plus symbol data; it does not redefine the storage
  format for either Markdown or XML documentation.

RavenDoc's two primary input paths are:

* source symbols with attached Markdown comments
* PE symbols with Markdown sidecars

Both paths normalize through the Raven documentation model before RavenDoc
assembles symbol pages and projects them to HTML. XML remains a compatibility
input for libraries that do not provide Raven Markdown, rather than the model
that shapes RavenDoc's APIs.

## Rendering direction

The current HTML renderer provides a Raven-specific, responsive API-reference
presentation with light and dark color schemes. Page titles use a compact
reference-heading scale, while editor-like Raven signatures carry the primary
visual weight. Namespace and member kinds use distinct symbols, generic
constraints remain visible in signatures, and generated pages include a
responsive page outline. Fenced `raven`, `rvn`, and `rav` code blocks receive
Raven syntax highlighting from a generated local asset, so published sites do
not require a CDN.

The current rendering boundary is intentionally explicit:

* `DocumentationGenerator` extracts documentation, builds symbol navigation,
  and selects page content.
* `RavenDocSiteTemplate` owns the HTML shell and reusable hero, signature, and
  member-list components.
* `Assets/ravendoc.css` and `Assets/ravendoc.js` own presentation and
  progressive enhancement.

This separation keeps extraction, symbol routing, Markdown rendering, and page
chrome independent. A future templating engine can replace the template
projection without teaching templates how to parse source, sidecars, XML
documentation, or compiler symbols.

RavenDoc and the browser Playground consume the same foundational Raven theme:
color tokens, typography, surfaces, borders, radii, shadows, and the Raven
brand mark. Each tool composes those primitives for its own purpose rather than
sharing one rigid page layout. This keeps reference reading and interactive
coding distinct while making movement between them feel continuous. RavenDoc
follows the system color scheme; the Playground additionally offers a
persistent System, Light, or Dark selector that also controls its editor.

A future rendering layer can introduce user-selectable templates and theme
customization at the same page-chrome boundary. Templates should receive the
Raven documentation model and resolved symbol navigation.

Interactive, executable examples belong to the future documentation-site
layer. That site can progressively enhance explicitly opted-in examples using
the same compiler-in-the-browser mechanism as the Raven playground. RavenDoc's
static output and ordinary fenced examples must continue to work without that
runtime or a network connection.

For the editor-facing documentation view that should share the same underlying
model without depending on published HTML, see
[Editor Documentation Experience](./compiler/design/editor-documentation-experience.md).

---

## Link conventions (important)

RavenDoc supports **symbol-aware links** using an `xref:` scheme, inspired by XML documentation IDs.

This allows documentation to link to:

* namespaces
* types
* members (methods, properties, fields)

### Basic form

```md
[Result](xref:T:System.Result`2)
```

At render time, RavenDoc resolves the `xref:` target and replaces it with a relative link to the generated page.

---

### Supported `xref:` prefixes

| Prefix | Meaning   | Example                               |
| ------ | --------- | ------------------------------------- |
| `N:`   | Namespace | `xref:N:System.Collections`           |
| `T:`   | Type      | `xref:T:System.Result\`2`             |
| `M:`   | Method    | `xref:M:System.Result\`2.UnwrapError` |
| `P:`   | Property  | `xref:P:System.Result\`2.Value`       |
| `F:`   | Field     | `xref:F:System.Result\`2.Error`       |

Notes:

* Generic arity is written using backticks (`` ` ``), e.g. ``Result`2``
* Overloads automatically resolve to the **member group page**
* If a link cannot be resolved, it is rendered without a target and marked as unresolved

---

### Why `xref:` exists

Normal Markdown links require knowing file paths.
`xref:` allows documentation authors to write **symbol-based links** without caring about layout, folders, or filenames.

This keeps documentation stable even if the generated structure changes.

---

## Recommended documentation structure (not enforced)

RavenDoc does **not** impose a schema. Sections are free-form Markdown.

That said, the following sections are recommended for consistency and readability:

### For types

```md
## Summary
Brief description of the type.

## Usage
Example usage.

## Remarks
Important details, constraints, or design notes.

## Examples
Longer or multiple examples.
```

### For members

```md
## Summary
What this member does.

## Parameters
Description of parameters (if applicable).

## Returns
What is returned (if applicable).

## Remarks
Edge cases, behavior, or guarantees.
```

You are free to ignore or reorder these sections.

---

## Current state and limitations

RavenDoc is currently **early-stage**.

Current limitations:

* Not a reusable library (requires recompilation)
* Fixed HTML layout
* No external pages or navigation injection
* No custom theming beyond CSS edits
* No schema validation for documentation content

Despite this, RavenDoc is already suitable for:

* internal libraries
* language/runtime documentation
* API reference generation
* early-stage public projects

---

## Summary

RavenDoc is intentionally simple:

* Markdown in source
* Symbol-aware links via `xref:`
* One page per namespace, type, and member group
* No external tooling required

As Raven evolves, RavenDoc can evolve with it — without breaking existing documentation.
