# **RavenDoc** — Documentation Generator

RavenDoc is Raven’s built-in documentation generator. It extracts documentation written directly in source code and produces a static HTML documentation site.

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

**RavenDoc focuses exclusively on Markdown documentation.**

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

```csharp
var comment = symbol.GetDocumentationComment();

var content = comment?.Content;     // Markdown, without "///"
var rawContent = comment?.RawContent; // Original text, with "///"
```

RavenDoc uses the processed Markdown content (`Content`) for rendering.

## Relationship to metadata sidecars

Raven’s compiler and IDE can also load Markdown documentation for metadata
references from assembly-adjacent `.docs/` sidecars. The built-in compiler
comment emitter now produces that structure for Markdown output, and RavenDoc is
the intended future integration point for exporting richer Raven-authored
documentation into the same metadata-friendly layout.

Important separation:

* Markdown sidecars are authored presentation content.
* XML sidecars are structured interoperability data.
* RavenDoc consumes Markdown plus symbol data; it does not redefine the storage
  format for either Markdown or XML documentation.

RavenDoc should be able to operate over:

* source symbols with attached Markdown comments
* PE symbols with Markdown sidecars
* PE symbols with XML sidecars, while still querying symbol metadata normally

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
