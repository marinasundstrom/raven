# External Documentation Sidecars

Raven supports loading external documentation for metadata symbols from files
stored next to a referenced assembly. The convention is designed to work both
for normal build output folders and for NuGet package layouts.

## Goals

- Support richer Markdown documentation for metadata symbols.
- Preserve ordinary `.xml` documentation compatibility for .NET libraries.
- Prefer Markdown when both formats are available.
- Use a stable symbol identity that matches the existing .NET documentation
  ecosystem.
- Leave room for RavenDoc to become a producer of Markdown sidecars later,
  without making RavenDoc a required runtime dependency for lookup.

## Assembly-adjacent layout

Given an assembly:

```text
MyLib.dll
MyLib.xml
MyLib.docs/
  manifest.json
  invariant/
    symbols/
      T/
      M/
      P/
      E/
      F/
```

The `.docs` directory is adjacent to the assembly and uses the same base name.

Example:

```text
lib/net10.0/MyLib.dll
lib/net10.0/MyLib.xml
lib/net10.0/MyLib.docs/
```

This same layout is valid for local build output and for NuGet packages under
`lib/<tfm>/`.

## Lookup precedence

When Raven resolves documentation for a metadata symbol from a referenced
assembly:

1. Probe `<AssemblyName>.docs/manifest.json`
2. If a Markdown symbol file exists for the requested symbol, use it
3. Otherwise probe `<AssemblyName>.xml`
4. If neither source contains the symbol, return no external documentation

Source symbols still use attached source documentation comments directly.

## Documentation model

Markdown and XML serve different purposes and should remain distinct:

- Markdown is authored presentation content.
- XML is structured interoperability data.
- RavenDoc is the HTML/documentation-site layer, not the storage format for
  either one.

This means:

- Markdown sidecars store raw Markdown exactly as authored.
- Adjacent XML sidecars remain XML and are not treated as Markdown artifacts.
- The symbol API returns the loaded format as-is, so consumers such as the IDE
  or RavenDoc can decide how to render it.

## Manifest

`manifest.json` describes the sidecar structure. Version 1 uses a minimal
schema:

```json
{
  "formatVersion": 1,
  "assemblyName": "MyLib",
  "documentationFormat": "markdown",
  "idFormat": "doc-comment-id",
  "defaultLocale": "invariant",
  "locales": ["invariant"],
  "symbolsPath": "symbols"
}
```

Notes:

- `formatVersion` is required and must currently be `1`.
- `symbolsPath` is optional and defaults to `symbols`.
- `assemblyName`, `documentationFormat`, and `idFormat` are descriptive in v1
  and may be validated more strictly later.
- `defaultLocale` and `locales` reserve the localization shape. The compiler
  currently emits an invariant locale root only.

## Symbol identity

Symbol files are keyed by XML documentation comment IDs.

Examples:

- `T:Raven.CodeAnalysis.Syntax.SyntaxFactory`
- `M:Raven.CodeAnalysis.Syntax.SyntaxFactory.StoredPropertyDeclaration(...)`
- `P:My.Namespace.Widget.Title`

Raven uses these IDs for both XML and Markdown metadata documentation so that:

- the identity format is already well understood,
- metadata lookup stays compatible with the .NET ecosystem,
- Markdown and XML can coexist for the same referenced assembly.

The shared symbol identity does not imply a shared content model. It only gives
both formats the same addressing scheme.

## Markdown symbol file paths

For v1, the symbol file path is:

```text
<docs-root>/<locale>/<symbolsPath>/<kind>/<sha256-hex-of-doc-id-without-prefix>.md
```

This keeps filenames stable and avoids path-length issues for long generic
member signatures.

The first character before the documentation ID colon becomes the kind
directory:

- `T`
- `M`
- `P`
- `E`
- `F`

The remainder of the documentation ID is hashed using SHA-256 and written as a
lowercase hexadecimal filename.

The built-in compiler emitter currently writes all Markdown sidecars under the
`invariant/` locale root.

## Markdown file content

Each symbol file contains raw Markdown only.

Example:

```md
# StoredPropertyDeclaration

Creates a stored property declaration with an initializer.

## Remarks

Alias for `PropertyDeclaration`. Prefer the canonical factory name unless the
alias is clearer at the call site.
```

No front matter is required in v1.

## Emission and future RavenDoc integration

Raven’s metadata lookup does not require RavenDoc.

However, RavenDoc is the natural future producer for Markdown sidecars because
it already understands Raven-authored Markdown documentation and symbol-aware
links. A later step can add:

- RavenDoc generation that targets the sidecar layout directly
- packaging support so Markdown sidecars flow into NuGet output
- localized documentation emission beyond the invariant root

The built-in compiler comment emitter already produces the `.docs/` layout for
Markdown output. XML output remains a normal `.xml` sidecar. The loader
intentionally treats RavenDoc as a future producer concern, not a lookup
dependency.

For the editor-facing presentation story built on top of these sidecars, see
[Editor Documentation Experience](./editor-documentation-experience.md).

## Project properties

The first project-system knobs for this convention are:

- `GenerateDocumentationFile`
  Emits adjacent XML documentation.
- `GenerateMarkdownDocumentationFile`
  Emits assembly-adjacent Markdown sidecars.
- `DocumentationFile`
  Optional explicit XML output path.
- `MarkdownDocumentationOutputPath`
  Optional explicit Markdown sidecar output path.

These are carried through Raven's MSBuild workspace model so open/save
round-trips preserve the documentation configuration even when the actual build
execution still comes from the compiler/CLI path.

## Current implementation status

Implemented:

- assembly-adjacent Markdown sidecar probing
- manifest-based Markdown lookup
- compiler emission of `.docs/` sidecars from source documentation comments
  under an invariant locale root
- Markdown-over-XML precedence for metadata symbols
- XML fallback from adjacent sidecar XML documentation files

Not yet implemented:

- RavenDoc generation into this structure
- multi-language/localized documentation variants beyond the reserved invariant
  locale root
- richer manifest validation
