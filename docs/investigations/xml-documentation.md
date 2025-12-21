# Documentation support for source and metadata symbols

## Goal
Capture the requirements and design options for adding documentation comments (XML and Markdown) to Raven. The feature should cover authoring documentation in source, loading metadata documentation from referenced assemblies, and surfacing documentation through the public APIs in a Roslyn-like manner. Markdown is supported for readability and parity with popular tooling while retaining XML for compatibility with existing guidance and metadata files.

## Scope and constraints
- **In scope**: tokenizing and attaching doc comments from source files, projecting documentation onto symbols in the semantic model, importing external XML documentation files for metadata references, and API changes that expose documentation (XML and Markdown) to tooling.
- **Out of scope for the first phase**: generating XML documentation files during compilation, rich formatting of documentation (`<see>` resolution, cross-linking), and editor tooling.
- **Compatibility**: align with the C# XML documentation comment shape where possible so existing guidance and Roslyn behaviors are a clear reference point. Markdown should follow CommonMark where feasible and degrade gracefully when rendered as plaintext. Markdown is the default authoring format when no explicit format is supplied.

## Current state
- The compiler records a `ParseOptions.DocumentationMode` flag, but it is unused elsewhere and no parser hooks exist for triple-slash (`///`) or block (`/** */`) doc comments. Comments are currently ignored by the syntax layer and never attached to bound symbols.
- Metadata references do not load external documentation files, and symbol APIs do not expose documentation content.

## Source documentation pipeline
1. ✅ **Lexing and trivia**
   - Recognize single-line (`///`) and block (`/** ... */`) doc comments as structured trivia and preserve them on syntax tokens. Mirror Roslyn’s behavior of ignoring leading whitespace indentation and validating that documentation comments appear immediately before a declaration (skipping only whitespace and newlines).
   - Respect `ParseOptions.DocumentationMode`: when disabled, skip expensive parsing/validation but still capture trivia so the mode can be toggled without reparsing.
   - Default to treating doc comments as Markdown unless a different `DocumentationFormat` is provided through parse options.
2. ✅ **Structured parsing**
   - Normalize doc comments into a structured payload that records the chosen documentation format (Markdown by default) and trims common prefixes (leading `///` or `/**`). Raw text is preserved so future diagnostics can map back to source spans.
   - Support the common XML elements Roslyn validates (`<summary>`, `<param>`, `<typeparam>`, `<returns>`, `<value>`, `<remarks>`, `<see>`, `<inheritdoc>`, `<exception>`, `<example>`), but treat unrecognized tags as well-formed text rather than hard errors initially. For Markdown, keep fenced code blocks and inline links intact so renderers can process them later.
3. ✅ **Binding to symbols**
   - Source symbols now expose a `DocumentationComment` that is computed from their declaring syntax references, honoring the syntax tree’s `ParseOptions.DocumentationFormat` (Markdown by default) and normalizing triple-slash/block documentation trivia. Partial declarations merge documentation blocks in declaration order.
   - Parameter/tag validation and inheritance diagnostics are still pending; the current binding stage only associates raw documentation with the declared symbol to unblock API consumers and future diagnostics work.
4. ☐ **Diagnostics**
   - Add diagnostics for malformed XML (unterminated tags, invalid characters), malformed Markdown (unclosed fences), misplaced documentation comments (not attached to a declaration), and parameter/type parameter mismatches. Respect `DocumentationMode` to suppress documentation diagnostics when disabled.

## Loading metadata documentation
1. **Documentation provider abstraction**
   - Introduce a `DocumentationProvider` concept (akin to Roslyn’s) that is associated with each metadata reference. The provider should expose a `GetDocumentation` method keyed by documentation identifiers (e.g., `M:Namespace.Type.Method(System.Int32)`) and return a discriminated shape (e.g., `DocumentationKind.Xml | DocumentationKind.Markdown`) alongside the raw payload.
   - For source compilations, the provider can look up documentation stored on symbols. For metadata references, a file-based provider should read sidecar XML files located next to the referenced assembly (same name, `.xml` extension) and optional Markdown files (`.md`) when available. Prefer XML when both are present to preserve compatibility.
2. **File resolution**
   - When resolving metadata references, probe for XML and Markdown documentation files using the same base path as the assembly and optional culture-specific subfolders (e.g., `en-US/Library.xml`, `en-US/Library.md`). Prefer invariant culture, then walk culture fallbacks.
   - Cache parsed documentation documents per reference and surface I/O failures as non-fatal diagnostics so missing docs don’t block compilation.
3. **Identifier mapping**
   - Emit documentation identifiers for symbols following the ECMA-335 ID string format (matching Roslyn). This enables reuse of existing XML documentation files and avoids inventing a new schema.
   - Ensure synthesized symbols that are visible to consumers (e.g., lowered async helpers) either map to their originating user symbol or are omitted from documentation lookup to avoid exposing internal details.

## Public API surface
1. **Symbol APIs**
   - Add a `GetDocumentationComment` API to `Symbol` (and/or a richer `DocumentationComment` wrapper) that resolves documentation across source and metadata, applies `<inheritdoc/>` if supported, and returns a structured result that records the format (XML vs. Markdown), the raw payload, and a normalized plaintext representation.
   - Provide a `DocumentationCommentFormatted` helper for consumers that want plaintext or Markdown summaries without implementing XML rendering themselves, mirroring Roslyn’s `GetDocumentationCommentId` + formatter pattern. For XML inputs, the helper should offer Markdown and plaintext projections; for Markdown inputs, it should avoid lossy round-trips and preserve inline links when possible.
2. **Semantic model and completion**
   - Semantic queries (hover/Quick Info, completion tooltips) should retrieve documentation via the new symbol API to ensure consistent handling of inherited docs and metadata-loaded comments regardless of source format.
   - Consider optional lazy loading so requesting documentation does not force XML parsing for symbols that are never queried and so Markdown parsing happens only when needed.
3. **Compilation options**
   - Extend `ParseOptions` or add a dedicated `DocumentationOptions` bag to control documentation mode, whether to resolve `<inheritdoc>`, whether to load external documentation files, and which formats to prefer when both XML and Markdown are available. Default to loading metadata documentation when references include an XML or Markdown file.

## Compiler switches
- Raven.Compiler accepts `--emit-docs` (optional path) to write collected documentation comments. Markdown output is the default (`.md`); `--doc-format xml` switches to XML and adjusts the default file extension accordingly.

## Testing strategy
- **Parser and diagnostics**: unit tests for trivia recognition, structured parsing, XML/Markdown validation, and parameter validation. Include cases with `DocumentationMode` off to confirm diagnostics are suppressed.
- **Symbol binding**: tests that attach comments to declarations, merge partials, and report errors for mismatched parameters or misplaced comments across both formats.
- **Metadata loading**: tests that associate XML and Markdown files with metadata references, resolve culture-specific docs, and tolerate missing/invalid files while returning null documentation.
- **API behavior**: verify `GetDocumentationComment` returns the expected payload format for source and metadata symbols (including inherited docs where applicable) and that completion/Quick Info surfaces the resolved documentation.
- **Update documentation**: Update language specification

## Open questions
- Should `<inheritdoc>` be fully resolved during binding or lazily in the API layer? Roslyn resolves lazily to avoid eager graph walks, but eager resolution could simplify tooling at the cost of upfront work.
- Do we need a lightweight format (e.g., a summarized string) for runtime reflection use, or is XML/Markdown sufficient for all planned tooling?
- How should documentation be surfaced for synthesized members (pattern-based codegen, implicit constructors) where no source comment exists? Options include suppressing documentation, redirecting to the containing symbol, or authoring synthesized summaries.
- Should Markdown authored docs flow into generated XML files for consumers that only ingest XML, or should we emit parallel `.md` outputs? If we project Markdown into XML, how lossy is the conversion?