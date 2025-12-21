# Documentation support for source and metadata symbols

## Goal

Capture the requirements and design options for adding documentation comments (XML and Markdown) to Raven. The feature should cover authoring documentation in source, loading metadata documentation from referenced assemblies, and surfacing documentation through the public APIs in a Roslyn-like manner.

**XML documentation is treated as a compiler artifact and can be emitted directly during compilation.**
**Markdown is supported as an authoring format for readability and parity with popular tooling, but rendering Markdown into HTML (or other rich outputs) is handled by external tools that consume Raven’s public APIs or emitted documentation artifacts.**

## Scope and constraints

* **In scope**: tokenizing and attaching doc comments from source files, projecting documentation onto symbols in the semantic model, importing external XML documentation files for metadata references, and API changes that expose documentation (XML and Markdown) to tooling.
* **Out of scope for the first phase**:

  * Rich formatting and rendering (HTML generation, styling, cross-linking)
  * Editor tooling
  * Markdown-to-HTML conversion
* **Compiler responsibility**: parse, validate, bind, and (optionally) emit **XML documentation** as part of the build.
* **Tooling responsibility**: process Markdown documentation (including doc tags) and render it into HTML or other presentation formats.
* **Compatibility**: align with the C# XML documentation comment shape where possible so existing guidance and Roslyn behaviors are a clear reference point. Markdown should follow CommonMark where feasible and degrade gracefully when rendered as plaintext. Markdown is the default authoring format when no explicit format is supplied.

## Current state

* The compiler records a `ParseOptions.DocumentationMode` flag, but it is unused elsewhere and no parser hooks exist for triple-slash (`///`) or block (`/** */`) doc comments. Comments are currently ignored by the syntax layer and never attached to bound symbols.
* Metadata references do not load external documentation files, and symbol APIs do not expose documentation content.

## Source documentation pipeline

1. ✅ **Lexing and trivia**

   * Recognize single-line (`///`) and block (`/** ... */`) doc comments as structured trivia and preserve them on syntax tokens. Mirror Roslyn’s behavior of ignoring leading whitespace indentation and validating that documentation comments appear immediately before a declaration (skipping only whitespace and newlines).
   * Respect `ParseOptions.DocumentationMode`: when disabled, skip expensive parsing/validation but still capture trivia so the mode can be toggled without reparsing.
   * Default to treating doc comments as Markdown unless a different `DocumentationFormat` is provided through parse options.
2. ✅ **Structured parsing**

   * Normalize doc comments into a structured payload that records the chosen documentation format (Markdown by default) and trims common prefixes (leading `///` or `/**`). Raw text is preserved so future diagnostics can map back to source spans.
   * Support the common XML elements Roslyn validates (`<summary>`, `<param>`, `<typeparam>`, `<returns>`, `<value>`, `<remarks>`, `<see>`, `<inheritdoc>`, `<exception>`, `<example>`), but treat unrecognized tags as well-formed text rather than hard errors initially.
   * For Markdown, preserve fenced code blocks, inline links, and embedded doc tags so that **external renderers** can process them later.
3. ☐ **Binding to symbols**

   * During symbol creation, attach the nearest leading documentation trivia to the declared symbol. For partial types/members, merge documentation blocks in declaration order; emit a diagnostic on conflicting summaries to match Roslyn’s duplicate documentation rules.
   * Resolve XML `<param>`/`<typeparam>` names against the symbol signature and issue diagnostics for missing or extra tags.
   * For Markdown, resolve parameter documentation by matching heading/list labels to the signature (best-effort) and capture unmatched entries for diagnostics.
   * For interface implementations and overrides, keep the raw documentation on the declaration and defer inheritance logic to the API layer.
4. ☐ **Diagnostics**

   * Add diagnostics for malformed XML (unterminated tags, invalid characters), malformed Markdown (unclosed fences), misplaced documentation comments (not attached to a declaration), and parameter/type parameter mismatches.
   * Respect `DocumentationMode` to suppress documentation diagnostics when disabled.

## Loading metadata documentation

1. **Documentation provider abstraction**

   * Introduce a `DocumentationProvider` concept (akin to Roslyn’s) that is associated with each metadata reference.
   * The provider exposes documentation keyed by documentation identifiers (e.g., `M:Namespace.Type.Method(System.Int32)`) and returns the raw payload along with its `DocumentationFormat`.
   * For metadata references, load sidecar XML documentation files (`.xml`) located next to the referenced assembly.
   * Optional Markdown sidecar files (`.md`) may be supported for tooling scenarios, but **XML remains the authoritative metadata format** for compatibility.
2. **File resolution**

   * When resolving metadata references, probe for XML documentation files using the same base path as the assembly and optional culture-specific subfolders.
   * Cache parsed documentation documents per reference and surface I/O failures as non-fatal diagnostics so missing docs don’t block compilation.
3. **Identifier mapping**

   * Emit documentation identifiers following the ECMA-335 ID string format (matching Roslyn).
   * Ensure synthesized symbols visible to consumers either map back to their originating user symbol or are excluded from documentation lookup.

## Public API surface

1. **Symbol APIs**

   * Add `GetDocumentationComment` (or a richer `DocumentationComment` wrapper) to `Symbol`.
   * The API resolves documentation across source and metadata and returns:

     * format (XML or Markdown)
     * raw payload
     * optional normalized plaintext representation
   * The compiler does **not** render Markdown to HTML; consumers are expected to use external tools for that step.
2. **Semantic model and completion**

   * Semantic queries (hover/Quick Info, completion tooltips) retrieve documentation through the symbol API to ensure consistent handling of source, metadata, and inheritance.
   * Allow lazy loading so documentation parsing does not occur unless requested.
3. **Compilation options**

   * Extend `ParseOptions` or introduce `DocumentationOptions` to control:

     * documentation mode
     * metadata documentation loading
     * preferred documentation formats when multiple are available

## Compiler switches

* `--emit-xml-docs [path]`

  * Emits standard XML documentation directly as part of the compilation output.
  * Mirrors Roslyn’s XML documentation emission model.
* **Markdown output is not emitted directly by the compiler.**

  * Markdown-authored documentation is exposed through APIs or intermediate artifacts and is intended to be consumed by **external documentation tools** that handle tag processing and HTML rendering.

## Documentation emission pipeline

* **Symbol discovery**: after binding succeeds, walk the compilation starting from the global namespace and visit publicly reachable symbols in a deterministic order.
* **XML output (compiler responsibility)**:

  * Project each documented symbol into the standard XML documentation structure:

    ```
    <doc>
      <assembly>...</assembly>
      <members>...</members>
    </doc>
    ```
  * Use documentation identifiers and emit the XML payload without further transformation.
* **Markdown processing (external tooling)**:

  * External tools consume Raven’s symbol APIs or emitted documentation artifacts.
  * These tools are responsible for:

    * resolving `<see>`-like references
    * handling `<inheritdoc>`
    * converting Markdown to HTML
    * generating navigation, indexes, and cross-links
  * This separation allows richer rendering without coupling presentation logic to the compiler.

## Testing strategy

* **Parser and diagnostics**: unit tests for trivia recognition, structured parsing, XML/Markdown validation, and parameter validation.
* **Symbol binding**: tests for attachment, partial merging, and diagnostics across both formats.
* **Metadata loading**: tests for XML association and graceful failure handling.
* **API behavior**: verify `GetDocumentationComment` returns correct formats and payloads for source and metadata symbols.
* **Update documentation**: update the language specification to reflect the XML-vs-Markdown responsibilities.

## Open questions

* Should `<inheritdoc>` be resolved eagerly during binding or lazily in the API layer?
* Do we need a lightweight documentation representation for runtime reflection scenarios?
* How should documentation behave for synthesized members?
* Should Markdown-authored documentation be projected into XML during XML emission, or should XML emission remain strictly XML-authored only to avoid lossy conversions?