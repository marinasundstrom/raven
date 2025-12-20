# XML documentation support for source and metadata symbols

## Goal
Capture the requirements and design options for adding XML documentation comments to Raven. The feature should cover authoring documentation in source, loading metadata documentation from referenced assemblies, and surfacing documentation through the public APIs in a Roslyn-like manner.

## Scope and constraints
- **In scope**: tokenizing and attaching doc comments from source files, projecting documentation onto symbols in the semantic model, importing external XML documentation files for metadata references, and API changes that expose documentation to tooling.
- **Out of scope for the first phase**: generating XML documentation files during compilation, rich formatting of documentation (markdown rendering, `<see>` resolution), and editor tooling.
- **Compatibility**: align with the C# XML documentation comment shape where possible so existing guidance and Roslyn behaviors are a clear reference point.

## Current state
- The compiler records a `ParseOptions.DocumentationMode` flag, but it is unused elsewhere and no parser hooks exist for triple-slash (`///`) or block (`/** */`) doc comments. Comments are currently ignored by the syntax layer and never attached to bound symbols.
- Metadata references do not load external documentation files, and symbol APIs do not expose documentation content.

## Source documentation pipeline
1. **Lexing and trivia**
   - Recognize single-line (`///`) and block (`/** ... */`) doc comments as structured trivia and preserve them on syntax tokens. Mirror Roslyn’s behavior of ignoring leading whitespace indentation and validating that documentation comments appear immediately before a declaration (skipping only whitespace and newlines).
   - Respect `ParseOptions.DocumentationMode`: when disabled, skip expensive parsing/validation but still capture trivia so the mode can be toggled without reparsing.
2. **Structured parsing**
   - Parse trivia into a structured XML node tree (or retain raw text plus offsets) so diagnostics can point to malformed tags. Reuse the existing lexer infrastructure to track line/column for precise error reporting.
   - Support the common elements Roslyn validates (`<summary>`, `<param>`, `<typeparam>`, `<returns>`, `<value>`, `<remarks>`, `<see>`, `<inheritdoc>`, `<exception>`, `<example>`), but treat unrecognized tags as well-formed text rather than hard errors initially.
3. **Binding to symbols**
   - During symbol creation, attach the nearest leading documentation trivia to the declared symbol. For partial types/members, merge documentation blocks in declaration order; emit a diagnostic on conflicting `<summary>` content to match Roslyn’s duplicate documentation rules.
   - Resolve `<param>`/`<typeparam>` names against the symbol signature and issue diagnostics for missing or extra tags. For interface implementations and overrides, keep the raw documentation on the declaration and defer inheritance logic to the API layer.
4. **Diagnostics**
   - Add diagnostics for malformed XML (unterminated tags, invalid characters), misplaced documentation comments (not attached to a declaration), and parameter/type parameter mismatches. Respect `DocumentationMode` to suppress documentation diagnostics when disabled.

## Loading metadata documentation
1. **Documentation provider abstraction**
   - Introduce a `DocumentationProvider` concept (akin to Roslyn’s) that is associated with each metadata reference. The provider should expose a `GetDocumentation` method keyed by documentation identifiers (e.g., `M:Namespace.Type.Method(System.Int32)`).
   - For source compilations, the provider can look up documentation stored on symbols. For metadata references, a file-based provider should read sidecar XML files located next to the referenced assembly (same name, `.xml` extension).
2. **XML file resolution**
   - When resolving metadata references, probe for an XML documentation file using the same base path as the assembly and optional culture-specific subfolders (e.g., `en-US/Library.xml`). Prefer invariant culture, then walk culture fallbacks.
   - Cache parsed XML documents per reference and surface I/O failures as non-fatal diagnostics so missing docs don’t block compilation.
3. **Identifier mapping**
   - Emit documentation identifiers for symbols following the ECMA-335 ID string format (matching Roslyn). This enables reuse of existing XML documentation files and avoids inventing a new schema.
   - Ensure synthesized symbols that are visible to consumers (e.g., lowered async helpers) either map to their originating user symbol or are omitted from documentation lookup to avoid exposing internal details.

## Public API surface
1. **Symbol APIs**
   - Add a `GetDocumentationCommentXml`-style API to `Symbol` (and/or a richer `DocumentationComment` wrapper) that resolves documentation across source and metadata, applies `<inheritdoc/>` if supported, and returns either raw XML or a structured model.
   - Provide a `DocumentationCommentFormatted` helper for consumers that want plaintext/markdown summaries without implementing XML rendering themselves, mirroring Roslyn’s `GetDocumentationCommentId` + formatter pattern.
2. **Semantic model and completion**
   - Semantic queries (hover/Quick Info, completion tooltips) should retrieve documentation via the new symbol API to ensure consistent handling of inherited docs and metadata-loaded comments.
   - Consider optional lazy loading so requesting documentation does not force XML parsing for symbols that are never queried.
3. **Compilation options**
   - Extend `ParseOptions` or add a dedicated `DocumentationOptions` bag to control documentation mode, whether to resolve `<inheritdoc>`, and whether to load external XML files. Default to loading metadata documentation when references include an XML file.

## Testing strategy
- **Parser and diagnostics**: unit tests for trivia recognition, structured parsing, and XML/parameter validation. Include cases with `DocumentationMode` off to confirm diagnostics are suppressed.
- **Symbol binding**: tests that attach comments to declarations, merge partials, and report errors for mismatched parameters or misplaced comments.
- **Metadata loading**: tests that associate XML files with metadata references, resolve culture-specific docs, and tolerate missing/invalid files while returning null documentation.
- **API behavior**: verify `GetDocumentationCommentXml` returns the expected XML for source and metadata symbols, including inherited docs where applicable, and that completion/Quick Info surfaces the resolved documentation.

## Open questions
- Should `<inheritdoc>` be fully resolved during binding or lazily in the API layer? Roslyn resolves lazily to avoid eager graph walks, but eager resolution could simplify tooling at the cost of upfront work.
- Do we need a lightweight format (e.g., a summarized string) for runtime reflection use, or is XML sufficient for all planned tooling?
- How should documentation be surfaced for synthesized members (pattern-based codegen, implicit constructors) where no source comment exists? Options include suppressing documentation, redirecting to the containing symbol, or authoring synthesized summaries.
