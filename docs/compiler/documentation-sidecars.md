# Imported union-case documentation

Projected CLI union cases first resolve documentation by their logical Raven case
identity. If absent, lookup falls back to the physical carrier type identity.
Each lookup uses the existing Markdown-first/XML-fallback sidecar rules. This
preserves authored documentation for older reference assemblies without exposing
carrier types as the language-facing union model.

Validated independently on the ordinary .NET compiler with separately emitted
generic union metadata: carrier-only XML descriptions load, logical descriptions
take precedence, and existing emitted Markdown logical-case help remains supported.
RavenDoc and editor help consume the resulting symbol documentation.
