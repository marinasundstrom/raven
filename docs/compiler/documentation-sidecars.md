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

## Function signatures in generated API references

RavenDoc member-list type uses now use the compiler's signature display with named
nominal delegates kept as names. Framework Func/Action callback contracts in parameter
and result tables use function syntax, recursively retaining links to documented type
arguments. Delegate type pages still describe their nominal declarations. Parameter
names do not split mid-identifier in narrow tables.

Twenty RavenDoc generation tests pass, including source and separately emitted metadata
fixtures for completion-only callbacks, callbacks returning generic types, nominal
delegate controls and valid nested type links. No Runtime Contract configuration,
semantic binding, emitted metadata or runtime execution behavior changes.
