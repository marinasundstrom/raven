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

## Namespace comments

The source declaration index retains namespace declarations so canonical source
namespace symbols expose their locations, syntax references and merged comments.
Merged source/metadata namespaces combine documentation from their constituent
symbols. Semantic-model consumers continue to use ordinary `GetDeclaredSymbol`
and `GetDocumentationComment`; no cache-specific API is needed.

Documentation emission/import recognizes `N:Qualified.Namespace` in Markdown
`.docs` and XML sidecars. CLI namespaces have no independent metadata records;
comments survive through sidecars, not new emitted namespace metadata. A metadata
namespace must exist through its types to import its comments. No Runtime Contract
configuration, target policy, grammar, highlighting, IL or runtime behavior changes.
RavenDoc displays these comments and accepts namespace `N:` IDs in `apiContent`.

Focused .NET tests cover cold file/block namespace queries, split declarations,
independent edited snapshots, Markdown/XML sidecar round trips, merged
source/metadata views and rendered namespace content. This does not validate
execution on .NET Framework, NanoFramework or native neoCLR.
