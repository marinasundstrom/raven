# Raven Documentation Model

Raven documentation is a language service in its own right. It is not
Markdown-flavored XML documentation and it is not tied to a particular site
generator.

The Raven documentation model and its public APIs use Raven concepts and
vocabulary. XML documentation is an input/output adapter at the .NET boundary,
not the base class, schema, or naming authority for that API.

## Principles

- Markdown is the default authoring format for Raven source.
- XML documentation remains a supported, explicit authoring format.
- A Raven-native semantic model represents documentation independently of the
  syntax used to author it.
- Loaders and projectors adapt external formats without leaking their schemas
  into the core API.
- Markdown sidecars are the preferred representation for Raven libraries.
- XML sidecars are emitted for .NET ecosystem compatibility.
- Importing documentation must accept both Markdown and XML, preferring
  Markdown when both describe the same metadata symbol.
- Projections should preserve everything their target format can express and
  must not silently redefine Raven's richer source model around XML's limits.

The canonical flow is:

```text
Markdown source (default) ─┐
                          ├─> Raven documentation model ─> Markdown sidecar
XML source (explicit) ────┘                              └> XML sidecar
```

The source syntax, semantic model, storage format, and rendered editor or site
experience are separate layers. This lets Raven evolve its documentation
language without making either XML or generated HTML the internal contract.

## Markdown influence

Raven's Markdown comments are loosely modeled on Java's Markdown documentation
comments: normal Markdown is readable in source, code spans and fenced blocks
remain literal, and documentation-specific structure can coexist with
Markdown. Raven does not inherit JavaDoc's complete tag grammar, doclet model,
or HTML-oriented output contract.

That influence is a starting point rather than a compatibility requirement.
New Raven documentation constructs should be judged by how naturally they read
in Markdown, how well tools can understand them, and how predictably they
project to interoperability formats.

## Semantic content

The documentation model should be symbol-addressed and preserve both authored
content and commonly understood semantic roles. Its initial structured fields
include:

- summary
- remarks
- parameters and type parameters
- return value
- exceptions
- examples
- related symbols and links
- arbitrary Markdown content that has no narrower structured role

The model should retain source-format information and raw authored content
where needed. Normalization is for semantic access and projection; it must not
prevent an editor or RavenDoc from presenting the author's Markdown faithfully.

XML input maps recognized elements into the same roles. Unrecognized XML should
be preserved where practical instead of being mistaken for Markdown.

The initial compiler API represents this as `RavenDocumentation`:

- ordered `DocumentationSection` values carry narrative roles such as summary,
  details, result, remarks, and examples
- `DocumentationAssociation` values attach content to subjects such as
  parameters, errors, and related links
- `SourceFormat` and `SourceText` preserve the loaded representation at the
  adapter boundary
- `InheritedFrom` carries an optional symbol relationship without exposing an
  XML element

`RavenDocumentationLoader` adapts Markdown or XML comments into this model.
Projectors consume the model and choose the target vocabulary. The older
format-shaped extraction types remain compatibility helpers while consumers
migrate to the Raven model.

## Tags, roles, and aliases

.NET XML documentation defines important compatibility semantics, but its
element names are neither Raven's source grammar nor its internal API. Raven
defines its own concepts first. Where one of those concepts genuinely
corresponds to a role that .NET tools expect—such as parameters, type
parameters, return values, exceptions, references, or inheritance—the .NET
adapter maps between them.

Markdown comments may expose familiar XMLDoc names where that improves
discoverability. Raven may also provide clearer Raven-native names and aliases.
Aliases normalize to the same semantic role before validation or projection;
they are not emitted as competing XML vocabularies.

An alias is applicable only when the meanings align. Raven should not invent a
false one-to-one correspondence or constrain a richer concept merely to reuse a
Microsoft name.

This compatibility should be permissive:

- accept familiar spellings when their meaning is unambiguous
- allow Raven aliases without changing the projected XML contract
- diagnose invalid symbol or parameter references rather than requiring an
  exact textual imitation of XMLDoc
- preserve Markdown content that has no XML equivalent
- avoid treating every unknown `@` sequence as a hard documentation error,
  especially in literal code

The supported role and alias table should be versioned and documented as it
grows. Adding an XML projection mapping does not automatically require adding a
new source tag.

The initial Markdown aliases are intentionally small:

| Raven role | Accepted Markdown names | .NET XML projection |
| --- | --- | --- |
| parameter | `@param`, `@parameter` | `<param>` |
| type parameter | `@typeparam`, `@typeparameter` | `<typeparam>` |
| result | `@return`, `@returns`, `@result` | `<returns>` |
| remarks | `@remarks`, `@notes` | `<remarks>` |
| example | `@example`, `@examples` | `<example>` |
| error | `@exception`, `@throws` | `<exception>` |
| inherited documentation | `@inheritdoc`, `@inherit` | `<inheritdoc>` |

These names are authoring conveniences. The Raven API exposes roles, not the
chosen spelling.

## Projection

The Markdown projection is the primary Raven projection. It can retain rich
Markdown, Raven-aware links, examples, and content that has no XML equivalent.

The XML projection is a compatibility projection. It maps the subset understood
by .NET XML documentation consumers, including summaries, remarks, parameters,
returns, exceptions, inheritance, and symbol references, using Microsoft's
established XML element shapes. This lets C#, F#, Visual Basic, and conventional
.NET documentation tools consume Raven library documentation. A successful XML
projection does not imply lossless round-tripping of every Raven Markdown
construct.

XML loading follows the reverse path: the .NET adapter parses recognized XML
elements into Raven documentation values, retaining unsupported material where
practical. Consumers query Raven APIs rather than an XML-shaped object model.
Markdown, XML, HTML, or future formats can therefore be added as adapters
without redesigning the core model around any one target.

Libraries build both projections by default. This provides the preferred Raven
experience and conventional XML documentation for existing .NET editors and
tools. Projects can disable the bundle or either projection.

## Consumption

For a metadata reference, Raven resolves documentation in this order:

1. Raven Markdown sidecar entry
2. adjacent .NET XML documentation entry
3. no external documentation

This gives Raven-produced libraries their richer native experience while making
ordinary .NET libraries first-class inputs.

## Editor direction

The language server should understand documentation before rendering it:

- syntax highlighting recognizes Markdown regions and documentation-specific
  constructs without highlighting code spans or fenced code as documentation
  syntax
- completion offers links and semantic documentation constructs in context
- diagnostics validate symbol references and structured fields
- hover renders a concise Markdown fragment
- a dedicated documentation view renders the complete assembled model

Inline editor rendering is a presentation of the semantic model, not a
compiler-generated HTML page.

## Evolution

The current structured tags are an initial bridge. Raven should not add more
JavaDoc- or XMLDoc-shaped syntax merely because an XML element exists. Future
syntax should start from Markdown authoring and Raven editor ergonomics, then
define explicit projections and degradation behavior for XML consumers.
