# Markdown Docs

This sample shows Raven-authored Markdown documentation comments, structured
roles and aliases, `xref:` links, editor highlighting, and the compiler's
default XML/Markdown documentation emission across sibling projects.

Projects:

- `library/MarkdownDocs.Library.rvnproj`
- `consumer/MarkdownDocs.Consumer.rvnproj`

## Build the library

```bash
dotnet build library/MarkdownDocs.Library.rvnproj --property WarningLevel=0 -o bin/library
```

## Build the consumer

```bash
dotnet build consumer/MarkdownDocs.Consumer.rvnproj --property WarningLevel=0 -o bin/consumer
```

## Generated documentation

After a successful build, the compiler writes both:

- `bin/library/MarkdownDocs.Library.xml`
- `bin/library/MarkdownDocs.Library.docs/`
- `bin/consumer/MarkdownDocs.Consumer.xml`
- `bin/consumer/MarkdownDocs.Consumer.docs/`

The Markdown sidecars preserve authored Markdown, including `xref:` links and
structured roles written with names such as `@parameter`, `@result`, and the
familiar `@returns` alias. The XML files project those roles into standard .NET
XML documentation elements.

Neither project configures documentation properties. Raven library projects
emit both projections by default. Set `RavenGenerateDocumentation=false` to
disable that bundle, or override the individual output properties when only one
projection is wanted.

Each emitted Markdown symbol file may also begin with metadata-only frontmatter
such as:

```md
---
xref: M:Samples.Docs.WidgetFactory.#ctor
---
```

That frontmatter is not rendered in hover or documentation views. It exists
only to bind the sidecar file to a specific symbol.

If you want to inspect one directly, start with a library output under:

- `bin/library/MarkdownDocs.Library.docs/invariant/symbols/`

For example:

- `bin/library/MarkdownDocs.Library.docs/invariant/symbols/M/`

## Suggested IDE checks

- Hover `Widget`, `GetTitle`, `WidgetFactory`, and `WidgetPrinter`
- Confirm headings, links, inline code, fenced Raven code, and documentation
  tags receive distinct editor highlighting
- Inspect the emitted `manifest.json` files in both `.docs` directories
- Inspect one of the emitted library `.md` files and confirm the top-of-file
  `xref` frontmatter is present
- Open the whole `markdown-docs` folder in VS Code and test navigation across
  the sibling projects
- Compare `@result` and `@returns` in the source with the emitted `<returns>`
  elements in XML

## RavenDoc handoff

This sample stops at source documentation and assembly-adjacent sidecars.
RavenDoc is the publishing stage that turns the same documentation into an HTML
site. It can obtain Markdown in either of two ways:

1. compile/read the Raven source projects and extract comments from source
   symbols
2. load a compiled library and its adjacent
   `MarkdownDocs.Library.docs/` sidecar

Those paths converge on the Raven documentation model before RavenDoc assembles
symbol pages and renders HTML. The XML sidecar exists for .NET compatibility;
it is not the preferred RavenDoc publishing input.
