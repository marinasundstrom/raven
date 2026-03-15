# Markdown Docs

This sample shows Raven-authored Markdown documentation comments, structured
doc tags, `xref:` links, and the compiler's XML/Markdown documentation
sidecar emission across sibling projects.

Projects:

- `library/MarkdownDocs.Library.rvnproj`
- `consumer/MarkdownDocs.Consumer.rvnproj`

## Build the library

```bash
dotnet run --project ../../../src/Raven.Compiler --property WarningLevel=0 -- library/MarkdownDocs.Library.rvnproj -o bin/library
```

## Build the consumer

```bash
dotnet run --project ../../../src/Raven.Compiler --property WarningLevel=0 -- consumer/MarkdownDocs.Consumer.rvnproj -o bin/consumer
```

## Generated documentation

After a successful build, the compiler writes both:

- `bin/library/docs/MarkdownDocs.Library.xml`
- `bin/library/docs/MarkdownDocs.Library.docs/`
- `bin/consumer/docs/MarkdownDocs.Consumer.xml`
- `bin/consumer/docs/MarkdownDocs.Consumer.docs/`

The Markdown sidecars preserve authored Markdown, including `xref:` links and
structured tags such as `@param` and `@returns`. The XML files remain standard
.NET XML documentation output.

This sample explicitly sets:

- `GenerateXmlDocumentationFromMarkdownComments=true`

so the project also demonstrates Markdown-to-XML projection. Without that
property, Markdown-authored comments still emit to `.docs`, but they are not
projected into the emitted `.xml` file.

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

- `bin/library/docs/MarkdownDocs.Library.docs/invariant/symbols/`

For example:

- `bin/library/docs/MarkdownDocs.Library.docs/invariant/symbols/M/6db33e7859e9ad17c69fd2f122df9874f465ebecee652a6f22c55e1dbdf39d2a.md`

## Suggested IDE checks

- Hover `Widget`, `GetTitle`, `WidgetFactory`, and `WidgetPrinter`
- Inspect the emitted `manifest.json` files in both `.docs` directories
- Inspect one of the emitted library `.md` files and confirm the top-of-file
  `xref` frontmatter is present
- Open the whole `markdown-docs` folder in VS Code and test navigation across
  the sibling projects
- Use RavenDoc against the built assemblies in a later step
