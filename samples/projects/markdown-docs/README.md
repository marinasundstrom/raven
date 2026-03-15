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

## Suggested IDE checks

- Hover `Widget`, `GetTitle`, `WidgetFactory`, and `WidgetPrinter`
- Inspect the emitted `manifest.json` files in both `.docs` directories
- Open the whole `markdown-docs` folder in VS Code and test navigation across
  the sibling projects
- Use RavenDoc against the built assemblies in a later step
