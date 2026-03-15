# Editor Documentation Experience

This document defines how Raven documentation should be presented inside the
editor and how that experience relates to metadata sidecars and RavenDoc.

## Goals

- Keep hover fast, focused, and stable.
- Support rich Markdown documentation without turning hover into a full page.
- Reuse the same symbol/documentation model for source symbols and metadata
  symbols.
- Keep RavenDoc as the publishing layer, not the in-editor rendering primitive.
- Allow richer merged content for a symbol without requiring HTML in the IDE.

## Experience split

Raven should expose two distinct documentation experiences in the editor.

### Hover

Hover is for quick local understanding.

It should contain:

- symbol signature
- a short rendered documentation fragment
- optionally a short example or remarks fragment when concise
- a `Read more` or `Open documentation` action when more content exists

It should not try to render the full merged symbol page.

### Documentation view

The editor should also support a dedicated documentation view for a symbol.

This is the place for:

- the full symbol documentation
- merged supplemental Markdown content
- longer examples
- remarks
- related links
- generated symbol metadata sections

This view should open inside the editor as a dedicated documentation surface,
not as generated site HTML.

## Why hover and full-page docs must be separate

Once Raven supports richer Markdown and merged documentation content, the full
documentation for a symbol stops fitting the hover interaction model.

Long hover content causes several problems:

- poor scanability
- unstable layout while typing or moving the cursor
- duplicated content when a symbol has both authored docs and merged content
- too much coupling between publishing-oriented docs and editor UX

The editor should therefore treat hover as a summary surface and the
documentation view as the full reading surface.

## Documentation content model

The editor and RavenDoc should share the same logical documentation model, even
if they render it differently.

The model should distinguish between:

### Symbol documentation fragment

The documentation attached directly to the symbol itself.

Examples:

- Markdown doc comment on a source symbol
- Markdown sidecar entry for a metadata symbol
- XML sidecar entry for a metadata symbol

This is the default content used in hover.

### Expanded symbol page

The assembled documentation page for a symbol.

This can include:

- the symbol documentation fragment
- additional Markdown content that RavenDoc would merge
- generated sections based on symbol metadata
- related symbols and navigation aids

This is the content used by the documentation view.

## Storage and loading implications

The existing sidecar convention remains symbol-addressable and source-neutral:

- source symbols expose attached documentation
- metadata symbols load Markdown sidecars first, then XML sidecars

This storage model is sufficient for both hover and the documentation view.
What differs is how much content gets assembled before rendering.

Additional merged content should not be forced into the symbol-attached
fragment. It can live beside the symbol-addressed docs and be assembled when the
full symbol page is requested.

## Read more / Open documentation

When the editor determines that more documentation exists than is suitable for
hover, it should expose a dedicated action such as:

- `Read more`
- `Open documentation`

That action should open the symbol's documentation page in an editor-hosted view
or virtual document.

Suggested URI shape:

```text
raven-doc:///MyAssembly/T:My.Namespace.Type
raven-doc:///MyAssembly/M:My.Namespace.Type.Method(System.Int32)
```

The URI is an implementation detail, but the important point is that the target
should be stable and symbol-based.

## Rendering model

The IDE should not depend on RavenDoc's final HTML output for in-editor
documentation.

Instead:

- hover renders a documentation fragment
- documentation view renders the assembled symbol page
- RavenDoc renders published HTML from the same underlying symbol/documentation
  model

This keeps HTML as a publishing artifact and avoids coupling editor UX to site
generation.

## Markdown and XML

Markdown and XML continue to serve different purposes:

- Markdown is presentation-oriented authored content.
- XML is structured interoperability data.

Editor rendering should respect that distinction:

- Markdown can be rendered directly as Markdown content.
- XML can be rendered through the structured fields Raven already understands.

The editor should not flatten both into one storage format just to simplify the
UI story.

## Future assembly model

The documentation page for a symbol should eventually be assembled from several
sources when available:

1. the symbol's own documentation fragment
2. additional Markdown content associated with that symbol
3. generated metadata sections
4. cross-reference data and related links

This is intentionally broader than metadata sidecar lookup. Sidecar lookup
solves symbol-attached docs; the documentation page solves full symbol reading.

## Relationship to RavenDoc

RavenDoc remains the publishing/documentation-site layer.

It should be able to:

- load source symbols
- load PE symbols plus documentation sidecars
- assemble a full symbol page
- render HTML for publishing

The editor documentation view should reuse the same underlying symbol/document
assembly model, but render to an editor-native surface instead of published
HTML.

## Initial implementation direction

The recommended order is:

1. Keep hover limited to summary-style fragments.
2. Add a `Read more` or `Open documentation` action for symbols with richer
   content.
3. Introduce a symbol-addressed documentation page model.
4. Expose that page through an editor-hosted virtual document or dedicated
   documentation view.
5. Reuse the same page model in RavenDoc for site generation.

## Non-goals

- opening generated site HTML inside the IDE as the primary documentation
  experience
- making hover responsible for the full merged symbol page
- forcing Markdown and XML into one normalized source format
