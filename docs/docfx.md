# Build the Raven documentation

The published documentation site is assembled with DocFX. The repository pins
the DocFX version in `.config/dotnet-tools.json`; a separate global installation
is not required.

## Build the site

From the repository root, run:

```bash
scripts/build-docs.sh
```

The generated site is written to `_site/`. To build and serve it locally, run:

```bash
scripts/build-docs.sh --serve
```

Then open <http://localhost:8080>.

## Raven theme

The custom DocFX template lives in `docs/template/`. Its `public/main.css` and
`public/main.js` adapt the modern DocFX template to Raven, including Raven code
highlighting and automatic light/dark theme support. The canonical shared color,
typography, and surface tokens live in
`docs/template/public/raven-theme.css`; RavenDoc and the Playground consume that
same file so the three surfaces remain visually connected.

Raven's purple accent, typography, cards, borders, and page surfaces define the
site shell. Code is a distinct shared layer modeled on Visual Studio Code's
Light+ and Dark+ editor themes. The shared theme defines the code background,
foreground, and syntax-token colors; the DocFX highlighter and RavenDoc consume
those variables, while the Playground's Monaco themes use the corresponding
editor and token palette. Changes to code colors should be applied consistently
to all three surfaces.

The home page presents three representative Raven examples in a carousel. It
advances every six seconds, pauses while the user hovers or interacts with it,
supports arrow-key tab navigation, and does not auto-advance when the user
prefers reduced motion. Samples should remain short enough to avoid changing the
hero's overall height as the carousel advances. Preserve normal source
formatting in every sample: indentation communicates nesting, and blank lines
should separate distinct declarations, constructs, and top-level operations.

Keep the global header deliberately compact. Detailed documentation hierarchy
belongs in the Docs menu and section sidebars, while the root page serves as a
dedicated introduction to Raven.

## Publication boundary

`docs/docfx.json` explicitly lists public learning material, language reference,
the tooling pages needed to use Raven, and a separate compiler API section.
Compiler architecture and implementation details, contributor instructions,
testing notes, investigations, language proposals, historical material, and
standalone design work remain available in source control but are intentionally
excluded from the user-facing site.

## Validation

The build restores the pinned tool and generates the site with DocFX warnings
treated as errors. Broken links and unresolved cross-references therefore fail
the build. Pages missing from navigation should also be treated as publication
defects during review.

## Publish the official website with GitHub Pages

The `.github/workflows/docs.yml` workflow builds the Raven language website for pull
requests and pushes to `main`. A successful push to `main` uploads `_site/` as a
GitHub Pages artifact and deploys it to:

<https://marinasundstrom.github.io/raven/>

The repository's Pages source must be set to **GitHub Actions** in the GitHub
Pages settings. The workflow keeps build and deployment as separate jobs, so a
failed DocFX validation cannot replace the published site.

The same Pages artifact includes the browser playground at:

<https://marinasundstrom.github.io/raven/playground/>

`scripts/build-playground-site.sh` builds the relocatable Blazor WebAssembly
application and places it under `_site/playground/` after DocFX has built the
main site. The playground and documentation therefore deploy atomically.
