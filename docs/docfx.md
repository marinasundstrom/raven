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
