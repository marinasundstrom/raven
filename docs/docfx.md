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
highlighting and automatic light/dark theme support. Its color, typography, and
surface tokens live in `docs/template/public/raven-theme.css`. The standalone
Playground owns an aligned copy in `src/Raven.Playground/wwwroot/css/` so local
development and independent deployments do not depend on the documentation
tree.

Raven's purple accent, typography, cards, borders, and page surfaces define the
site shell. Code is a distinct shared layer modeled on Visual Studio Code's
Light+ and Dark+ editor themes. The shared theme defines the code background,
foreground, and syntax-token colors; the DocFX highlighter and RavenDoc consume
those variables, while the Playground's Monaco themes use the corresponding
editor and token palette. Changes to code colors should be applied consistently
to all three surfaces.

The home page presents an application-workload carousel and one ordered
learning path. Keep the first decision obvious: learn the language or try it
online. Every carousel item must represent a real, checked-in workload and
include a **Learn more** link to its workload guide. Preserve normal source
formatting in every sample: indentation communicates nesting, and blank lines
should separate distinct declarations, constructs, and top-level operations.

Keep the global header deliberately compact. Detailed documentation hierarchy
belongs in the Docs menu and section sidebars, while the root page serves as a
dedicated introduction to Raven.

## Guides and specification

User-facing guides and the language specification serve different reading
goals and must remain visibly separate in navigation and writing style.

Guides teach a concept or help complete a task. They should lead with
motivation and recognizable application code, explain the useful default, call
out important tradeoffs, and point to a next step. A feature guide should not
attempt to enumerate every grammar production, conversion rule, diagnostic, or
edge case.

The specification is the precise reference for syntax and semantics. It may be
exhaustive and organized around language constructs rather than a learning
journey. Guides should link to the relevant specification section when exact
rules matter instead of reproducing normative material.

Workload guides live under `docs/workloads/`. Each homepage application sample
must link to the guide for that workload, not only to its source directory. A
workload guide should show the project layout, complete key files, build and run
commands, ordinary .NET integration, Raven-specific choices, and how the code
can be split as the application grows.

## Publication boundary

`docs/docfx.json` explicitly lists public learning material, language reference,
the library entry points, the tooling pages needed to use Raven, and the
supporting compiler API section. Compiler architecture and implementation
details, contributor instructions, testing notes, investigations, language
proposals, historical material, and standalone design work remain available in
source control but are intentionally excluded from the user-facing site.

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

The artifact also includes the zero-install experimental component-template
showcase at:

<https://marinasundstrom.github.io/raven/experiments/html-macro/>

`scripts/build-html-macro-site.sh` publishes its standalone Blazor WebAssembly
host under `_site/experiments/html-macro/`. It consumes the checked-in sample
without embedding the experimental macro implementation into the Playground.

The artifact also bundles two independent RavenDoc sites:

- `Raven.Core` at `/raven/libraries/raven-core/`
- `Raven.Macros` at `/raven/libraries/raven-macros/`

RavenDoc retains its own static-site structure and rendering pipeline. The
DocFX navigation links to these library references, and the RavenDoc headers
link back to the language documentation and related references. The shared
Raven theme keeps the sites visually related without coupling their page
models.
