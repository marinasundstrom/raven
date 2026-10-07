# Building the Raven website

Run `scripts/build-docs.sh` to build the compiler/libraries and publish the complete
RavenDoc website into `_site`. Use `--no-build` with current build outputs and
`--serve` to preview at http://localhost:8080.

`docs/ravendoc.json` selects public pages explicitly. Section `toc.yml` files
supply navigation and can introduce pages; development records are not globbed
into the site. Existing article paths and library API paths are preserved.
The header exposes both Raven.Core and Raven.Macros API references.

`docs/assets` owns the shared theme/highlighting and website-specific landing
styles, tabs, reference finder and playground links. RavenDoc supplies the page
shell, accessible navigation, and light/dark theme controls.

The website workflow adds the WebAssembly playground and component showcase,
then build provenance, navigation/browser validation and production analytics.
Publication remains a separate manual GitHub Pages workflow; a local build does
not deploy. `scripts/build-playground-site.sh` and
`scripts/build-html-macro-site.sh` build the two application surfaces.
