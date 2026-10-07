# **RavenDoc** — Documentation Generator

RavenDoc is Raven’s built-in documentation generator. It produces a static
HTML sites combining authored Markdown/HTML pages with generated API reference.
API documentation comes from Raven source comments, assembly-adjacent `.docs`
sidecars, or XML documentation for .NET libraries. One publisher provides site
navigation, symbol pages, syntax highlighting, themes and project branding.

The core idea is simple:
**author content in Markdown or HTML, and derive API structure from symbols**.

---

## For whom is RavenDoc intended?

RavenDoc is intended for developers who:

* want documentation colocated with source code
* prefer Markdown over external documentation systems
* don’t need a separate authoring pipeline
* want documentation generated as part of compilation or tooling

Use source comments for API contracts and separate pages for guides, feature
overviews and landing content. Both publish in the same site.

---

## Documentation comments in Raven

Raven supports two kinds of documentation content:

### XML documentation

* Structural and machine-readable
* Intended for external tools (e.g. DocFX-style pipelines)
* Focuses on data extraction rather than presentation

### Markdown documentation

* Presentation-focused
* What you write is what gets rendered
* Supports headings, tables, lists, code blocks, etc.

**RavenDoc prefers Markdown documentation.** XML documentation remains a
compatibility input for .NET libraries that do not publish Raven Markdown.

---

## Documentation comment syntax

Documentation comments are attached to declaration syntax nodes as **leading trivia**.

```raven
/// ## Hello
///
/// **Test**
public func Foo() { }
```

Notes:

* Each line is prefixed with `///`
* The prefix is stripped before Markdown processing
* Blank documentation lines must still use `///`

### Markdown doc tags

Raven Markdown documentation also supports a lightweight block-tag layer for
structured member data.

Example:

```raven
/// Parses a widget title.
///
/// @param text Input text to parse.
/// @returns The parsed title.
/// @remarks This is culture-invariant.
func ParseTitle(text: string) -> string
```

Supported tags currently include:

* `@param name`
* `@typeparam name`
* `@returns`
* `@value`
* `@remarks`
* `@example`
* `@exception TypeName`
* `@see SymbolOrLink`
* `@seealso SymbolOrLink`
* `@inheritdoc`

These tags are parsed structurally by the compiler while still preserving the
original Markdown content as authored.

---

## Accessing documentation from symbols

Documentation comments can be retrieved from symbols, both for:

* source-defined symbols
* metadata symbols (when available)

```raven
let comment = symbol.GetDocumentationComment()

let content = comment?.Content // Markdown, without "///"
let rawText = comment?.RawText // Original text, with "///"
```

RavenDoc uses the processed Markdown content (`Content`) for rendering.

RavenDoc adapts compiler and CLR symbol data to Raven language conventions.
Metadata ownership is not automatically presentation ownership: emitted
containers, marker types, and helper members remain implementation details,
while Raven declarations are projected using Raven names, signatures, and
grouping. For example, namespace functions appear under their namespace and
named cases of nominal unions appear on their declaring union rather than as
separate emitted case types.

## Generating a site

RavenDoc accepts a Raven project, one Raven source file, a directory containing
Raven source files, or a compiled library:

```bash
dotnet run --project src/RavenDoc -- \
  samples/projects/markdown-docs/library/MarkdownDocs.Library.rvnproj \
  --output artifacts/markdown-docs-site
```

To publish from a library, keep its `.docs` sidecar adjacent to the assembly:

```bash
dotnet run --project src/RavenDoc -- \
  artifacts/library/MarkdownDocs.Library.dll \
  --output artifacts/markdown-docs-library-site
```

Prefer project or source input when publishing APIs from the current
repository. Source symbols retain their file paths and line spans, allowing
RavenDoc to link declarations to the corresponding GitHub source lines.
Assembly input remains useful for external libraries, but metadata symbols do
not imply a local source mapping.

Use `--framework <tfm>` when the input targets something other than `net10.0`.
When `--output` is omitted, RavenDoc writes `_site` next to the input.
Source-directory input can add assembly dependencies with repeatable
`--reference <assembly>` options. Repeatable `--nav <label=url>` options add
links to related documentation sites in the generated header.

### Building a complete documentation site

Use a JSON site configuration to combine authored Markdown pages, a menu you
control, and a generated API-reference section:

```bash
dotnet run --project src/RavenDoc -f net10.0 -- --site path/to/ravendoc.json
```

```json
{
  "name": "My project",
  "output": "_site",
  "api": "../src/MyProject.rvnproj",
  "framework": "net10.0",
  "pages": [
    { "source": "index.md", "title": "Overview" },
    { "source": "getting-started.md", "output": "guides/start.html", "title": "Getting started" }
  ],
  "toc": "toc.yml",
  "links": [{ "label": "Project home", "url": "https://example.com/" }],
  "resources": ["images", "custom.css"],
  "logo": "images/mark.svg",
  "favicon": "images/mark.svg",
  "notice": "Development docs · May include unreleased APIs.",
  "stylesheet": "custom.css",
  "footer": "My project documentation",
  "values": { "version": "1.0" }
}
```

Input paths and `output` are relative to the configuration file. Page output
paths, menu URLs, `logo`, `favicon`, and `stylesheet` are relative to the generated site
root. A page without an explicit output retains its source path with an `.html`
extension. Include a page at `index.html`; Markdown or HTML supplies the home page.
Page `title` supplies the browser title, while Markdown supplies visible headings.
Relative Markdown links between listed pages, including fragments, are rewritten
to their output locations. Links and images targeting copied resources are also
adjusted when a page moves. Root-relative and external URLs are retained.

The `toc.yml` menu preserves the configured order and supports nested groups.
An entry with `href: api/` inserts the generated namespace/type tree at that
position; without one, the API section is appended. API generation uses the same Raven
symbol projection as standalone RavenDoc: union cases belong to their union,
not the navigation's type list. Authored pages can link to API symbols with
`xref:T:Namespace.Type` or the other documented ID prefixes below. Unresolved
article xrefs fail the build rather than publishing broken links.

`api` accepts the same project, source, directory, or assembly inputs as the
standalone command and publishes below `api/`. Omit it for an authored-only
site. Use `references` for additional source-directory assembly references.
`resources` lists files or directories to copy with their relative structure.
The optional stylesheet loads after Raven's styles, allowing projects to
override theme tokens while retaining Raven's layout. `links` adds header links;
use the grouped menu for the full documentation structure.

The builder renders into a temporary directory before replacing the configured
output. Input files, duplicate page destinations, reserved API paths, and
escaping output paths are checked before publication. Use a dedicated output
directory: a successful build replaces its contents. This is a local build;
it does not deploy the website.

A runnable example lives in
[`samples/projects/markdown-docs/site`](../samples/projects/markdown-docs/site/ravendoc.json).
The site configuration is RavenDoc's own JSON format. Menus can reuse DocFX
`toc.yml` files as described below; DocFX build JSON, conceptual-page front
matter, and UID shorthand are not imported. Migrating
an existing site requires listing its pages/resources and converting its symbol
links to RavenDoc IDs. There is currently one generated API input per site.

### Branding and colors

`name` sets the site name in the header and browser titles; a page's `title`
sets its individual browser title. `logo` replaces the Raven mark, `favicon` sets
the browser-tab icon, and `footer`
sets the footer text. Include local logos and stylesheets in `resources` so
they are copied to the output. These settings apply to authored and API pages.

A `stylesheet` is loaded after Raven's styles. For example, `custom.css` can
change the accent colors while keeping Raven's layout:

```css
:root {
    --raven-accent: #176d83;
    --raven-accent-strong: #115367;
    --raven-accent-soft: #e0f2f5;
}

:root[data-theme="dark"] {
    --raven-accent: #7fd0de;
    --raven-accent-strong: #a6e2eb;
    --raven-accent-soft: #173b44;
}

@media (prefers-color-scheme: dark) {
    :root:not([data-theme="light"]):not([data-bs-theme="light"]) {
        --raven-accent: #7fd0de;
        --raven-accent-strong: #a6e2eb;
        --raven-accent-soft: #173b44;
    }
}
```

Other shared tokens include `--raven-bg`, `--raven-surface`, `--raven-ink`,
`--raven-muted`, `--raven-line`, `--raven-header-bg`, `--raven-font-sans`, and
`--raven-radius`. Use the matching dark-mode selector when overriding dark
colors, because the shared theme uses that selector too. The sample site
includes a custom logo and this color scheme.

### Typography

RavenDoc defaults to compact technical documentation: `0.9375rem` prose and
member labels (15px with the usual browser default), `0.8125rem` code blocks and
API signatures (13px), and a 1.6 line height. Inline code is 90% of its surrounding
text. Relative units preserve browser font preferences and zoom. Highlighted code
uses the same size as plain code. Article headings range up to `2rem`.

Override these defaults in the site `stylesheet`, without modifying the generator:

```css
:root {
    --doc-text-size: 1rem;
    --doc-code-size: 0.875rem;
}
```

The sample site retains the shared defaults. Project-specific hero typography
can be styled separately from documentation pages.

### Section navigation with toc.yml

Set `"toc": "toc.yml"` in the site configuration. The small nested authoring
format is familiar to DocFX users but is compiled into RavenDoc's own model:

```yaml
- name: Overview
  href: index.md
- name: Guides
  items:
    - name: Getting started
      href: getting-started.md
- name: API reference
  href: api/
```

RavenDoc supports `name`, `href`, nested `items`, and their order as a small authoring format; it does not aim to clone DocFX configuration or templates. Markdown
pages referenced by the TOC are included automatically; `pages` remains useful
for additional unlisted pages or explicit output/title overrides. All relative
TOC links resolve from the directory containing that TOC. Markdown links are
mapped to the page's final HTML URL. External and root-relative links remain
unchanged.

`href: api/`, `api/index.html`, or `api/toc.yml` (relative to the configuration
root) inserts the generated API-reference tree. Nested `toc.yml` files and
folders containing a `toc.yml` are supported, along with explicit `tocHref`
and `topicHref`. Circular includes fail with a diagnostic. A folder TOC is a
menu group unless a `topicHref` supplies its landing page. Entries require a
`name`; UID-only navigation and DocFX template-specific metadata are not
interpreted. Markdown uses `xref:` links; explicit ID prefixes are recommended when a short name would be ambiguous.

The checked-in example uses this TOC format, so projects can retain their menu
structure when moving from DocFX to RavenDoc.

### Injecting build values

RavenDoc can replace explicit placeholders in Markdown with values supplied by
the build or publishing workflow. Pass `--value name=value` once for each
value:

```bash
dotnet run --project src/RavenDoc -- \
  src/Raven.Core/Raven.Core.rvnproj \
  --output artifacts/raven-core-api \
  --value version=1.4.0 \
  --value apiRoot=../api/
```

Use the values in documentation Markdown with `{{name}}`:

```raven
/// Available since Raven {{version}}.
///
/// See the [complete API reference]({{apiRoot}}).
public func Parse(text: string) -> SyntaxTree
```

Values are substituted as plain Markdown before HTML rendering. This supports
paths, package or compiler versions, commit identifiers, and version stamps
without making RavenDoc responsible for discovering that build metadata.
Whitespace inside a placeholder is optional. Value names may contain letters,
digits, `_`, `-`, and `.`, and must begin with a letter or `_`. Repeating a
name uses its last supplied value. Placeholders without a supplied value remain
visible in the generated documentation.

Namespace functions are organized under their Raven namespace. Their pages
also identify the emitted CLR container so consumers using C#, reflection, or
another .NET language can locate the metadata member. Namespace-level
`macro` declarations are listed separately as macros and do not require a
fabricated containing type in the Raven-facing reference.

## Project ownership and external symbols

The generated API section belongs to the project or assembly being documented.
Only namespaces containing its documented declarations appear in navigation.
Referenced assemblies and compiler-only namespaces do not become local pages
merely because their symbols are available during compilation. A namespace
shared with a dependency still lists only this project's declarations.

External types remain visible in signatures, inheritance and interface lists.
RavenDoc does not fabricate local URLs for them. Authored documentation can use
ordinary external links; unresolved symbol xrefs follow the documented warning
or error behavior for API comments and authored pages, respectively.

Future cross-site navigation could consume published symbol-to-URL maps from
other RavenDoc sites or framework documentation. Such a resolver would use an
explicit external documentation source, preserving the distinction between the
current project's API and a dependency's API. This capability is not yet
implemented; namespaces alone are not sufficient to choose a documentation site.

## Relationship to metadata sidecars

Raven’s compiler and IDE load Markdown documentation for metadata references
from assembly-adjacent `.docs/` sidecars. The built-in compiler comment emitter
produces that structure for Markdown output. RavenDoc consumes the same content
either directly from Raven source symbols or indirectly from a compiled
library and its Markdown sidecar.

Important separation:

* Markdown sidecars are authored presentation content.
* XML sidecars are structured interoperability data.
* RavenDoc consumes Markdown plus symbol data; it does not redefine the storage
  format for either Markdown or XML documentation.

RavenDoc's two primary input paths are:

* source symbols with attached Markdown comments
* PE symbols with Markdown sidecars

Both paths normalize through the Raven documentation model before RavenDoc
assembles symbol pages and projects them to HTML. XML remains a compatibility
input for libraries that do not provide Raven Markdown, rather than the model
that shapes RavenDoc's APIs.

## Rendering direction

The current HTML renderer provides a Raven-specific, responsive API-reference
presentation with light and dark color schemes. Page titles use a compact
reference-heading scale, while editor-like Raven signatures carry the primary
visual weight. Since RavenDoc presents the exported Raven API, signatures omit
the redundant leading `public` modifier. Private and internal accessor details
remain implementation details; restricted accessors are shown only when they
contribute to the inheritable surface through protected accessibility.
Compiler-emitted extension grouping and marker types are also excluded from
the Raven-facing reference. Case-declared unions receive a dedicated Cases
section using their logical Raven case names and parameter signatures.
Parenthesized unions of member types remain a distinct shape and do not receive
a Cases section. Namespace and member kinds use distinct symbols, generic
constraints remain visible in signatures, and generated pages include a
responsive page outline. Fenced `raven`, `rvn`, and `rav` code blocks receive
Raven syntax highlighting from a generated local asset, so published sites do
not require a CDN.

The current rendering boundary is intentionally explicit:

* `DocumentationGenerator` extracts documentation, builds symbol navigation,
  resolves links, and projects compiler symbols into page models.
* `RavenDocContentTemplate` owns page-content composition such as metadata,
  relationships, documentation, member sections, and overload variants.
* `RavenDocSiteTemplate` owns the outer HTML shell and reusable hero,
  signature, and member-list components.
* `Assets/ravendoc.css` and `Assets/ravendoc.js` own presentation and
  progressive enhancement.

This separation keeps extraction, symbol routing, Markdown rendering, and page
chrome independent. A future templating engine can replace the template
projection without teaching templates how to parse source, sidecars, XML
documentation, or compiler symbols.

RavenDoc and the browser Playground consume the same foundational Raven theme:
color tokens, typography, surfaces, borders, radii, shadows, and the Raven
brand mark. Each tool composes those primitives for its own purpose rather than
sharing one rigid page layout. This keeps reference reading and interactive
coding distinct while making movement between them feel continuous. RavenDoc
and the Playground both offer a persistent System, Light, or Dark selector.
RavenDoc applies it to the whole site, including syntax highlighting.

Site configuration supports project branding and an additional stylesheet at
the page-chrome boundary. A future rendering layer can introduce user-selectable templates. Templates should receive the
Raven documentation model and resolved symbol navigation.

Interactive, executable examples belong to the future documentation-site
layer. That site can progressively enhance explicitly opted-in examples using
the same compiler-in-the-browser mechanism as the Raven playground. RavenDoc's
static output and ordinary fenced examples must continue to work without that
runtime or a network connection.

For the editor-facing documentation view that should share the same underlying
model without depending on published HTML, see
[Editor Documentation Experience](./compiler/design/editor-documentation-experience.md).

---

## Link conventions (important)

RavenDoc supports **symbol-aware links** using an `xref:` scheme, inspired by XML documentation IDs.

This allows documentation to link to:

* namespaces
* types
* members (methods, properties, fields)

### Basic form

```md
[Result](xref:T:System.Result`2)
```

At render time, RavenDoc resolves the `xref:` target and replaces it with a relative link to the generated page.

---

### Supported `xref:` prefixes

| Prefix | Meaning   | Example                               |
| ------ | --------- | ------------------------------------- |
| `N:`   | Namespace | `xref:N:System.Collections`           |
| `T:`   | Type      | `xref:T:System.Result\`2`             |
| `M:`   | Method    | `xref:M:System.Result\`2.UnwrapError` |
| `P:`   | Property  | `xref:P:System.Result\`2.Value`       |
| `F:`   | Field     | `xref:F:System.Result\`2.Error`       |

Notes:

* Generic arity is written using backticks (`` ` ``), e.g. ``Result`2``
* Overloads automatically resolve to the **member group page**
* If a link cannot be resolved, it is rendered without a target and marked as unresolved

---

### Why `xref:` exists

Normal Markdown links require knowing file paths.
`xref:` allows documentation authors to write **symbol-based links** without caring about layout, folders, or filenames.

This keeps documentation stable even if the generated structure changes.

---

## Recommended documentation structure (not enforced)

RavenDoc does **not** impose a schema. Sections are free-form Markdown.

That said, the following sections are recommended for consistency and readability:

### For types

```md
## Navigation levels and custom pages

RavenDoc keeps three independent navigation models:

- `links` defines the site-wide main menu. An item has `label`, an optional `url`,
  and optional nested `children`; children render in a keyboard-operable dropdown.
- `toc.yml` is the author-facing format for section side navigation. The loader
  compiles nested `items` into `DocumentationNavigationItem` values. The API
  generator produces the same internal model directly from documented symbols;
  authors do not maintain a duplicate namespace/type TOC. A logical `href: api/`
  entry places that generated tree in an authored menu.
- The page outline uses headings from the current page and is independent of
  both navigation menus.

Set `"namespaceNavigation": "flat"` in the site configuration to list full namespace
names as alphabetically sorted peers (for example `System`, `System.Networking`,
`System.Web`). Each namespace still expands to its types; nested types and union
cases are linked from their union pages. The default, `"hierarchical"`, nests child namespaces
beneath their parents. This changes only generated section navigation, not page
URLs, namespace overview contents, authored `toc.yml` groups, or page outlines.
Assembly/source CLI builds accept `--namespace-navigation flat` as well.
Namespaces without documented members of their own are omitted from the sidebar
by default. Their populated descendants remain visible, promoted to the nearest
visible ancestor in hierarchical mode. Set `"showEmptyNamespaces": true` (or use
`--show-empty-namespaces` for assembly/source CLI builds) to include these grouping
namespaces. Namespace overview pages and URLs are retained in either mode.
Type navigation and headings use declared names (`Object`,
`String`, `Char`); Raven aliases remain in code signatures.

A root `toc.yml` is discovered when neither `toc` nor `navigation` is configured.
For authored pages, the nearest `toc.yml` between the source directory and the
configuration directory defines their section menu, even when the output page is
relocated. Referenced Markdown or HTML pages are included in the build. Section
menus do not automatically append the API tree; use `href: api/` to include it.
The configuration's `navigation` property remains an alternative for defining the
root side menu directly; it is not the main menu.

The shared side-menu renderer labels its reference browser **API Browser**, supports
expand/collapse, filtering, current-location highlighting, and single-line labels
with full-name tooltips. At widths up to 760px it becomes an off-canvas modal drawer
opened with **Browse API**. The native dialog supplies focus containment and Escape
handling; a close button and backdrop dismissal are available. Without JavaScript,
side navigation remains visible. The drawer and desktop menu share the same model.

Markdown and HTML body fragments accept optional scalar front matter:

```yaml
---
title: Welcome
layout: landing
toc: false
---
```

`title` overrides the configured page title. `layout` is `docs` or `landing`;
landing pages omit the side menu. `toc` is a Boolean controlling the in-page
outline (distinct from the configuration's `toc` filename). Unknown, duplicate or
invalid keys fail the build. HTML inputs are fragments; the publisher owns the
HTML document shell. Full `html`, `head`, `body` and doctype wrappers are rejected.
Site-wide `showToc` defaults to true. `apiNavigationRoot`, when supplied, limits the
root side menu on authored pages to that output directory; generated API pages
always receive navigation. Explicit section TOCs are used within that scope.

Additional site configuration includes `subtitle`, `notice`, `releaseUrl`, and
`releaseLabel` for a project-owned availability notice. `apiPath` defaults to `api`.
Optional `types` selects documented type IDs (without `T:`), and `excludedMembers`
lists exact XML member IDs to omit. Selection affects generated navigation and
links. The site build exports `xref-map.json` for consumers needing compatibility
redirects or coverage checks. These controls do not infer a project's release status.

## Compact reference lists

API lists default to `memberListStyle: "compact"`: names first, properties and fields
as `Name: Type`, and methods as `Name(parameter: Type) -> ReturnType`. Overloads keep
separate entries. Generic parameters, nullable types, reference-passing annotations,
optional parameter `?` markers and variadic `...` markers are retained; unit returns
use `()`. Declaration keywords and accessibility modifiers are omitted. Static
members have an S badge on their icon, with a tooltip and screen-reader label.
Type icons use C for classes, I for interfaces, E for enums, U for unions, D for
delegates and S for structs. Union identity comes from `IUnionSymbol`, independently of carrier storage. Detail pages retain full Raven declarations.

Set `"memberListStyle": "signatures"` or use the assembly/source CLI's
`--list-signatures` switch to retain full declarations in browsing lists. This is
a presentation change and does not alter compiler semantics or emitted metadata.

## Raven syntax highlighting

RavenDoc embeds the Raven website's shared `raven-language.js` Highlight.js grammar
and `raven-highlight.css` token colors. Fenced `raven`, `rvn` and `rav` blocks,
HTML code blocks with these language classes, and generated API signatures use
that same renderer. The vendored Highlight.js 11.11.1 core and BSD license are
published locally; neither generation nor page viewing requires a CDN or Node.
Run `node scripts/test-raven-highlighting-sync.mjs` when changing the lexer.

### Favicon

Set `favicon` to a site-relative icon path (for example `favicon.svg`) and include
the file in `resources`. Authored and generated API pages resolve it relative to
the site root, including under a deployment subpath.

## Color themes

The header theme icon opens Light, Dark and Auto options. Auto follows the device setting,
including changes while the page is open. An explicit choice is saved locally
and applied before styles load on the next page. Storage-disabled browsers still
support switching for the current page. Shared Raven theme variables cover
content, navigation and syntax colors; project styles should override both light
and `:root[data-theme="dark"]` palettes without forcing OS dark mode over a
reader's explicit Light selection.

## Verify the complete sample

From the repository root:

```sh
dotnet run --project src/RavenDoc -f net11.0 -- --site samples/projects/markdown-docs/site/ravendoc.json
python3 -m http.server 8769 --directory artifacts/markdown-docs-site
```

The [sample walkthrough](../samples/projects/markdown-docs/README.md#complete-sample-site)
shows a landing page, Markdown guides, HTML page controls, nested menus, generated
APIs, favicon and theme switching. Open it over HTTP so browsers can load the
local JavaScript modules. No CDN or external runtime is needed to view the site.

For CI, run the same `--site` command from a built/pinned RavenDoc CLI, validate
the output and publish it with the project's deployment system. Generator version
selection and release-status text belong to that project's build configuration.
RavenDoc does not infer whether an API has shipped.

### Optional Google Analytics

Set `"googleAnalyticsId": "G-YOURID"` in the site configuration to include the
standard [Google tag](https://developers.google.com/tag-platform/gtagjs) once per
authored or generated API page. Omit the property to emit no analytics scripts.
RavenDoc validates the GA4 measurement ID and does not configure additional events
or consent controls. Configured analytics also runs when previewing that build;
omit the ID from a preview configuration if it should not be measured.

### Member structure and authored documentation

Member pages derive their basic structure from symbols, even without XML or Markdown:
Parameters list names and linked types; Property value, Return value, Field value
and Event type show the corresponding linked type. Constructors omit Return value.
The metadata card labels the owner as Declaring type. Overloads each retain their
own parameter/result contract. Nullable, array and generic arguments link to local
type pages where available; external or excluded types remain readable plain text.
Authored parameter/value/result descriptions enhance these sections rather than
controlling whether the structure exists. Remarks and examples follow them.

This follows Microsoft Learn's [property value](https://learn.microsoft.com/en-us/dotnet/api/system.string.length)
and [parameter/return](https://learn.microsoft.com/en-us/dotnet/api/system.string.substring)
sections, with explicit Return value wording. [Rust](https://doc.rust-lang.org/std/string/struct.String.html)
and [Kotlin](https://kotlinlang.org/api/core/kotlin-stdlib/kotlin.collections/-list/get.html)
also provide navigable types in signatures; RavenDoc retains its highlighted Raven
signature and places accessible type links in the structured contract below it.


## Semantic API pages and authored enrichment

RavenDoc presents Raven's interpretation of source and CLI metadata. Type names
remain nominal in headings and navigation, including generic unions and delegates.
A delegate's page also displays its declaration and invocation signature.
Namespace-level functions appear alongside types in the API Browser; ordinary
members remain on their type pages. Unions and enums are direct navigation links,
not expandable groups. Union cases have their own type pages and payload-member
pages, linked from the union's Cases section. Recognized generic-union companion
containers contribute to the logical union rather than becoming duplicate types;
the association comes from Raven's case metadata, not matching names.

Type pages include public inherited members and interface contracts in the same
member-kind lists as declared members. Inline labels link each implementing member to its interface declaration.
Interface member pages state whether implementation is required or a default is
provided, including mixed property accessors. Inline origins on consuming types distinguish declared implementations, inherited implementations, overrides
and interface defaults, with links to the declarations providing the behavior.
Incomparable default declarations are reported without guessing a winner. Links target documented declarations; external declarations without a local
page remain readable text. Constructors and accessor implementation methods are
not inherited entries. Static classes list only their own members. Static members appear only on their declaring type;
applicable extensions are still included independently. Closed hierarchies list permitted direct subtypes and
membership in closed base/interface hierarchies. These sections reflect semantic
symbols, independently of XML summaries. Use authored content for deeper design
narratives or diagrams.

Extension discovery is opt-in and uses Raven's semantic lookup, including generic
applicability and constraints. Configure exact namespaces with
`"extensionNamespaces": ["System.Linq", "System.Runtime.Reflection"]`.
Use a curated list of namespaces containing useful extensions; discovery does not
scan every namespace. Open generic receiver pages retain their type parameters
during lookup, so applicable extensions can be inferred. Ordinary operators on
non-extension types are not extension members.
Optionally restrict the results further using `extensionMembers`, an array of
exact member documentation IDs. Unselected namespaces do not contribute extension
sections. Displayed extensions identify and link their declaring container. Only extensions
from the documented assembly are included, preventing host-only framework
extensions from leaking into a different target's reference. Extension methods and
properties appear alongside ordinary members in their respective groups, with an
distinct E badge and a link to their declaring definition. The E badge replaces
the S badge on extension members. Compact labels on receiver pages omit generic
parameters already bound by receiver lookup: `Any()` rather than `Any<T>()`,
while a caller-selected result parameter remains, such as `Map<U>(...)`. The
extension container and declaration signature preserve the full generic contract.

Set `"apiContent": "api-content"` in the site configuration to merge a separate
Markdown tree into type and member pages. Directory structure and filenames are
for the author's organization; exact documentation IDs bind the files to symbols:

```markdown
---
uid: M:Samples.Docs.Widget.GetTitle
---
## Using the returned title

Additional guidance and [related APIs](xref:T:Samples.Docs.Widget).
```

Content is appended after generated documentation and contracts, before member
lists on type pages. It does not replace signatures or XML/Markdown comments.
Each overload uses its exact ID, including parameter types, even when several
members share one output page. Duplicate IDs, missing IDs and IDs that do not
match a rendered symbol fail the build, so stale content is not silently lost.
Use `xref:` links for API references. Ordinary relative links are relative to the
resulting API page, not the source Markdown directory. The site builder preserves
previous published output if validation fails.

The `samples/projects/markdown-docs/site` sample demonstrates separate authored
type and member content alongside generated documentation and syntax highlighting.


The **Group members** display control switches between **By member kind** and
**By declaring type**, retaining the full member set and its origin labels. The
page outline updates with the selection, which is remembered in the browser.
`"memberGrouping": "kind"` is the default; set it to `"declaringType"` for a
different initial preference. Without JavaScript, the member-kind lists remain
available. Existing bookmarks to member-kind headings open in that layout.
The **Show inherited members** checkbox is enabled by default, works in either
grouping mode, and remembers the reader's choice. Counts and the page outline
reflect visible members. Extensions participate in grouping but are independent
of the inherited-member filter.

The API sidebar measures its available viewport space below the site header and
release notice. The notice stays in normal document flow; it can scroll away
without requiring the article to scroll before the last navigation entry is
reachable. Wrapped headers and viewport resizing recalculate the available space.

### Types that derive from or implement a contract

Type pages list documented derived types. Interfaces separately list derived
interfaces and implementing types (including structs). These linked sections
include indirect relationships, marked as such, and match constructed generic
contracts to their original definitions. They describe the selected API surface
of the documented assembly, not every possible implementation in other assemblies.
Excluded/private types and compiler companion scaffolding remain omitted.

The **Show extension members** reader toggle appears on types with applicable
extensions. It defaults to enabled, remembers the reader’s choice, and works
independently of **Show inherited members** in either grouping mode. Hidden
extensions are excluded from group counts and empty groups disappear. This
filters extensions selected by the site, not all extensions in other assemblies.
An extension container always shows its own declarations, including their E badges;
the toggle only filters members contributed from another container.

Member display controls wrap with the available width. Changing a control updates
the current URL with `groupBy=kind|declaringType`, `inherited=true|false` and
`extensions=true|false`, preserving other query parameters and the fragment.
Valid URL choices override saved reader preferences; missing or invalid choices
fall back to those preferences and site defaults. Opening a shared URL does not
overwrite saved preferences. Changes replace the current history entry to avoid
adding a Back-button step for every toggle.

The Raven project website uses `docs/ravendoc.json`; see [Website build](website.md).
Set `script` to a site-relative JavaScript module for authored-page interactions.
Raw HTML links to selected Markdown articles are rewritten to their output URLs.

## Site-wide search

Set `"search": true` in the site configuration (default: `false`) to index authored
articles and generated API pages. A navbar search icon opens a labelled field and
keyboard-accessible results. A nonempty query keeps the panel open; Escape or the
icon closes it while retaining the query. All query terms must match, with title
matches ranked first. Search runs locally in the browser without a service.

For a combined output assembled in several steps, run
`RavenDoc --finalize-site path/to/ravendoc.json` after adding API sites. This
rebuilds the index and controls for all articles, including nested library sites.
Paths remain relative, including under a GitHub Pages prefix. Finalization is
idempotent. It indexes article text, not navigation or executable app content.

## Copying code samples

Set `"copyCode": true` (default: `false`) to add a Copy button to every article
code block, including generated signatures. It copies the displayed code with
its whitespace intact and announces success or clipboard failure. Clipboard
access requires HTTPS or localhost and browser permission. Copy controls are
outside the code text and do not change playground examples. Combined sites use
the same `--finalize-site` step as search.

Ordinary type pages list their public **Nested types** with links. Selecting an
outer type includes its public nested types and their members. Private nested
types remain excluded. Union case types stay in the union's **Cases** section,
never in Nested types. Namespace pages retain the **Types** heading.

Namespace pages render `///` comments from namespace declarations and `N:`
entries from Markdown/XML sidecars. Add authored namespace guidance with
`uid: N:Example.Settings` in an `apiContent` file; it is validated and merged
like type/member content. Source namespace comments from multiple declarations
merge in compilation source order. Implicit parent namespaces do not inherit them.
