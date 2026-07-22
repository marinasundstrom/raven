# Documentation

Raven keeps user documentation and development records in the same repository,
but only an explicit user-facing subset is published by DocFX. The publication
list is defined in [`docfx.json`](docfx.json); adding a Markdown file under
`docs/` does not publish it automatically.

## User-facing documentation

Public documentation should help someone learn or use Raven. It should explain
what a feature does, when to use it, and show representative Raven examples.

### Learn Raven

* [Introduction](introduction.md)
* [Raven for absolute beginners](raven-for-absolute-beginners.md)
* [Raven for C# developers](raven-for-csharp-developers.md)
* [Language docs](lang/README.md)
* [Domain modeling](lang/domain-modeling.md)

### Language reference

* [Language specification](lang/spec/language-specification.md)
* [Type system](lang/spec/type-system.md)
* [Grammar](lang/spec/grammar.ebnf)

### Tools

* [Compiler and command line](compiler/raven-compiler.md)
* [Project system](compiler/project-system.md)
* [VS Code extension](compiler/raven-vscode-extension.md)

### Compiler API

The compiler API is published for analyzer, generator, refactoring, and tooling
authors, but it is kept separate from the language-feature documentation.

* [Compiler API overview](compiler/api/README.md)
* [Syntax tree API](compiler/api/syntax-tree.md)
* [Semantic analysis API](compiler/api/semantic-analysis.md)
* [Generated .NET API reference](api/)

## Development documentation

Compiler architecture, implementation designs, investigations, test guidance,
and language proposals are retained for contributors but are not part of the
published user manual or the compiler API section. If compiler-development
documentation is published later, it should have its own top-level section.
These documents live primarily under:

* `docs/compiler/architecture/`
* `docs/compiler/design/`
* `docs/compiler/development/`
* `docs/design/`
* `docs/investigations/`
* `docs/lang/proposals/`
* `docs/testing/`

Do not link to these areas from published pages. When a proposal becomes part
of the language, move its user-relevant behavior into the specification and
learning material rather than publishing the proposal as the feature guide.
