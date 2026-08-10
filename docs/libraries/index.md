# Raven libraries

Raven ships a runtime library and a compile-time macro library. Their API
references are generated directly from Markdown documentation comments by
RavenDoc and are published as independent static sites alongside this language
documentation.

## Raven.Core

`Raven.Core` provides the standard runtime vocabulary used by ordinary Raven
programs, including `Option`, `Result`, error helpers, parsing projections,
sequence extensions, unions, and JSON integration.

- [Learn how Raven.Core fits into the language](../compiler/raven-core-library.md)
- [Browse the Raven.Core API reference](https://marinasundstrom.github.io/raven/libraries/raven-core/)

## Raven.Macros

`Raven.Macros` provides the standard compile-time macros, including `quote!`,
`compile!`, `query!`, and the `Error` macros. Import `Raven.Macros.*` to bring
their short aliases into scope.

- [Learn how Raven.Macros is referenced and authored](../compiler/raven-macros-library.md)
- [Browse the Raven.Macros API reference](https://marinasundstrom.github.io/raven/libraries/raven-macros/)
- [Understand Raven syntax trees](../compiler/api/syntax-tree.md)

The library references intentionally remain separate RavenDoc sites. This is
the first integration point for RavenDoc; its publishing and navigation model
can evolve independently as Raven's documentation system grows.
