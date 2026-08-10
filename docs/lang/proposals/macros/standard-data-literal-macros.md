# Standard JSON and XML macro plan

Status: planned, post-QueryMacro promotion

This proposal defines two small data-literal DSLs for `Raven.Macros`:

- `json!`, producing the standard `System.Text.Json.Nodes` object model;
- `xml!`, producing the standard `System.Xml.Linq` object model.

The relevant JSON namespace is `System.Text.Json.Nodes`. Unlike
`System.Xml.Linq`, `System.Text.Json` does not expose a `.Linq` namespace. The
two APIs nevertheless play the same role here: they are the platform's mutable
in-memory JSON and LINQ to XML models.

These macros are demonstrations of Raven's general macro infrastructure. They
must not add JSON or XML syntax to Raven's lexer, parser, syntax tree, or bound
tree, and they must not introduce a Raven-specific runtime document model.

## Goals

1. Construct normal .NET JSON and XML values with concise literal syntax.
2. Embed ordinary Raven expressions at explicit splice points.
3. Project those expression spans through the shared macro-fragment API so
   diagnostics, hover, completion, navigation, and source mapping behave like
   ordinary Raven code.
4. Produce ordinary Raven syntax that calls the platform APIs. After expansion,
   normal Raven binding and type checking remain authoritative.
5. Recover from incomplete input without throwing or leaving the compiler or
   language server in an inconsistent state.

They are not intended to replace serializers, schema systems, streaming JSON
readers/writers, XPath, XSLT, Razor, or the experimental Blazor component
template macro.

## Proposed authored forms

```raven
import Raven.Macros.*

let payload = json! {
    {
        "name": #(name),
        "active": true,
        "tags": ["raven", #(category)]
    }
}

let element = xml! {
    <user id=#(id)>
        <name>#(name)</name>
    </user>
}
```

`#(expression)` is the recommended splice syntax for the first implementation.
It is already used by `quote!`, clearly separates Raven from the surrounding
data language, and does not compete with JSON object braces or XML markup. The
syntax is still an explicit decision gate: before implementation, compare it
with the component-template `{ expression }` convention and choose one shared
rule for data DSLs. The two new macros should not invent different splice
conventions independently.

## JSON MVP

The JSON parser accepts:

- objects with quoted property names;
- arrays;
- string, number, Boolean, and null literals;
- Raven expression splices in value position.

It expands to construction of `JsonObject`, `JsonArray`, and `JsonValue`/
`JsonNode` values from `System.Text.Json.Nodes`. A splice is checked by normal
Raven binding after expansion. Values implicitly convertible to `JsonNode` are
accepted; arbitrary object serialization remains explicit through
`JsonSerializer.SerializeToNode` at the authored splice.

The MVP deliberately excludes computed property names, object/array spreads,
comments, conditionals, comprehensions, serializer options, schema validation,
and typed deserialization. Those features require demonstrated use cases rather
than speculative syntax.

## XML MVP

The XML parser accepts:

- one root element;
- nested elements;
- quoted attributes;
- text content;
- self-closing elements;
- Raven expression splices in attribute-value or content position.

It expands to `XElement`, `XAttribute`, `XText`, and `XName` construction from
`System.Xml.Linq`. Namespace declarations and qualified names should be part of
the MVP only if they can map directly to `XNamespace`/`XName` without adding a
second name-resolution system. Otherwise they are the first follow-up slice.

The MVP excludes DTDs, external entities, processing instructions, XPath,
schema validation, and document parsing. It constructs values and therefore
does not need to enable external resource resolution.

## Shared macro implementation model

Both macros should use the same small internal architecture:

1. Token-tree cursor with non-throwing `TryRead`/`Expect` helpers.
2. A private, immutable DSL representation owned by the macro implementation.
3. Accumulated body-relative diagnostics with stable codes (`JSON001...` and
   `XML001...`).
4. `MacroFragmentRegion` entries for every Raven splice.
5. `MacroTokenInfo`/classification for data-language punctuation, names,
   literals, and structural tokens.
6. Expansion to ordinary Raven `ExpressionSyntax`, with source mappings for
   generated expressions that originate from authored splices.

The private representation is an implementation detail, not a new compiler
syntax-node API. This follows the same principle as `query!`: the compiler owns
tokens, spans, Raven fragments, diagnostics, and semantic projection; the macro
owns the structure of its DSL.

## Developer-experience requirements

- Hovering the macro name describes the macro and points to the expansion
  command; hovering inside the body does not repeat the invocation hover.
- Hover and completion inside `#(...)` use normal Raven semantic information.
- JSON property names and XML element/attribute names receive DSL
  classification, not fake Raven symbols.
- Incomplete strings, missing separators, mismatched XML tags, unfinished
  splices, and partially typed closing delimiters produce diagnostics and
  recoverable token information rather than exceptions.
- Repeated queries against one document snapshot return stable diagnostics,
  fragments, tokens, and expansion results.
- F5/source mapping should step through embedded Raven expressions, not through
  the macro implementation or generated constructor boilerplate.

## Test and delivery slices

1. Build a shared non-throwing cursor/diagnostic helper only if implementing the
   second macro proves it removes real duplication; do not pre-design a general
   parser framework.
2. Implement `json!` with literal construction, splices, runtime tests, malformed
   input tests, and fragment/tooling tests.
3. Implement `xml!` using the same public macro primitives, with equivalent
   runtime and recovery coverage.
4. Add language-server integration tests that repeatedly edit malformed bodies
   and request diagnostics, hover, completion, semantic tokens, and expansion.
5. Add focused Playground examples after the macros work in the WebAssembly
   compiler/runtime environment.

## Playground showcase

The Playground already embeds `Raven.Macros` as both a metadata and macro
reference. A separate implementation slice should add a “Built-in macros”
example under the Metaprogramming category that demonstrates `query!`,
`#[Error]`/`#[ErrorMessage]`, and `quote!` without requiring a custom project.

Keep the first example executable in the existing console preview. It should
not require the future Blazor preview initialization model. When `json!` and
`xml!` are available and verified under WebAssembly, add them to this example
or split them into a concise “Data literals with macros” example. The browser
smoke test must compile and run every built-in macro used by the selected
example before the documentation site is published.

## Decision gates

Before implementing `json!` or `xml!`, decide and record:

1. the shared Raven splice syntax;
2. the exact JSON splice conversion rules;
3. whether XML namespaces are MVP or the first follow-up;
4. whether each macro returns the concrete root type (`JsonNode`/`XElement`) or
   relies solely on target typing;
5. whether the standard macros remain experimental or are included in the
   supported `Raven.Macros` surface.

No new Raven keyword, compiler syntax node, bound node, or runtime abstraction
is justified by this plan.
