# Standard JSON and XML macro plan

Status: implemented (initial library slice)

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
    "name": "$name",
    "active": true,
    "tags": ["raven", $category],
    "next": ${sequence + 1}
}

let element = xml! {
    <user id="$id">
        <name>$name</name>
        ${CreateStatusElement()}
    </user>
}
```

The data macros use Raven's established Kotlin-like interpolation forms:
`$identifier` for one identifier and `${expression}` for a larger expression.
Inside a quoted data value these forms produce text interpolation. In JSON
value position they serialize the Raven value as JSON; in XML content they
pass the value to LINQ to XML, which applies its normal value, escaping, and
node semantics. `quote!` retains its separate `#(expression)` syntax because
it quotes Raven syntax rather than embedding a data language.

The outer `json!` braces represent the JSON object's braces to consumers. In
the Raven syntax tree they remain the ordinary macro token-body delimiter. The
initial contract therefore returns an object and does not require a redundant
inner `{ ... }`. `xml!` uses its braces only as the carrier and accepts one XML
root element inside them.

## JSON MVP

The JSON parser accepts:

- objects with quoted property names;
- arrays;
- string, number, Boolean, and null literals;
- Raven expression splices in value position.

It expands to ordinary `System.Text.Json` calls and returns a strongly typed
`ExpressionSyntax<JsonObject>` contract. Structural splices are serialized by
`JsonSerializer` and the resulting text is parsed as `JsonObject`; normal Raven
binding checks every splice before emission. This accepts the same values that
the platform serializer accepts without introducing a Raven-specific JSON
conversion layer.

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

It expands to `XElement`, `XAttribute`, and `XName` construction from
`System.Xml.Linq` and returns a strongly typed `ExpressionSyntax<XElement>`
contract. LINQ to XML owns escaping and the interpretation of inserted values
or nodes. Namespace declarations and qualified names are deferred to a
follow-up slice so the first version does not invent a second name-resolution
system.

The MVP excludes DTDs, external entities, processing instructions, XPath,
schema validation, and document parsing. It constructs values and therefore
does not need to enable external resource resolution.

## Shared macro implementation model

Both macros should use the same small internal architecture:

1. A body-relative cursor owned by each Raven-authored macro parser.
2. Accumulated body-relative diagnostics with stable codes (`JSON001...` and
   `XML001...`).
3. Final validation by `JsonDocument` or `XDocument`, so accepted completed
   literals follow the platform formats rather than a permissive approximation.
4. `MacroFragmentRegion` entries for every Raven splice.
5. Position-preserving JSON/XML projections for embedded-language editor
   services, with Raven fragments masked from those projections.
6. Expansion to ordinary Raven `ExpressionSyntax`, with source mappings for
   generated expressions that originate from authored splices.

Any private parser representation is an implementation detail, not a new
compiler syntax-node API. This follows the same principle as `query!`: the compiler owns
tokens, spans, Raven fragments, diagnostics, and semantic projection; the macro
owns the structure of its DSL.

## Developer-experience requirements

- Hovering the macro name describes the macro and points to the expansion
  command; hovering inside the body does not repeat the invocation hover.
- Hover and completion inside `$identifier` and `${expression}` use normal
  Raven semantic information.
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

## Recorded decisions

1. Both macros use `$identifier` and `${expression}`.
2. JSON value splices use `JsonSerializer`; interpolation inside a JSON string
   remains string interpolation.
3. XML namespaces are the first follow-up rather than MVP behavior.
4. The concrete contracts are `JsonObject` and `XElement`.
5. The macros ship in `Raven.Macros` as an initial standard-library surface;
   their deliberately small DSL grammars may evolve before Raven 1.0.

No new Raven keyword, compiler syntax node, bound node, or runtime abstraction
is justified by this plan.
