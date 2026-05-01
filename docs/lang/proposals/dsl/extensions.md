## Proposal: Raven DSL Extensions

> Status: **Exploratory**.
>
> The implemented baseline is trailing closure invocation plus explicit
> `[Builder<T>]` result-builder binding. This document sketches possible
> extensions that keep the feature Raven-shaped rather than copying Swift,
> Kotlin, or F# directly.

## Goals

* Keep `expr { ... }` as ordinary invocation syntax first.
* Keep DSL participation explicit and type-directed.
* Support UI builders, query builders, validation builders, and future
  computation-expression-style workflows on the same block foundation.
* Avoid framework-specific compiler knowledge.
* Prefer regular Raven declarations, attributes, and overload resolution over
  special parser cases.

## Non-goals

* Do not reintroduce brace trailers as object initializers.
* Do not make every block a DSL candidate.
* Do not add framework-specific syntax for WPF, Xamarin.Forms, MAUI, Avalonia,
  or other UI stacks.
* Do not require F# computation-expression syntax wholesale.

## Baseline

The closed builder-block feature provides:

```raven
Window {
    StackPanel {
        Button(text: "OK") {
            Submit()
        }
    }
}
```

The outer blocks bind to builder-annotated closure parameters. The button block
can bind to an ordinary final `() -> ()` parameter when the API models a primary
action that way.

Object initialization remains separate:

```raven
val person = Person with {
    Name = "Anna"
}
```

## Raven.Core DSL primitives

Raven.Core should eventually provide the canonical DSL support surface:

```raven
class BuilderAttribute<T> : Attribute {
}
```

Possible shared runtime types can be added incrementally:

* `BuilderAttribute<T>`: canonical result-builder opt-in.
* `BuilderContext`: optional ambient context passed by adapters.
* `Fragment<T>` or `NodeList<T>`: reusable list-like component carrier for tree
  DSLs.
* `Key`: standard identity metadata for reconcilers.
* Adapter abstractions for descriptor discovery and child-slot assignment.

The compiler should continue to key off the explicit attribute shape, not a
hard-coded UI runtime.

## Extension area 1: richer builder protocols

The current builder protocol is expression-composition oriented:

* `BuildExpression`
* `BuildBlock`
* `BuildOptional`
* `BuildEither`
* `BuildArray`
* `BuildFinalResult`

Future builders can add computation-expression-style methods for effectful
workflows:

* `Bind`
* `Return`
* `ReturnFrom`
* `Zero`
* `Combine`
* `Delay`
* `While`
* `For`
* `Using`
* `TryWith`
* `TryFinally`

This enables Raven-native workflows without making UI builders pay for
effect sequencing.

Example shape:

```raven
Task {
    val user = do LoadUser(id)

    if user.IsActive {
        return user.Name
    }

    return "inactive"
}
```

The exact binding form for effectful `do`/`bind` operations is intentionally
open. Raven should prefer a spelling that fits existing statement syntax and
diagnostics instead of importing F# `let!`/`do!` unchanged.

## Extension area 2: explicit workflow builders

Result builders and workflow builders should remain distinct capabilities.

One possible shape:

```raven
func LoadName([Workflow<TaskBuilder>] body: () -> string) -> Task<string> {
    return TaskBuilder.Run(body)
}
```

`[Builder<T>]` would continue to mean "collect components into a value".
`[Workflow<T>]` could mean "sequence effects and produce a result".

This keeps overload resolution predictable:

* a non-annotated trailing block is an ordinary closure,
* `[Builder<T>]` enables component collection,
* `[Workflow<T>]` enables effect sequencing.

## Extension area 3: adapter descriptors

UI and object-graph DSLs should be implemented by adapters, not compiler rules.

An adapter descriptor can define:

* factory selection,
* property and event assignment,
* attached-property handling,
* child slot rules,
* metadata such as `Key`,
* conversion rules,
* reconciliation identity.

Example:

```raven
AppRoot {
    Grid {
        Label(text: "Name", row: 0, column: 0)
        TextBox(text: model.Name, row: 0, column: 1)
    }
}
```

The compiler should see ordinary calls and builder components. The adapter
decides whether `row:` maps to WPF `Grid.Row`, Avalonia grid metadata, or a
custom runtime descriptor.

## Extension area 4: handler conventions

Swift and Kotlin often use a trailing closure as the primary action:

```raven
Button(text: "Save") {
    Save()
}
```

Raven can support this without new syntax because the trailing block is already
an ordinary final closure argument unless the selected parameter is
builder-annotated.

Adapters can layer framework-specific conventions on top:

* map a final `onClick: () -> ()` constructor parameter to a button click event,
* map `Clicked: () -> ()` named arguments to events,
* preserve explicit event names when ambiguity matters.

## Extension area 5: named trailing blocks

V1 supports one unlabeled trailing block. A future extension could support
multiple named trailing blocks only if the syntax remains easy to parse and
clearly maps to named closure parameters.

Possible shape:

```raven
Dialog(title: "Delete file") {
    Text("This cannot be undone.")
} actions: {
    Button(text: "Cancel") { Close() }
    Button(text: "Delete") { Delete() }
}
```

This should remain future work until the single trailing-block model has enough
real-world pressure.

## Extension area 6: diagnostics and language service support

Flexible DSLs need targeted diagnostics:

* missing builder method for a used construct,
* ambiguous handler/content trailing block,
* adapter cannot find a property/event/child slot,
* unsupported control flow in a workflow builder,
* component type cannot be adapted through `BuildExpression`.

Language service support should expose the selected mode at the block:

* ordinary closure,
* result builder,
* workflow builder,
* adapter-recognized node construction.

## Open questions

* Should workflow builders use a separate `[Workflow<T>]` attribute or extend
  `[Builder<T>]` with additional methods?
* What Raven syntax should represent effectful binding without importing F#
  `let!` directly?
* Should `Key:` remain adapter metadata or become a first-class language-level
  convention?
* How much adapter descriptor shape belongs in Raven.Core versus framework
  packages?
* Can named trailing blocks be added without making invocation parsing feel
  non-Raven?
