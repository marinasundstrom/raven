# Built-in analyzers

Raven ships analyzers with the compiler so projects can adopt consistent
guidance without installing a separate package. Analyzer diagnostics do not
change whether a program is valid Raven. They form a policy layer over the
language: keep the defaults, promote selected rules to errors, lower their
severity, or disable them to suit the project.

This differs from a compiler error. A compiler error means the program has no
valid meaning or cannot be emitted. An analyzer warning means Raven understands
the program but has identified a convention, likely mistake, or maintainability
concern.

## Configure a rule

Set severity by diagnostic ID in the nearest `.editorconfig`:

```ini
[*.rvn]
# Require explicit handling of a discarded tail value.
dotnet_diagnostic.RAV9034.severity = error

# Do not enforce the preference for Result over throw.
dotnet_diagnostic.RAV9013.severity = none
```

Use another section for legacy `.rav` files when a project contains them:

```ini
[*.rav]
dotnet_diagnostic.RAV9034.severity = none
```

Common values are `error`, `warning`, `info`, `hidden`, and `none`. `default`
restores the level in the table below. See
[Analyzer configuration](configuration.md) for project-wide analyzer
participation, source suppression, and the opt-in returned-value analyzer.

## Analyzer reference

“Default” means the descriptor severity before any `.editorconfig`, project, or
command-line override. All listed analyzers participate by default. The full
returned-value mode extends `RAV9034` to bare calls and member accesses.

| ID | Default | Rule |
| --- | --- | --- |
| `RAV1051` | Warning | Prefer a newline between declarations. |
| `RAV9001` | Info | Add an inferred return type annotation. |
| `RAV9003` | Warning | Make an event delegate nullable when the event can be empty. |
| `RAV9004` | Warning | Use `let` when a local declared with `var` is never reassigned. |
| `RAV9006` | Warning | Initialize a property in storage or a constructor. |
| `RAV9012` | Info | Prefer `Option<T>` or `Result<T, E>` over nullable domain flow. |
| `RAV9013` | Warning | Prefer `Result<T, E>` over `throw` for expected failure. |
| `RAV9014` | Warning | Prefer Raven's `Option`/`Result` LINQ alternatives where applicable. |
| `RAV9015` | Warning | Use `is null` or `is not null` when a strict null check and flow narrowing are intended. |
| `RAV9016` | Info | Make an unexposed member private. |
| `RAV9017` | Info | Make a method static when it does not use instance data. |
| `RAV9018` | Warning | Remove or use a property that is never referenced. |
| `RAV9019` | Warning | Remove or invoke a method that is never referenced. |
| `RAV9023` | Warning | Follow Raven's constructor-parameter naming convention. |
| `RAV9026` | Warning | Use the new value returned by an immutable collection operation. |
| `RAV9027` | Warning | Remove or use an unused local value. |
| `RAV9028` | Warning | Remove an unnecessary trailing separator. |
| `RAV9030` | Warning | Remove or use an unused parameter. |
| `RAV9031` | Hidden | Remove an unused import directive. |
| `RAV9032` | Warning | Initialize a field in storage or a constructor. |
| `RAV9033` | Warning | Dispose a disposable value before leaving its scope. |
| `RAV9034` | Warning | Make an unused expression result explicit. This includes value-forming expressions and a non-`unit` tail value in a `unit` callable; full mode also checks bare calls and member accesses. |

## Choosing a policy

Raven's defaults are recommendations, not a single mandatory programming
style. For example, all of the following are legitimate project choices:

- keep `RAV9034` as a warning to make ambiguous tail discards visible;
- promote it to an error in a codebase that requires explicit value flow;
- disable it in a codebase that primarily uses explicit returns or intentionally
  permits discarded expression results;
- enable full returned-value handling only where every returned member value
  must be handled.

Prefer committing `.editorconfig` with the project so command-line builds and
the Raven language server present the same policy to every contributor.
