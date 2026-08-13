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

For an opt-in rule, first add its analyzer type to `RavenEnabledAnalyzers` in
the project file. A severity entry alone does not activate an optional analyzer.

Use another section for legacy `.rav` files when a project contains them:

```ini
[*.rav]
dotnet_diagnostic.RAV9034.severity = none
```

Common values are `error`, `warning`, `info`, `hidden`, and `none`. `default`
restores the level in the table below. See
[Analyzer configuration](configuration.md) for project-wide analyzer
participation, source suppression, and the opt-in full returned-value mode.

## Analyzer reference

“Severity” means the descriptor severity before any `.editorconfig`, project,
or command-line override. Analyzers are grouped by kind, and each analyzer has
its own participation default. Raven enables correctness and safety checks by
default; nonessential rules are opt-in. The full returned-value mode extends
`RAV9034` to bare calls and member accesses.

| Kind | ID | Participation | Severity | Rule |
| --- | --- | --- | --- | --- |
| Style | `RAV1051` | Opt-in | Warning | Prefer a newline between declarations. |
| Typing | `RAV9001` | Opt-in | Info | Add an inferred return type annotation. |
| Typing | `RAV9003` | Default | Warning | Make an event delegate nullable when the event can be empty. |
| Typing | `RAV9004` | Opt-in | Warning | Use `let` when a local declared with `var` is never reassigned. |
| Initialization | `RAV9006` | Default | Warning | Initialize a property in storage or a constructor. |
| Typing | `RAV9012` | Opt-in | Info | Prefer `Option<T>` or `Result<T, E>` over nullable domain flow. A scoped code fix can rewrite simple local null-guarded flow to an `Option` pattern. |
| Error handling | `RAV9013` | Opt-in | Warning | Prefer `Result<T, E>` over `throw` for expected failure. |
| Error handling | `RAV9014` | Opt-in | Warning | Prefer Raven's `Option`/`Result` LINQ alternatives where applicable. |
| Typing | `RAV9015` | Default | Warning | Replace `== null` or `!= null`, which may invoke user-defined equality, with an identity-based `is null` or `is not null` check. Neither form refines the checked storage. This is a safety transformation, not a preference over pattern bindings or `Option<T>`. |
| Design | `RAV9016` | Opt-in | Info | Make an unexposed member private. |
| Design | `RAV9017` | Opt-in | Info | Make a method static when it does not use instance data. |
| Usage | `RAV9018` | Opt-in | Warning | Remove or use a property that is never referenced. |
| Usage | `RAV9019` | Opt-in | Warning | Remove or invoke a method that is never referenced. |
| Naming | `RAV9023` | Opt-in | Warning | Follow Raven's constructor-parameter naming convention. |
| Immutability | `RAV9026` | Default | Warning | Use the new value returned by an immutable collection operation. |
| Usage | `RAV9027` | Default | Warning | Remove or use an unused local value. |
| Style | `RAV9028` | Opt-in | Warning | Remove an unnecessary trailing separator. |
| Usage | `RAV9030` | Opt-in | Warning | Remove or use an unused parameter. |
| Usage | `RAV9031` | Default | Hidden | Remove an unused import directive. |
| Initialization | `RAV9032` | Default | Warning | Initialize a field in storage or a constructor. |
| Usage | `RAV9033` | Default | Warning | Dispose a disposable value before leaving its scope. |
| Usage | `RAV9034` | Default | Warning | Make an unused expression result explicit. This includes value-forming expressions and a non-`unit` tail value in a `unit` callable; full mode also checks bare calls and member accesses. |
| Style | `RAV9035` | Opt-in | Info | Prefer `let` over `val` for immutable lexical bindings. |
| Style | `RAV9036` | Opt-in | Info | Prefer `loop` over `while true` for an unconditional loop. |

### Known `RAV9027` macro-fragment gap

`RAV9027` does not yet count a caller local as used when its only reference is
inside an embedded Raven fragment reported by a macro. For example, `items` may
currently receive an unused-local warning even though the Query macro binds and
compiles its use correctly:

```raven
let items = [1, 2, 3, 4]
let queryResult = query! {
    from value in items
    where value > 2
    select value * 10
}
```

This is an analyzer limitation, not a macro-expansion or fragment-binding
error. Hover, completion, diagnostics, and semantic classification inside the
reported fragments use the compiler's macro-fragment semantic model. The
unused-local analyzer still needs to consume those semantic references before
it can suppress `RAV9027` in this case.

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

For nullable code, `RAV9012` expresses Raven's configurable preference for
`Option<T>` or `Result<T, E>` in domain flow. `RAV9015` has a narrower purpose:
it makes an existing equality-based null check strict so user-defined equality
is not involved. It does not make the checked value non-null in either branch.
The language still recommends explicit pattern bindings and matches as the
first teaching model. See
[Nullability and absence](../../lang/nullability.md).
