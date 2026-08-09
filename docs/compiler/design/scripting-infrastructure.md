# Scripting infrastructure

## Decision

Raven will provide its own scripting infrastructure in
`Raven.CodeAnalysis`. Its public shape should follow the useful parts of the
Roslyn scripting model, but Raven will not depend on either the Dynamic Language
Runtime or `Microsoft.CodeAnalysis.Scripting`.

The DLR hosting APIs solve a different problem: hosting languages implemented
on the DLR and providing dynamic object interoperability. Raven is a statically
compiled .NET language and does not use the DLR execution model.

Roslyn's scripting API is a closer architectural reference. It models an
immutable script, a chain of submissions, execution state, options, globals,
and a result. However, it is not an external language-provider contract. Its
language bridge (`ScriptCompiler`), script builder, and script constructors are
internal. C# and Visual Basic enter through implementation-owned factories.
Raven therefore cannot plug its compiler into that package without depending on
Roslyn internals or copying implementation details that should remain under
Raven's control.

Relevant Roslyn references:

- [`Script` and `Script<T>`](https://github.com/dotnet/roslyn/blob/main/src/Scripting/Core/Script.cs)
- [`ScriptState` and `ScriptState<T>`](https://github.com/dotnet/roslyn/blob/main/src/Scripting/Core/ScriptState.cs)
- [`ScriptOptions`](https://github.com/dotnet/roslyn/blob/main/src/Scripting/Core/ScriptOptions.cs)
- [C# submission creation](https://github.com/dotnet/roslyn/blob/main/src/Scripting/CSharp/CSharpScriptCompiler.cs)
- [compiler-level script compilation information](https://github.com/dotnet/roslyn/blob/main/src/Compilers/Core/Portable/Compilation/ScriptCompilationInfo.cs)
- [submission member lookup](https://github.com/dotnet/roslyn/blob/main/src/Compilers/CSharp/Portable/Binder/InSubmissionClassBinder.cs)
- [cross-submission storage](https://github.com/dotnet/roslyn/blob/main/src/Compilers/CSharp/Portable/Lowering/SynthesizedSubmissionFields.cs)
- [cross-submission reference lowering](https://github.com/dotnet/roslyn/blob/main/src/Compilers/CSharp/Portable/Lowering/LocalRewriter/LocalRewriter_PreviousSubmissionReference.cs)

## Architectural boundary

The scripting API belongs in `Raven.CodeAnalysis`, beside the compiler APIs it
coordinates. The `rvn` tool is a host and consumer of that API. It should not
be the implementation boundary for compilation or submission state.

The initial public model should contain Raven-owned equivalents of these
concepts:

- `RavenScript` as the language-specific factory and convenience API;
- `Script` and `Script<T>` as immutable source plus continuation objects;
- `ScriptOptions` for references, imports, source resolution, target framework,
  diagnostics, and execution-related compiler options;
- `ScriptState` and `ScriptState<T>` for the successfully executed submission
  chain, variables, return value, and a captured runtime exception when the host
  requests that behavior;
- `ScriptVariable` for host inspection of declared submission variables;
- `ScriptRunner<T>` as a reusable compiled executor delegate;
- an injectable assembly-loading service owned by the host layer.

These names are provisional until the first API slice is reviewed. The
important contract is the separation between immutable compilation input,
compiled submission, and mutable execution state.

## Compiler support

A real interactive session cannot be implemented by repeatedly compiling and
running a cumulative source file. Doing so would rerun earlier side effects.
Compiling every input independently would instead lose earlier declarations and
values.

Raven needs compiler-level submission support with these properties:

1. A submission compilation records its previous submission compilation.
2. Declarations from previous submissions participate in binding.
3. Each new submission emits only its own executable body and storage.
4. The emitted submission can access prior submission storage.
5. The trailing expression can produce a typed result.
6. Syntax APIs can distinguish complete, incomplete, and invalid submissions so
   the REPL can request continuation lines without guessing from diagnostics.
7. One-shot compilation remains independent and authoritative.

This likely requires a Raven equivalent of `ScriptCompilationInfo`, plus a
`Compilation.CreateScriptCompilation` or `CreateSubmissionCompilation` factory.
Submission-specific state must be compilation-owned rather than added to normal
project or language-server workspace state.

Previous declarations enter executable binding through a dedicated
`SubmissionBinder`. They are intentionally not copied into `TopLevelBinder`'s
local and function tables: the current top-level method does not own them, and
later emission must access them through persistent submission storage. A
submission chain is also one logical internal-access domain, while file-scoped
declarations remain file-scoped.

This mirrors Roslyn's separation of responsibilities without copying its
implementation. Roslyn's `InSubmissionClassBinder` searches the members of the
current and previous submission classes. Its lowering then creates receiver
fields for referenced earlier submissions. Raven has established the binder
boundary first; the next compiler slice must replace the provisional source
symbols returned from earlier submissions with emitted submission members and
lower cross-submission accesses through their storage representation.

The first executable representation uses typed state cells. Each submission
variable has a stable slot, the declaring submission stores its value after
initialization or assignment, and later submissions load and update that slot
through `SubmissionRuntime`. The runtime scope is ambient and async-flow-aware,
but is entered and owned by the future `ScriptState` host API. All related
code-generation hooks are gated by `Compilation.IsSubmission`; ordinary
top-level programs continue to use normal locals exclusively.

This cell representation proves continuation semantics without rerunning prior
side effects. Functions and user-defined types cross the submission boundary
through real emitted metadata references. `SubmissionCompilationState` owns the
lazy declaration projection, emitted-reference chain, accessibility domain, and
variable slots; `SubmissionCodeGenerator` owns the runtime cell bridge. This
keeps `Compilation`, the general binders, and the normal emitters as thin
coordinators and avoids runtime reflection dispatch.

Default top-level functions and types in a submission are emitted publicly so a
later submission assembly can reference them through ordinary CLR metadata.
This affects submission compilations only; regular project accessibility is
unchanged.

## Execution model

`Script.RunAsync` executes a chain from its beginning. Continuing an already
executed `ScriptState` executes only the new submission and returns a new state.
`rvn eval` creates and runs one script. `rvn repl` retains the latest successful
state and continues from it.

Compilation diagnostics and runtime failures remain distinct:

- compilation produces ordinary Raven diagnostics and does not start execution
  when errors are present;
- runtime exceptions normally propagate, with an opt-in host policy for
  capturing them in the resulting state;
- cancellation is accepted by compile and execute APIs;
- output formatting belongs to the host, not the compiler API.

`ScriptExecutionSession` abstracts the runtime mechanism behind the public
`RavenScript`, `Script`, `ScriptOptions`, and `ScriptState` APIs. It owns typed
variable cells, sequential execution, emitted reference files, and a
collectible `AssemblyLoadContext`. Disposing a state releases the entire chain;
the load context is not a security boundary and unload remains cooperative.
The host mechanism stays internal so it can evolve without leaking runtime
loader details into compiler APIs.

For a submission ending in a value-producing expression, `TopLevelBinder`
routes that expression to a compiler-owned `SubmissionResultSymbol`.
`SubmissionCodeGenerator` stores the typed value in the active execution
context, and `ScriptState` exposes it through `HasReturnValue` and
`ReturnValue`. Unit-producing statements leave `HasReturnValue` false. This
policy is gated by `Compilation.IsSubmission`, so normal top-level programs
continue to discard expression-statement values exactly as before.

## Submission completeness

`SyntaxTree.GetSubmissionCompleteness()` is the first compiler-facing scripting
API. It applies to syntax trees parsed with `SourceCodeKind.Script` or
`SourceCodeKind.Interactive` and returns one of three results:

- `Complete` for syntactically valid input that is ready to compile;
- `Incomplete` when the final construct needs more input, such as an open block,
  missing operand, unterminated literal or comment, or unmatched `#if`;
- `Invalid` when the input is complete but contains syntax errors.

This lets an interactive host request continuation lines only for `Incomplete`.
Both `Complete` and `Invalid` are final submissions: the former proceeds to
compilation and the latter reports parser diagnostics immediately.

## CLI preparation

The current `rvn` file runner shells out to `rvnc --run`, while project commands
delegate to the .NET SDK. Those behaviors remain valid during the transition,
but they are now isolated behind command-specific classes rather than embedded
in the CLI entry point.

When the scripting engine is ready:

- `FileApplicationCommand` can consume a one-shot Raven script runner instead
  of constructing the compiler-driver process directly;
- `eval` can use the same engine with a string source and result formatting;
- `repl` can retain a `ScriptState` and add interactive input handling;
- project `build`, `run`, and `clean` remain SDK operations and do not need to
  flow through the scripting engine.

## Delivery slices

1. Introduce script syntax/completeness classification and focused parser tests.
   This slice is implemented by `SyntaxTree.GetSubmissionCompleteness()`.
2. Add compiler submission chaining and a dedicated submission binder, with
   semantic tests proving that declarations from a previous submission bind in
   the next one. Implemented together with typed runtime variable storage and
   emitted references for functions and types.
3. Add `RavenScript`, `Script`, `ScriptOptions`, and `ScriptState` over those
   compiler primitives. Implemented with continuation execution and explicit
   lifetime management.
4. Capture and expose a typed trailing-expression result. Implemented through
   a compiler-owned result destination and the submission runtime bridge.
5. Implement `rvn eval` as the first CLI consumer.
6. Add reset/unload coverage and `rvn repl`.

The public API should be introduced incrementally with each working compiler
slice. Avoid publishing placeholder interfaces whose required state and
lifetime rules have not yet been proved by execution tests.
