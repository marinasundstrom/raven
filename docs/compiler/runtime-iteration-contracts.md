# Alternative-runtime iteration contracts

`CompilationOptions.RuntimeIterationContract` selects a nominal synchronous iteration
protocol. It is opt-in; null preserves Raven's existing .NET interface and pattern
iteration. The API is separate from `MetadataImportOptions`, which controls assembly
resolution. Alternate targets should supply their own metadata references and explicit
core configuration as well as this protocol.

```csharp
options = options.WithRuntimeIterationContract(new RuntimeIterationContract(
    "NeoCLR.CoreProbe",
    "System.Collections.Iterable`1",
    "System.Collections.Iterator`1"));
```

The record also configures acquisition, advance and element-property names, defaulting
to GetIterator, MoveNext and Current. The compiler contains no neoCLR-specific type
names. This is compiler-API configuration; project-file and CLI switches are not yet
implemented.

For `for value in values`, the collection must implement exactly one instantiation
of the selected iterable interface. The selected declarations must be accessible
one-parameter interfaces from the configured assembly name. Acquisition must be an
accessible parameterless instance method returning the corresponding Iterator<T>;
advance must be parameterless and return Boolean; the accessible non-indexed instance
property getter must return T. Missing/incompatible/ambiguous contracts report RAVT001
at the loop. The target path does not fall back to the .NET pattern by matching method
names. Arrays and ranges keep their existing lowering; async iteration and synthesized
yield iterators are outside this option.

The bound loop stores the resolved method symbols and element type. Existing codegen
emits their calls, so no target-specific opcode rewrite is needed. Semantic queries
see the same inferred loop element type. Option copies retain the contract, and changing
it blocks transfer of previous semantic state. No syntax or TextMate grammar changed.

## Known discrepancy: iterator disposal

Inspection on 2026-09-12 found that `EmitEnumeratorForLoop` currently emits acquisition,
MoveNext and Current but no automatic Dispose call, including on the default .NET path.
This is a behavior gap, not the desired contract. The target option does not fix it or
make a new guarantee about cleanup. Normal completion, break and return can execute
without disposing the cursor; explicit iterator use can still call Dispose itself.

Plan a separate correction covering normal exhaustion, break, return, nesting and
fault/exception exits. Keep .NET's disposal/unwinding expectations on the default target;
neoCLR needs a defined terminal-fault cleanup policy. Do not silently omit cleanup when
a target lacks a disposal member or exception-handling import support. The author asked
that this discrepancy be recorded and considered for a fix; no cleanup implementation
is claimed by this slice.

The subsequent author discussion proposed `defer` as a possible language construct for
neoCLR, which has no recoverable guest exception flow. Record this for now; no defer
syntax or cleanup mechanism is being added here. Cleanup still needs runtime support
for the relevant exits. A finally-only CLI region is one candidate representation and
does not require a catch clause or a guest Exception hierarchy; compare
[C# try-finally](https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/statements/exception-handling-statements)
and [CLI endfinally](https://learn.microsoft.com/en-us/dotnet/api/system.reflection.emit.opcodes.endfinally)
(primary sources consulted 2026-09-12). Which faults unwind, and what happens if cleanup
faults, remain open. Defer could lower onto that shared mechanism after its contract is
settled, rather than being treated as a substitute for it.
