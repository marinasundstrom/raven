# Alternative-runtime iteration contracts

`CompilationOptions.RuntimeIterationContract` selects a nominal synchronous iteration
protocol. It is opt-in; null preserves Raven's existing .NET interface and pattern
iteration. The API is separate from `MetadataImportOptions`, which controls assembly
resolution. Alternate targets should supply their own metadata references and explicit
core configuration as well as this protocol.

```csharp
options = options.WithRuntimeIterationContract(new RuntimeIterationContract(
    "IterationContracts",
    "Contracts.Iterable`1",
    "Contracts.Iterator`1"));
```

The record also configures acquisition, advance and element-property names, defaulting
to GetIterator, MoveNext and Current. The compiler resolves these names from supplied CLI metadata. This is compiler-API configuration; project-file and CLI switches are not yet
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

## Cleanup boundary

This contract selects iteration symbols; it does not select an exception model or
change cleanup lowering. Iterator disposal and structured cleanup must be validated
separately for a target. A target lacking the required cleanup facilities cannot gain
correct disposal merely by renaming its iterable interfaces.
