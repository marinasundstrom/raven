# .NET behavior across the neoCLR emission refactoring

## 2026-10-01 bounded audit

The author prioritizes regressions introduced by the shared-codegen refactoring
before further native emission work. Move semantic decisions earlier when appropriate;
this is not a directive to move backend handle resolution into binding.

Comparison: shared main `e5607ca17` versus integration `eb606a3bf`, after the six
independently proven compiler fixes were extracted to main. Existing constructor,
field, loop, unsigned-array, virtual-call and receiver tests pass 21/21 on main and
25/25 on integration (four additional integration tests). Invocation and iteration
coverage passes 99/99 on both. Integration initially lost decimal-loop console output
in a parallel run; the isolated test and serial group pass. This is not confirmed as
an emission regression; console-capture interference is the suspected cause.

`SharedEmissionParityTests` adds return-value/fault checks for receiver and argument
evaluation order, collection replacement during iteration, short-circuit side effects
and null receiver calls. All five cases pass on both lines and exercise Debug and
Release. No new refactoring regression is established by this bounded audit. It is
not full-suite, library-bootstrap or native-target qualification.

The imported union carrier probe is already semantically wrong on shared main:
`Choice<Item>(None())` selects the Some constructor before emission. Keep this as
an independent binder/symbol investigation, not a backend workaround. A separate
captured-loop-variable probe returns 0 on main and 333 on integration instead of
123; both lines are incorrect. Shared array lowering changes the manifestation,
and closure lifetime/iteration capture remains an explicitly open general issue.
Neither issue is claimed fixed or used as passing acceptance evidence here.

Reproduce the new checks with:

```sh
dotnet test test/Raven.CodeAnalysis.Tests -p:WarningLevel=0 --filter FullyQualifiedName~SharedEmissionParityTests
```

For console-output groups use xUnit `ParallelizeTestCollections=false` in runsettings.
Native emission can resume within the verified subset; captured-loop callbacks and
explicit imported carrier construction remain excluded from that acceptance scope.


## 2026-10-03 target-boundary reassessment

The author directs preserving established .NET compilation while retaining the NeoCLR
metadata library and finding simpler shared paths. Host .NET is not the target runtime.
This audit compares main `46491585e` with integration `97d07b901`; it changes no compiler
implementation and does not claim full-suite parity.

### Executable evidence

Both worktrees were built during the preceding array audit. The additional tests use
`dotnet test test/Raven.CodeAnalysis.Tests/Raven.CodeAnalysis.Tests.csproj --no-build
-p:WarningLevel=0 --settings /tmp/raven-audit-serial.runsettings --filter <filter>`.
The runsettings disable xUnit collection parallelism to avoid console-capture interference.
The test host is net11.0. Earlier net10.0 native/bootstrap evidence is separate.

| Group | Main | Integration |
| --- | --- | --- |
| Array/iteration audit already recorded in neoCLR | 36 pass | 36 pass |
| Invocation, receiver, constructor, field, loop, collection | 71 pass | 71 pass |
| Metadata/import/propagation | 44 pass, 1 fail | 45 pass, 1 fail |

The 71-test filter includes GenericInvocationCodeGenTests, ValueTypeReceiverCodeGenTests,
MethodReferenceCodeGenTests, RuntimeSymbolResolverTests, LoopStatementCodeGenTests,
FieldInitializationTests, BaseConstructorTests and CollectionExpressionTests.
The import filter includes ImportedUnitInterfaceTests, ImportedInterfaceIndexerTests,
ImportedTypeArityTests, PropagationCodeGenTests, NullableAttributeEmissionTests,
ImportedEmptyUnionCaseTests, ImportedGenericMethodContextTests,
ImportedMemberUnionPatternTests and ImportedUnionEmissionTests. Join each group with
`|`, using `FullyQualifiedName~` before each class name.

Both fail GenericParameterConstraintKinds_RoundTripThroughMetadata for struct:
expected ValueType, imported ValueType | Constructor. Main commit `b4052b0aa` deliberately
emits the implied constructor flag. Source-versus-imported ConstraintKind normalization
and the test expectation need reconciliation; do not silently change the test or attribute
this shared failure to the integration branch. The extra integration import case passes.
A default-driver array/IList mutation/LINQ program also returns expected 42 on both lines.
These checks do not cover all async, closure, debugging or framework behavior. The earlier
captured-loop-variable discrepancy remains open and is not requalified by this run.

### Keep, simplify, or consider reverting

| Area and concrete implementation | Recommendation | Reason and next proof |
| --- | --- | --- |
| Separate NeoCLR metadata library, Introspection.MetadataLoadContext, builders/GetILGenerator | Keep | Own format resolution, construction and encoding outside Raven; retain native execution evidence. |
| ISemanticMetadataReference / CompositeSemanticDataLoader / NativeMetadataContext | Keep the narrow symbol-provider boundary | Existing CLI references still use DotNetSemanticDataLoader. Native views project into symbols; do not recreate reflection or translate native references to CLI. |
| Compilation.Emit / ICompilationEmissionBackend | Keep explicit selection | Diagnostics/preparation are shared; output ownership belongs to the selected backend. |
| IImportedAssemblySymbol.ResolvedArtifact and native symbol-authored references in Int32Emitter | Keep | Native emission authors output-owned references from symbol facts and explicit artifact identity. Legacy definition lookup is a separate branch and rejects native unsupported contracts. |
| ReflectionEmitLinearMethodBuilder.TryEmit called by MethodGenerator | First simplification/reversion candidate | Release, non-debug methods may use a second bounded body generator while other methods fall back to MethodBodyGenerator. It adds parity work without eliminating the established generator. Assess removing production .NET selection while retaining the native planner and comparison tests. No removal is performed here. |
| LinearMethodBody / SourceCallablePlan / CallableSignature / EmissionCapabilities | Keep supported native functionality, stop expanding merely to exercise both emitters | These implement a bounded translation, not a complete shared backend. Do not introduce another IR to replace them. Share individual semantic operations only when their equivalence is established. |
| Lowerer.Loops array transformation | Audit separately | This changes default .NET input to codegen; the earlier capture discrepancy demonstrates that passing primitive loops is insufficient. Require evaluation-order, capture and transfer tests before deciding whether to keep or narrow it. |
| Native and legacy branches in Int32Emitter / NeoClrEmitOptions | Simplify incrementally | Native references already have a symbol-only path. Inventory remaining legacy probe consumers before removing bridge branches; preserve the explicitly permitted primitive bootstrap. |
| .NET source-void mapping and NeoCLR-library-on-CLR services | Quarantine for review | Added for cross-runtime library execution, not ordinary .NET targeting. Do not expand adapters or merge these merely because their isolated tests pass. Establish actual consumers before deciding removal. |

### Smallest next sequence

1. Reconcile the shared constraint discrepancy independently, deciding the public symbol
   contract before adjusting code or expectations. Keep fixes separate from target work.
2. Test the proposed removal of production .NET portable-path selection in isolation,
   comparing Debug/Release behavior, generics, callbacks, exceptions and debug output.
   Keep the established emitter and native metadata backend; do not replace either.
3. Audit shared array lowering and the recorded captured-loop behavior. Distinguish bugs
   already on main from integration changes in behavior and fix at the responsible layer.
4. Inventory legacy/native import and emission branches against real consumers, then remove
   proven-unused bridge work one slice at a time. Re-run a native library/consumer and the
   existing native broad gate for changes that affect those paths.

This is a bounded recommendation, not approval to rewrite both backends or a claim that
all listed code must be reverted. The proposed CLR array adapter remains suspended.
Ordinary .NET regression tests and native library/runtime acceptance must remain separate;
forcing the NeoCLR source library to execute on the CLR is not proof of target parity.


Constraint reconciliation (2026-10-03): the PE importer intentionally retains CLI flags,
while source flags describe written constraints. Correct the round-trip test's imported
expectation for struct to ValueType | Constructor; retain the source ValueType assertion.
This changes no compiler contract. Four round-trip cases pass on integration; the same
four plus three existing CLR metadata flag cases pass on the main-based fix branch.
