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
