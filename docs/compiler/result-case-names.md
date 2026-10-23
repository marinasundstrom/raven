# Result case naming

Raven.Core and neoCLR use Result.Ok(value) and Result.Failure(error). Ok stays short
for the common success path; Failure names the state and the error is its payload.
System.Error (neoCLR) and IError (Raven.Core) remain diagnostic contracts. Methods
such as MapError and UnwrapError still describe the error payload.

This is a source and metadata breaking rename of the previous Error case. Update
construction, patterns, generated case-type reflection and serialized case tags,
and rebuild consumers against matching libraries. Raven.Core's JSON converter
writes and reads Failure. No library alias named Error or Err is introduced.

Compiler Result recognition accepts Failure and legacy Error case metadata. This
covers try expressions, carrier conditional access, result-case semantic queries,
entry-point handling and unsafe-extraction diagnostics. Inferred top-level results
resolve the generic System.Result type even when its non-generic companion is in
scope. Propagation still uses the configured propagation protocol; it does not
wrap failures or require an error interface. No new Runtime Contract configuration,
CLI representation or native metadata encoding is introduced.

The same shared change is developed on main and carried deliberately to codex/neoclr;
neoCLR supplies its independently compiled library. Tests cover construction,
propagation, try conversion, conditional access and entry-point emission.

Compiler conveniences share internal `ResultFacts` names and failure-case lookup.
The current Failure case takes precedence over legacy Error when resolving cases.
Payload property names and arbitrary domain unions remain independent contracts.
Changing a constant alone does not migrate library declarations or serialized data.

Full diagnostic binding establishes the top-level return context before binding
return expressions, as incremental diagnostics already do. Inference resolves the
generic Result definition even when a nongeneric companion shares its name.

Validation: 184 focused .NET 11 tests pass for propagation, try, conditional
access, entry points, top-level binding and diagnostics. Raven main also passes
all 84 Raven.Core tests on .NET 10. Legacy custom Result case fixtures remain in
the focused suite. This does not qualify older .NET targets.
