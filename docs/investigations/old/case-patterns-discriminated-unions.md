# Discriminated union case-pattern investigation

## Objective
Summarize why `.Case(payload)` patterns still fail to bind for discriminated unions and capture the concrete compiler work needed to make them succeed end-to-end.

## Current behavior
Building `samples/discriminated-unions2.rav` still reports that `.Error(message)` is both unrecognized and unbound, even though the case exists. The match is deemed non-exhaustive, the `.Error` case name is treated as a missing identifier, and no payload locals are produced for the arm bodies.【25f513†L15-L25】

## Findings
1. **Pattern binding depends on synthesized `TryGet*` helpers.** The case-pattern binder resolves the case symbol from the union, then immediately looks for an instance `TryGet{Case}` method whose single `ref` parameter matches the nested case type. If the helper is absent, the binder replaces the entire pattern with a discard and never binds the payload arguments, which explains the missing locals and exhaustiveness diagnostic in the sample.【F:src/Raven.CodeAnalysis/BoundTree/BoundIsPatternExpression.cs†L561-L599】
2. **Union and case members are constructed but never attached.** `RegisterUnionCases` allocates constructors, payload properties, `ToString`, implicit conversions, and `TryGet{Case}` helpers for every case, yet none of these symbols are added to the corresponding `SourceNamedTypeSymbol` member tables. Only the case list is stored on the union, so the binder cannot find the `TryGet` methods it requires, and the generated case types expose no constructor or property members for lowering or codegen to consume.【F:src/Raven.CodeAnalysis/SemanticModel.cs†L916-L1119】
3. **Lowering assumes those members exist.** The match expression emitter calls the bound `TryGet` helper and then enumerates the case type’s property getters to feed nested pattern checks; without registered members, the pattern always falls through. The method-body generator already knows how to emit both the conversion operators and `TryGet` helpers once the symbols are discoverable, so wiring them into the member tables should unblock codegen as soon as binding succeeds.【F:src/Raven.CodeAnalysis/CodeGen/Generators/ExpressionGenerator.cs†L1113-L1178】【F:src/Raven.CodeAnalysis/CodeGen/MethodBodyGenerator.cs†L628-L703】

## Work required
* Attach all synthesized union and case members (constructors, properties, `ToString`, implicit conversions, and `TryGet*` helpers) to their containing symbols so the binder and code generator can discover them.
* Re-run case-pattern binding after the helpers exist to ensure payload locals and exhaustiveness tracking use the real `BoundCasePattern` instead of a discard.
* Add semantic and lowering tests that cover `.Ok(payload)`/`.Error(message)` patterns, including verification that the emitted `TryGet` helpers and case property getters are invoked during match lowering.
