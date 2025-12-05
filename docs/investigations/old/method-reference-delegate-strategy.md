# Method reference delegates implementation plan

## Background

- The language specification treats functions and methods as first-class values: referencing a method without invoking it must yield a delegate, overloads require explicit disambiguation, and the compiler synthesizes a delegate when no existing type matches the signature.【F:docs/lang/spec/language-specification.md†L767-L800】
- The binder currently fails to treat method symbols as expressions. `BindIdentifierName` only returns namespaces, types, locals, parameters, fields, or properties; any other symbol (including methods) becomes a bound error.【F:src/Raven.CodeAnalysis/Binder/BlockBinder.cs†L1683-L1702】
- Member access binding reuses `BoundMemberAccessExpression`, but the resulting node reports a method's *return type* as its expression type, so `Console.WriteLine` is seen as producing `void` rather than a callable value.【F:src/Raven.CodeAnalysis/BoundTree/BoundMemberAccessExpression.cs†L3-L25】
- Invocation binding assumes either a single resolved method symbol or a callable receiver and does not understand a method-group expression that must be converted to a delegate first.【F:src/Raven.CodeAnalysis/Binder/BlockBinder.cs†L1811-L2080】
- Conversion classification and application only handle type-to-type conversions today; they have no path to translate a method group into a delegate target before emit.【F:src/Raven.CodeAnalysis/Compilation.cs†L464-L738】【F:src/Raven.CodeAnalysis/Binder/BlockBinder.cs†L2580-L2667】
- Lambda infrastructure already infers delegate signatures and emits IL for delegates via `Compilation.CreateFunctionTypeSymbol` and `EmitLambdaExpression`, providing patterns we can reuse for method references.【F:src/Raven.CodeAnalysis/Binder/BlockBinder.cs†L600-L720】【F:src/Raven.CodeAnalysis/CodeGen/Generators/ExpressionGenerator.cs†L150-L334】

## Implementation steps

### Status snapshot

- ✅ The tree layer now understands method groups: `BoundMethodGroupExpression` tracks receivers, candidates, and lazily-computed delegate types, and the walker/rewriter/symbol info plumbing propagates the overload set to consumers.【F:src/Raven.CodeAnalysis/BoundTree/BoundMethodGroupExpression.cs†L1-L53】【F:src/Raven.CodeAnalysis/BoundTree/BoundTreeRewriter.cs†L6-L76】【F:src/Raven.CodeAnalysis/BoundTree/BoundTreeWalker.cs†L22-L199】【F:src/Raven.CodeAnalysis/BoundTree/BoundExpressionExtensions.cs†L7-L23】
- ✅ Binder entry points now surface method groups: identifier and member access binding fold overload sets into `BoundMethodGroupExpression`, and invocation dispatch hands those candidates to the overload resolver before falling back to existing call paths.【F:src/Raven.CodeAnalysis/Binder/BlockBinder.cs†L1645-L1758】【F:src/Raven.CodeAnalysis/Binder/BlockBinder.cs†L1760-L2115】
- ✅ Conversions now translate method groups into delegates: `ApplyConversion` recognizes method-group operands, selects overloads that match a delegate target, and produces `BoundDelegateCreationExpression` nodes while preserving ambiguity state for diagnostics.【F:src/Raven.CodeAnalysis/Binder/BlockBinder.cs†L2724-L2876】【F:src/Raven.CodeAnalysis/BoundTree/BoundDelegateCreationExpression.cs†L1-L21】
- ✅ Delegate inference now synthesizes missing delegate types, caching signatures so repeated method references reuse either `Func`/`Action` or a compiler-generated `MulticastDelegate` emitted during codegen.【F:src/Raven.CodeAnalysis/Compilation.cs†L143-L214】【F:src/Raven.CodeAnalysis/Symbols/Synthesized/SynthesizedDelegateTypeSymbol.cs†L1-L111】【F:src/Raven.CodeAnalysis/CodeGen/TypeGenerator.cs†L35-L137】

1. ✅ **Introduce a bound representation for method groups.** *(Completed)*
   - Add a `BoundMethodGroupExpression` (or similar) that carries the receiver (nullable for static references), the candidate `IMethodSymbol` list, and lazily-computed delegate type.
   - Extend symbol info storage so the semantic model can report all candidate methods alongside the chosen symbol (matching Roslyn's `MethodGroup` behavior).

2. ✅ **Update binder entry points to produce method groups.** *(Completed)*
   - `BindIdentifierName`, `BindMemberAccessExpression`, and `BindMemberBindingExpression` now aggregate overload sets via `LookupSymbols`/`SymbolQuery.LookupMethods` and return `BoundMethodGroupExpression` nodes so both free functions and receiver-qualified method references preserve their candidate lists.【F:src/Raven.CodeAnalysis/Binder/BlockBinder.cs†L1628-L1768】
   - `BindInvocationExpression` recognizes method groups, binding arguments once, routing candidates through the overload resolver, and reusing the existing conversion pipeline for resolved targets or diagnostic reporting when no match exists.【F:src/Raven.CodeAnalysis/Binder/BlockBinder.cs†L1760-L2115】

3. ✅ **Implement method-group to delegate conversions.** *(Completed)*
   - `ApplyConversion` detects method-group operands with delegate targets, filters overloads against the delegate `Invoke` signature, and wraps the resulting group in `BoundDelegateCreationExpression` so downstream phases see the inferred delegate type and selected method.【F:src/Raven.CodeAnalysis/Binder/BlockBinder.cs†L2724-L2876】【F:src/Raven.CodeAnalysis/BoundTree/BoundDelegateCreationExpression.cs†L1-L21】
   - Conversion sites (locals, returns, assignments, arguments, and collection initializers) now treat method groups as convertible even though their natural type is `Error`, letting delegate-typed contexts drive overload selection while preserving ambiguity for later diagnostics.【F:src/Raven.CodeAnalysis/Binder/BlockBinder.cs†L120-L3058】

4. ✅ **Report overload ambiguity and missing-target diagnostics.** *(Completed)*
   - The binder now emits `RAV2201` when a method group appears without any delegate-typed context, and surfaces `RAV2202`/`RAV2203` when overload resolution cannot choose a unique target for a delegate conversion.【F:src/Raven.CodeAnalysis/Binder/BlockBinder.cs†L132-L176】【F:docs/lang/spec/language-specification.md†L781-L796】
   - Conversion sites pass syntax locations into `ApplyConversion`, allowing delegate creation to report precise diagnostics for ambiguous or incompatible method groups.【F:src/Raven.CodeAnalysis/Binder/BlockBinder.cs†L154-L3120】

5. ✅ **Synthesize delegates when no suitable type exists.** *(Completed)*
   - Method groups with a single candidate now cache inferred signatures and either reuse `Func<>`/`Action<>` or synthesize a dedicated delegate so inference works without annotations.【F:src/Raven.CodeAnalysis/Compilation.cs†L143-L214】
   - `SynthesizedDelegateTypeSymbol` produces runtime metadata by modeling constructor and `Invoke` members that mirror the referenced method's parameter and ref-kinds.【F:src/Raven.CodeAnalysis/Symbols/Synthesized/SynthesizedDelegateTypeSymbol.cs†L1-L111】
   - Code generation recognizes synthesized delegates and emits `MulticastDelegate` definitions with runtime-implemented constructors and `Invoke` methods so inferred delegates execute correctly.【F:src/Raven.CodeAnalysis/CodeGen/TypeGenerator.cs†L35-L137】

6. ✅ **Lower delegate creation in the emitter.** *(Completed)*
   - `ExpressionGenerator` now visits `BoundDelegateCreationExpression`, loading static targets with `ldftn`/`newobj` and using `ldvirtftn` with `dup` for instance receivers while boxing value types so the delegate constructor sees the correct `this` object.【F:src/Raven.CodeAnalysis/CodeGen/Generators/ExpressionGenerator.cs†L152-L199】
   - When a synthesized delegate supplies the target type, the emitter requests its `TypeBuilder` before resolving the constructor so compiler-generated delegates can be instantiated just like framework `Func`/`Action` types.【F:src/Raven.CodeAnalysis/CodeGen/Generators/ExpressionGenerator.cs†L158-L163】

7. ✅ **Augment semantic model, tooling, and tests.** *(Completed)*
   - `SemanticModel.GetSymbolInfo` now binds expressions and statements through the cached bound tree so method groups surface candidate sets and inferred delegate types just like other expressions.【F:src/Raven.CodeAnalysis/SemanticModel.cs†L80-L110】
   - Added unit tests that exercise semantic model queries, diagnostics, runtime execution, and metadata emission for static, instance, and `ref`/`out` method references so the end-to-end experience is covered.【F:test/Raven.CodeAnalysis.Tests/Semantics/MethodReferenceSemanticTests.cs†L1-L116】【F:test/Raven.CodeAnalysis.Tests/Semantics/MethodReferenceDiagnosticsTests.cs†L1-L55】【F:test/Raven.CodeAnalysis.Tests/CodeGen/MethodReferenceCodeGenTests.cs†L1-L143】
   - Invocation regression tests confirm that supplying a method group argument correctly selects delegate-typed overloads without disturbing existing call semantics.【F:test/Raven.CodeAnalysis.Tests/Semantics/MethodReferenceSemanticTests.cs†L48-L115】

## Follow-up questions

- Decide whether open instance method conversions (where the delegate's first parameter becomes the receiver) are in scope or should be deferred until the basic closed-form support is stable.
- Confirm whether synthesized delegates must live alongside user code (e.g., nested in a compiler-generated namespace) and how they should participate in reflection or metadata emission policies.
- Evaluate caching lifetime for synthesized delegates to avoid memory churn during incremental compilation.
