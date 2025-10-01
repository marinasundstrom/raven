# Extension methods and lambda integration investigation

This investigation replaces the previous collection of design notes that had drifted out of sync with the current binder and lowerer. It re-establishes a single source of truth for how extension method consumption interacts with lambda expressions so we can plan the next iteration of work.

## Snapshot of existing behavior

* **Extension invocations rewrite the receiver** – When overload resolution selects an extension method, `BlockBinder.ConvertInvocationArguments` injects the receiver as the synthetic first argument and then converts the remaining call-site arguments.【F:src/Raven.CodeAnalysis/Binder/BlockBinder.cs†L3958-L3991】
* **Argument counting already treats the receiver specially** – `AreArgumentsCompatibleWithMethod` increments the provided argument count whenever the candidate is an extension method and the binder synthesized an extension receiver, preventing arity checks from double-counting that value.【F:src/Raven.CodeAnalysis/Binder/BlockBinder.cs†L4017-L4025】
* **Lambda rebinding is cached per delegate** – After overload resolution picks a delegate type, `LambdaRebindKey` ensures the binder only rebinds each lambda body once per delegate target, avoiding exponential work when metadata extensions introduce new delegate shapes.【F:src/Raven.CodeAnalysis/Binder/BlockBinder.cs†L4042-L4074】
* **Regression coverage stops at metadata consumption** – The semantic suites verify both metadata-backed and Raven-authored extensions bind and lower correctly, but they only cover direct method invocations. No test asserts that a lambda argument supplied to an extension method rebinds after the receiver swap.【F:test/Raven.CodeAnalysis.Tests/Semantics/MetadataExtensionMethodSemanticTests.cs†L11-L149】【F:test/Raven.CodeAnalysis.Tests/Semantics/ExtensionMethodSemanticTests.cs†L12-L608】

## Outstanding gaps to investigate

1. **Replay lambda conversions after extension promotion** – Metadata extensions that accept delegates still surface binding failures when the receiver forces overload resolution to reconsider lambda shapes. We need to confirm whether the lambda cache is consulted with the correct delegate type after the extension receiver is injected.
2. **Audit lowering of extension-sourced delegates** – Expression lowering rewrites extension calls into static invocations, but we do not record whether delegate arguments preserve their inferred types once the receiver becomes the first parameter. Instrument the lowerer to flag any delegate conversions that change type after rewriting.
3. **Separate metadata vs. source behaviors** – Raven-authored extensions use `SourceMethodSymbol.ComputeIsExtensionMethod`, while metadata extensions rely on attributes. Verify both pathways mark the parameters identically so the binder applies the same receiver synthesis logic regardless of origin.【F:src/Raven.CodeAnalysis/Symbols/Source/SourceMethodSymbol.cs†L197-L221】【F:src/Raven.CodeAnalysis/Symbols/PE/PEMethodSymbol.cs†L165-L208】

## Proposed next steps

* Capture a minimal failing example that exercises a lambda argument flowing through an extension method, preferably one drawn from `System.Linq` to mirror real-world usage.
* Extend the semantic test fixtures to cover that scenario for both metadata and source extensions, ensuring the binder rebinds lambdas after rewriting the receiver.
* Introduce targeted logging or assertions inside `ConvertInvocationArguments` to guarantee the converted extension receiver and lambda arguments share the expected delegate type before and after lowering.
* Update the language specification once the investigation produces actionable changes so the documented extension behavior matches the implementation.
