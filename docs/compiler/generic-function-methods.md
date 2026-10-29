# Generic function method signatures for neoCLR

**Investigation, 2026-10-29; not implemented.** Author direction is to support a
general open-generic function capability, with signals only one consumer:

```text
// Proposed syntax. Invoke can be any method name.
class Forwarder<T> where T: func {
    func Invoke: T with args {
        // args: object?[]; null references and boxed value arguments.
    }
}
```

The method's authoritative signature is a function-type reference. It is either
a structural function type or an in-scope generic parameter constrained as a
function. neoCLR metadata must preserve that reference in the generic definition;
generic materialization substitutes it and produces a complete ordinary method
signature. It is not sufficient to expand source syntax only at known call sites.

The proposed `MethodInfo.FunctionType: TypeInfo` admits exactly those two cases.
A structural function can itself have unresolved generic components. A closed
method has ordinary reflected parameters and result information; it does not
require a special reflected method category. Names, owner/receiver and declaration
identity remain separate from signature type identity. Unit-only results are a
possible signal policy, not a language restriction on this capability.

The author permits redesign and breaking compatibility in this experimental work.
Prefer a single authoritative function-type signature representation over parallel
independent parameter/result and projected-signature metadata. This is permission
for that design direction, not a claim of completed implementation.

## Compiler findings

Inspected `codex/neoclr` at `b4c5790e199563a502ac20460dc260e87e0d0add`.

- `Syntax/Model.xml` and `Syntax/InternalSyntax/Parser/Parsers/TypeDeclarationParser.cs`
  require a parameter list. Add a signature-type alternative to declarations;
  update local/assembly function syntax deliberately rather than accidentally.
- `Binder/TypeMemberBinder.cs` constructs fixed parameter symbols from syntax.
  Bind an open signature reference and scoped argument-pack symbol instead.
- `Symbols/Constructed/StructuralFunctionTypeSymbol.cs` supplies existing structural
  shape identity and substitution. `ConstructedMethodSymbol.Parameters` currently
  substitutes a fixed list; projected methods require whole-signature resolution.
- `BoundTree/BoundInvocationExpression.cs` carries a concrete argument sequence and
  uses the selected method's result. Add dependent pack forwarding and result
  projection semantics, with lazy binder-owned state and consistent public semantic
  APIs. Do not misreport an unknown arity as zero parameters.
- `CodeGen/Portable/CallableSignature.cs` and native
  `NeoClrCallableDefinitionBuilder.cs` produce explicit parameter lists. Extend
  native plans, SDK authoring and imports to carry the function-type reference.
  `NativeAssemblySymbol.NativeMethodSymbol` currently imports expanded parameters
  through SDK GetParameters; it also needs open signature support.

Paths are relative to `src/Raven.CodeAnalysis` or `src/Raven.CodeAnalysis.NeoClr`
as applicable. No source changes were made in this investigation.

## Target contract and lowering

Raven parses/binds the general capability; admission and lowering for neoCLR use
explicit target capabilities. Native metadata and generic materialization must
resolve dependent signatures and expand argument-pack operations before concrete
ABI verification, GC-root planning and code generation. Ordinary .NET defaults
stay unchanged; no Runtime Contract setting has been added.

Represent `with args` as an owned `object?[]`, excluding the receiver. This is
primarily interop syntax; the author selected the nullable array element type.
This supersedes the earlier scoped-pack proposal for the initial implementation.
Materialized entry adapters box supported value arguments and preserve null
references. Array allocation/boxing optimizations must preserve observable
identity, mutation and escape behavior.

The proposed `FunctionReflectionHelpers.Invoke<T>(handler, args)` additionally
requires a result-type projection. Its parameters are `(T, object?[])`, while its
result is conceptually `ReturnTypeOf(T)`. This is distinct from giving a method
the entire signature T. The projection remains dependent in an open body and
resolves with materialization. It needs general type-expression support or an
explicit provisional intrinsic contract; an ordinary object-returning library
method cannot supply this static relationship by reflection alone. Helper
spelling, projection syntax and reflection-error policy remain design work.

A multicast loop that discards results still needs an explicit result policy for
the enclosing method when T returns a value, including the empty-handler case.
Preserve native no-result versus inhabited unit and diagnose unsupported byref
interop shapes. Task usage and async aggregation are separate issues by author
direction; do not infer implicit awaiting or result unwrapping.

The full metadata/substitution design, .NET comparison and retained local-driver
probe results are in the neighboring neoCLR repository's
[investigation](../../../neoclr/docs/design/generic-function-methods.md).
Initial probes are not implementation validation and the existing driver was not
rebuilt to match inspected source. Start with an SDK-authored dependent method,
materialization and provider/consumer roundtrip; then implement Raven syntax and
pack lowering against that contract. Include parser recovery, diagnostics,
semantic/operations APIs, signature help and incremental invalidation coverage.
Update syntax generators through the repository build workflow when implementing.

## Native metadata and CLI boundary

An open whole-signature dependency has no direct ordinary CLI MethodDefSig
encoding. Native export/import must preserve it; unresolved executable CLI export
must reject rather than erase it to a delegate or Object[] signature. Any closed
CLI projection is limited by the component-type/body profile. No new encoding,
runtime service or binary compatibility promise is implemented here.

General shared compiler changes belong on main and are carried deliberately to
the neoCLR line; native SDK/runtime changes stay behind target contracts. Keep
Raven and neoCLR bridge docs/changelogs aligned when implementation begins.

## Author-directed initial delivery profile (2026-10-29)

The acceptance target is a library `Signal<T>` / `SignalSource<T>` (or corresponding
Event names) that is useful wherever ordinary .NET events are used. A property
exposes subscriptions; the source retains raising authority. Compound assignment
operators may provide `+=`/`-=` syntax through Raven's ordinary operator support.
No event metadata or special signal type is introduced. An end-to-end unit-returning
notification consumer is the first qualification target; reactive operators and
Task handling are separate work.

Use the straightforward interop implementation: owned `object?[]` arguments,
reflection invocation, checked result adaptation and normal generic return-path
checking. Do not expand this slice into return-shape constraints, allocation
optimizations or Task integration. For arbitrary open T, the expected result is
opaque `ReturnType<T>`; returning a matching helper result is valid, but falling
through cannot establish a result for every possible construction. The distinction
between native no-result and inhabited source unit remains relevant to adaptation.

The author selected return projection as a general capability. The implementation
uses a native `FunctionReturn` type expression, retaining its operand until generic
substitution. The initial Raven spelling under development is `ReturnType<T>`;
this is a type operator on the neoCLR target, not a nominal wrapper. The metadata
SDK's `SignatureType.FunctionReturn` and `FunctionReturnTypeInfo` describe the same
operation. Open signatures retain the projection; a concrete function operand
resolves directly to its result. .NET-target admission is not implied.

Implementation is in progress. SDK dependent-signature and result-projection
roundtrips pass, and runtime/native-codegen compilation passes. Raven's existing
11 structural-function tests passed before compiler changes. New projection
semantic tests, projected method syntax, typed invocation helper, guest reflection
API, and executable signal qualification are not yet claimed complete.
