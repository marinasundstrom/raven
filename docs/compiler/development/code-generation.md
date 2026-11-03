# Code generation notes

When emitting IL we must resolve CLR metadata types through the same `MetadataLoadContext` used by the compiler.

- Always obtain runtime type symbols via the symbols API or the `CoreAssembly` helpers so they originate from the metadata context hosting the reference assemblies.
- Avoid using `typeof(...)` when you need a `Type` handle in the emitter. `typeof` binds against the compiler host's runtime and the resulting symbol will not match the metadata context.
- If you start from an `ITypeSymbol`, use `GetClrType()` to map the symbol into the `MetadataLoadContext` before passing it to the IL generator.
- Every type, method, and field reference emitted into IL must ultimately resolve through the `MetadataLoadContext`; mixing contexts will produce invalid handles or missing members at runtime.

Following these rules keeps the code generator consistent with the metadata snapshot supplied to `MetadataLoadContext` and ensures we emit IL using the correct reference assemblies.

## Nullable source structs and coalescing

Null-coalescing emission uses the same nullable-accessor helpers as other nullable
operations. For a constructed Nullable<T> containing a source TypeBuilder (also
inside generic source structs), Reflection.Emit cannot directly reflect its
properties. Map HasValue and Value from the generic nullable definition instead.
The left operand is evaluated once, and the right operand only on the empty path.
Tests cover present/empty values, default fallback and generic/non-generic source
structs through observable execution, without asserting opcode sequences.

## Imported nullable conversion parameters

Imported signatures may retain a constructed `System.Nullable<T>` symbol instead
of the nullable wrapper used for Raven syntax. Conversion classification and CLI
conversion emission normalize these signatures through the same internal helper.
This includes user-defined operators accepting nullable value types. Target-owned
nullable ABI projection still decides how the normalized type is represented;
normalization does not introduce a new language conversion.

Closed nullable metadata types must also keep their generic definition and type
argument in the same target metadata context. A host `Nullable<>` definition plus
a metadata-only argument produces a mixed reflection type; constructor member
resolution can then use a host token against the target reference assembly.
Source-defined type-builder arguments retain their existing emission path.

## Generic annotations and conditional-access results

The value-handling decision unwraps nullable annotations whose target ABI projects
to the underlying type. An unconstrained `T?` remains a generic parameter slot;
it must not be treated as an object slot and receive a boxed `T` argument. This
also applies to synthesized record equality invoking `EqualityComparer<T>`.
Actual nullable value types retain their distinct nullable representation.

Conditional access that lifts a source enum or value type into `Nullable<T>` uses
the shared nullable-constructor mapping helper. Reflection cannot directly query
constructors on a `TypeBuilderInstantiation`; map the definition constructor.

### Captured fields in generic methods

Outer-method accesses to captured locals resolve fields against the constructed
closure type used by the closure instance. Returning an open FieldBuilder from
captured-field lookup can encode closure type parameters in the enclosing
method's context, producing invalid metadata even if the field value itself is
non-generic. MethodBodyGenerator returns FieldInfo mapped through the closure's
runtime type; load, store and address-taking consumers share that mapping.
Inside the closure body the defining field remains appropriate. Existing async
state-machine field lookup is unchanged. Runtime regressions exercise shared
local reads/writes with value and reference method type arguments.

### Property-pattern receiver storage

Property-pattern emission retains a typed input local for reference receivers.
An object-typed temporary is optional: it exists only when boxing or a narrowed
type test needs it. Receiver casts must load that temporary when present, or the
input local otherwise. Constructed runtime types containing source-defined type
arguments may reach the cast path even when their logical type is unchanged.
Loading a missing optional local previously surfaced as the misleading IL-builder
error `Local was not created by this IL builder`. The correction retains null
failure and property-test semantics and is covered by null, empty and populated
List<SourceType> runtime cases.


### Iterator instance receivers

Shared iterator lowering redirects original `self` and `base` receivers through
the state machine's captured instance. Implicit instance-field reads and writes
use that instance as well; synthesized state-machine fields retain their own
receiver. This preserves ordinary field and method behavior across yields.
Runtime regressions cover implicit/explicit receiver access and mutation on .NET.
The lowering is shared with the neoCLR CLI bridge; native neoCLR execution has
not been validated by these tests.


Local functions capturing only `self` use the lexical parent method's closure
frame. Source function symbols may be stored under their containing type, so
closure ownership must consult the enclosing method declaration before defining
the hidden closure parameter. Reassigning the frame after sibling methods have
been defined can pass an incompatible object layout to a generated call.


A lambda in a generic method can depend on method type parameters without
capturing a runtime value. Its carrier must still preserve that generic context:
CLI lambda signatures use the method closure's type parameters, not the original
method's generic slots. For closures nested inside generic types, method aliases
map to the trailing closure parameters after the enclosing type's parameters.


### Syntax port: captures and protected exits

The shared bound-tree walker visits tuple elements, so lambdas capture locals
used inside tuple projections. Iterator lowering rewrites address-taking for
hoisted locals and parameters to their state-machine fields; an `out` write and
a subsequent yield must address the same storage. Both target pipelines consume
these foundational fixes.

CLI loop branches use `leave` when crossing exception regions, preserving finally
execution. The persisted emitter also retains delegate constructor/Invoke
metadata definitions when a source generic parameter appears inside an array
argument. This adapter compensates for Reflection.Emit's definition recovery;
it does not replace the encoded Func/Action type or change language semantics.
The native target does not depend on this host reflection representation.
