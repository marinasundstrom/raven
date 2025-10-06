# System.Reflection2 Integration Guide

System.Reflection2 wraps `System.Reflection.Metadata` with familiar reflection types so the Raven compiler and its tests can interrogate metadata without loading assemblies into the runtime. This guide summarizes the common flows when working with persisted assemblies and highlights the differences from the BCL `System.Reflection.MetadataLoadContext`.

## Inspecting a `PersistedAssemblyBuilder`

1. Instantiate a `MetadataLoadContext` with an `IMetadataAssemblyResolver` that can provide reference assemblies. The `PathMetadataAssemblyResolver` shipped with System.Reflection2 handles simple name resolution from a set of search paths.
2. Call `PersistedAssemblyBuilderExtensions.ToMetadataAssembly` to serialize the in-memory metadata from a `PersistedAssemblyBuilder` directly into the load context. The extension keeps the PE image in memory so no temporary files are required.
3. Query types, members, and signatures through the metadata-backed reflection objects (`MetadataType`, `MetadataMethodInfo`, etc.) just like you would against runtime reflection. System.Reflection2 ensures common shape queries (generics, arrays, pointers, events, interface maps) all behave like their runtime equivalents.

This in-memory workflow contrasts with the BCL `MetadataLoadContext`, which expects callers to provide file-backed assemblies or pre-built `MetadataReader` instances. By staying entirely in memory System.Reflection2 is well suited for compiler pipelines and emit tests that generate transient IL.

## Opting into runtime invocation

System.Reflection2 defaults to a metadata-only experience—no invocation, handles, or runtime allocation occur. When you need to execute code, assign an `IMetadataRuntimeBridge` to `MetadataLoadContext.RuntimeBridge`. The built-in `RuntimeReflectionBridge` adapts metadata objects to their runtime counterparts and flows binder and culture information, so custom `Binder` implementations continue to participate in overload resolution and argument coercion. This makes it possible to bridge dynamic assemblies emitted at test time back into the runtime for verification.

## Unsupported scenarios

System.Reflection2 intentionally omits several runtime-only behaviors:

- Runtime handles (`TypeHandle`, `MethodHandle`, etc.) remain unavailable without a bridge because metadata objects have no execution backing.
- Attribute instantiation is not performed—`CustomAttributeData` exposes constructor and named arguments for inspection, but creating attribute instances is left to consumers.
- Advanced signature features such as custom modifiers and unmanaged calling conventions are decoded only at the metadata level today. Future work can extend the signature provider if consumers require parity with runtime reflection.

If your scenario requires any of these capabilities, plan to supply a runtime bridge or extend the metadata layer accordingly.

## Feature parity gap analysis

Bringing System.Reflection2 to full parity with runtime reflection requires additional work in a few focused areas:

- **Runtime handles and invocation paths** – Surfaces such as `MetadataMethodInfo.MethodHandle`, `MetadataFieldInfo.FieldHandle`, and the property getter/setter helpers throw `NotSupportedException` until a runtime bridge is configured, so callers cannot observe handles or invoke members without opting into `MetadataLoadContext.RuntimeBridge`.【F:src/System.Reflection2/System/Reflection2/MetadataMethodInfo.cs†L73-L116】【F:src/System.Reflection2/System/Reflection2/MetadataFieldInfo.cs†L37-L77】【F:src/System.Reflection2/System/Reflection2/MetadataPropertyInfo.cs†L122-L146】
- **Member-level modifiers and marshalling** – Parameter metadata already flows custom modifiers, but properties always return `Type.EmptyTypes` for the modifier queries and fields only decode constant values, so signature-level modifiers and marshalling descriptors (`MarshalAs`, `[In]`, `[Out]`, etc.) are still unavailable to consumers. Filling this gap will require decoding the field/property signatures alongside the existing constant logic.【F:src/System.Reflection2/System/Reflection2/MetadataPropertyInfo.cs†L119-L146】【F:src/System.Reflection2/System/Reflection2/MetadataFieldInfo.cs†L23-L103】【F:src/System.Reflection2/System/Reflection2/MetadataParameterInfo.cs†L70-L116】
- **Type identity helpers** – `MetadataType.GUID` currently returns `Guid.Empty`, and the implementation surfaces raw `TypeAttributes` without translating layout or COM metadata into the richer helpers that `System.Type` offers (for example, `Type.GUID`, `Type.GetDefaultMembers`, or `StructLayoutAttribute` projections). Additional decoding is needed to match runtime reflection in these scenarios.【F:src/System.Reflection2/System/Reflection2/MetadataType.cs†L108-L121】【F:src/System.Reflection2/System/Reflection2/MetadataType.cs†L500-L510】

Tracking these gaps clarifies where future engineering effort should focus to make System.Reflection2 a drop-in replacement for `System.Reflection` in the Raven compiler’s metadata stack.
