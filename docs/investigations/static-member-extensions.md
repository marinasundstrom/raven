# Static member extensions investigation

## Goal
Capture the design space for supporting static extension members comparable to C#, where static members declared in an `extension` container can be imported and invoked via the target type name (`Foo.Bar(...)`). This doc focuses on how Raven's existing extension infrastructure could evolve to support static methods, properties, and (eventually) operators without breaking current instance-style semantics.

## Current state and gaps
- The language specification restricts extension containers to instance-style members: every member is given an implicit `self` parameter, and modifiers like `static` are rejected entirely.【F:docs/lang/spec/language-specification.md†L633-L709】
- Extension lookup today triggers only after instance lookup fails for an expression receiver. The binder gathers extension methods/properties whose synthesized `self` parameter matches the receiver value, then lowers the invocation/property access into a static call that passes the receiver as the first argument.【F:docs/lang/spec/language-specification.md†L667-L709】
- Metadata emission for extensions relies on ordinary static methods marked with `ExtensionAttribute`. Because the syntax does not allow `static` declarations, there is no representation for type-qualified extension calls (for example, `Foo.StaticHelper()` or `Foo.StaticProperty`).

## Design constraints
- Preserve source/metadata parity: static extension members declared in Raven should be callable from metadata consumers (e.g., C#) and vice versa when possible. Emitted IL should remain a static method on the generated extension container type to minimize runtime changes.
- Avoid ambiguity with existing instance extensions. Static extension lookup should not shadow real static members on the receiver type; instead it should follow the same "use extensions only when normal lookup fails" rule used for instance receivers.
- Imports should continue to gate visibility. Both `import Namespace.*` and `import Namespace.ExtensionContainer` should surface static extension candidates; without an import they remain hidden.

## Candidate semantics
1. **Syntax and containers**
   - Permit `public static` function and property members inside `extension` containers. The receiver type in the `for` clause remains the target, but no synthesized `self` parameter is created for static members. Static accessibility mirrors the existing default (`public` unless `internal` is specified).
   - Preserve existing restrictions for instance members (no `protected`/`private`/`static`). Mixed containers (instance + static members) remain valid, with lookup rules driven by the member modifier.

2. **Binding and overload resolution**
   - Extend member lookup when the receiver is a **type** (e.g., `Foo.M(...)` or `Foo.Prop`) and ordinary static lookup fails. At that point the binder should gather imported static extension members whose receiver type matches the target type symbol (respecting type arguments and constraints) and run overload resolution alongside any remaining candidates.
   - For open generic receivers, surface static extensions only when all type arguments are known at the use site; the binder should substitute the concrete type arguments to validate constraints before adding a candidate.
   - Ambiguity rules mirror instance extensions: if both a real static member and a static extension are applicable, the real member wins; ties between multiple extensions produce the usual overload ambiguity diagnostics.

3. **Lowering and emission**
   - Lower successful static extension calls into direct static calls on the generated extension container type. Because no `self` parameter exists, the lowered argument list is identical to the source call (unlike instance extensions that prepend the receiver).
   - Properties lower to accessor calls on the extension container (`get_`/`set_`), mirroring existing extension property lowering but without an injected receiver argument.
   - Continue emitting `ExtensionAttribute` for discoverability across languages, even though no `this` parameter exists; this matches C# behavior for static extensions and keeps metadata consumers aligned.

4. **Diagnostics and tooling**
   - Disallow `static` extension members whose signatures reference an implicit `self` (e.g., `public static M() -> int { return self.Field; }`) by treating `self` as undefined in static bodies. The parser or binder should surface a clear diagnostic when `self` is used in a static extension.
   - Report a dedicated diagnostic when a static extension import exists but the receiver type is missing required type arguments or violates constraints.
   - Update quick info/signature help to distinguish between instance extensions (with `self`) and static extensions (type-qualified) so tooling can present accurate invocation shapes.

## Implementation considerations
- **Symbol model:** Introduce a marker on extension symbols indicating whether they are static or instance-driven so binding can branch without duplicating symbol types. Ensure metadata decoding preserves this flag when reading external assemblies.
- **Lookup tables:** Namespace/type import binding currently indexes extensions by receiver type for instance lookups. Add a parallel index for static extension members keyed by the receiver type symbol to avoid polluting instance lookup and to keep search costs predictable.
- **Name resolution with aliases:** When a type is imported via an alias, static extension lookup should respect the alias the same way normal static lookup does, so `import FooAlias = Namespace.Foo` followed by `FooAlias.Ext()` works.
- **Specification updates:** The language specification must be updated to lift the `static` restriction and document the binding/lowering flow for type-qualified extension calls. Grammar changes are likely unnecessary if member declarations already support the `static` modifier.

## Interaction with extension operators
Operator overloading via extensions (including conversions and indexers) is tracked separately in the draft proposal for static extension members.【F:docs/lang/proposals/drafts/static-extension-members.md†L17-L33】 The operator surface should align with the static extension lookup rules above so that operators declared in an extension container participate in operator resolution when the operand or target type matches the receiver.

## Open questions
- Should static extension members participate in lookup for the nullable form of the receiver type (e.g., `Foo?`), or should they require an exact non-nullable match to avoid surprising matches?
- How should static extension imports interact with nested types? For example, should `import Outer.NestedExtensions` expose static extensions for `Outer.Nested` without importing `Outer` itself?
- Do we need additional attributes or metadata markers to help tools distinguish static extensions from ordinary static helpers beyond `ExtensionAttribute`?
