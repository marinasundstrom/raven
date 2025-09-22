# Investigation: interface static members and default implementations

## Current symbol and emission shape

* Interface declarations are lowered to `SourceNamedTypeSymbol` instances with `TypeKind.Interface`, `System.Object` as the synthetic base, and the abstract flag forced to `true`, so downstream stages never treat them as concrete classes.【F:src/Raven.CodeAnalysis/Compilation.cs†L346-L375】
* During emission the `TypeGenerator` marks interface types with `TypeAttributes.Interface | TypeAttributes.Abstract`, registers any listed base interfaces, and then returns early—method handling is deferred entirely to the method generator.【F:src/Raven.CodeAnalysis/CodeGen/TypeGenerator.cs†L33-L87】

## Static interface members are not emitted

* The binder happily records `static` modifiers for interface members; the modifier pipeline is shared with classes and never blocks static interface methods.【F:src/Raven.CodeAnalysis/Binder/TypeMemberBinder.cs†L138-L254】
* `MethodGenerator.DefineMethodBuilder` recognizes interface methods but unconditionally adds the `Abstract | Virtual | NewSlot` flags and explicitly suppresses the `Static` flag when the containing type is an interface, so every interface method is emitted as an instance slot regardless of source modifiers.【F:src/Raven.CodeAnalysis/CodeGen/MethodGenerator.cs†L26-L92】

## Default interface bodies are discarded

* Method bodies inside interfaces still flow through the regular binder pipeline: a method block under a `MethodBinder` is upgraded to a `MethodBodyBinder`, which binds statements and caches the bound tree even when the containing type is an interface.【F:src/Raven.CodeAnalysis/Binder/BinderFactory.cs†L33-L55】【F:src/Raven.CodeAnalysis/Binder/MethodBodyBinder.cs†L8-L52】
* Despite that, `MethodGenerator.EmitBody` bails out as soon as it sees the containing type is an interface, so any bound body (including static members) is dropped during code generation.【F:src/Raven.CodeAnalysis/CodeGen/MethodGenerator.cs†L160-L169】

## Starting points for implementation

* Distinguish between interface members that declare a body and those that remain abstract so we only stamp the `Abstract` bit (and skip IL emission) for the latter. Static members fall into the "has body" bucket.
* Allow the method generator to set `MethodAttributes.Static` for static interface members and to emit IL for default bodies, while keeping abstract slots untouched.
* Once emission respects bodies, extend binding/diagnostics so interface modifiers (`abstract`, `override`, etc.) are validated under the new rules and update the language docs to document the supported feature set.
