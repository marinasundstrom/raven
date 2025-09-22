# Investigation: explicit base constructor invocation

## Current behavior

* The emitter always injects a call to the base type's *parameterless* constructor for every ordinary (non-named) instance constructor. It loads `self`, resolves the base constructor with zero parameters, and emits a direct `call`.【F:src/Raven.CodeAnalysis/CodeGen/MethodBodyGenerator.cs†L109-L126】【F:src/Raven.CodeAnalysis/CodeGen/MethodBodyGenerator.cs†L382-L395】
* Existing tests only cover the implicit chaining path: derived constructors without an initializer rely on the default base call, and succeed as long as the base constructor takes no parameters.【F:test/Raven.CodeAnalysis.Tests/CodeGen/BaseConstructorTests.cs†L11-L82】
* The inheritance proposal documents that explicit base constructor invocation is still pending work, matching the current implementation gap.【F:docs/lang/proposals/class-inheritance.md†L31-L52】

## Missing syntax support

* The syntax model for constructors does not expose any slot for an initializer expression. Both `BaseConstructorDeclaration` and its derived nodes stop at the body/expression body/terminator slots, leaving no way to represent `: base(...)` or similar forms.【F:src/Raven.CodeAnalysis/Syntax/Model.xml†L153-L173】
* The parser mirrors this limitation: after reading the parameter list it looks only for a block body, an expression-bodied arrow, or a terminator. There is no branch that consumes a constructor initializer token sequence.【F:src/Raven.CodeAnalysis/Syntax/InternalSyntax/Parser/Parsers/TypeDeclarationParser.cs†L192-L250】
* No lexical entry exists for a `base` keyword (or alternative syntax) in `Tokens.xml`, so even the token stream cannot distinguish an initializer clause today. Introducing an initializer will require adding the appropriate keyword or contextual token alongside the parser changes.【F:src/Raven.CodeAnalysis/Syntax/Tokens.xml†L4-L79】

## Semantic gaps

* Constructor symbols are created without inspecting any initializer. `BindConstructorDeclaration` collects parameters, checks for duplicate signatures, and stops. There is no storage location for a chosen base constructor or the bound argument list that an initializer would produce.【F:src/Raven.CodeAnalysis/Binder/TypeMemberBinder.cs†L257-L307】
* The IL generator assumes that the parameterless base call is always needed and never consults the syntax/bound tree for an explicit initializer, so adding the syntax alone would still emit the implicit call first and then any user-specified call, resulting in duplicate chaining or incorrect argument passing.【F:src/Raven.CodeAnalysis/CodeGen/MethodBodyGenerator.cs†L109-L126】
* Because no binding occurs, there are no diagnostics for invalid initializer scenarios (missing accessible base constructor, wrong argument counts, use in static constructors, etc.), and the runtime fallback throws `NotSupportedException` when the base type has no parameterless constructor.【F:src/Raven.CodeAnalysis/CodeGen/MethodBodyGenerator.cs†L382-L386】

## Strategy and task breakdown

Implementing explicit chaining requires a staged plan so we can land incremental PRs without breaking existing behavior. Each step below is a self-contained task that can be tracked independently.

## Progress update

The first three slices of the plan have landed in the minimal implementation:

* ✅ **Task 1 — Syntax & tokens** introduced the `base` keyword, initializer syntax nodes, and parser support.
* ✅ **Task 2 — Minimal binding pipeline** binds `: base(...)` clauses for instance constructors and stores the result on `SourceMethodSymbol`.
* ✅ **Task 3 — Code generation integration** reuses the bound initializer when emitting constructor bodies and falls back to the implicit parameterless call when none exists.

The remaining effort now concentrates on validation, diagnostics, and specification updates. The Task&nbsp;4a diagnostic slices are complete, and Task&nbsp;4 now continues as a set of smaller follow-ups so we can land them independently:

* ✅ **Task 4a.1 — Static constructor guard**: report `RAV0312` when a static constructor specifies `: base(...)`.
* ✅ **Task 4a.2 — Preserve user intent on errors**: bail out of emission when initializer binding reports argument diagnostics so we skip the legacy implicit base call instead of chaining to the wrong overload.
* ✅ **Task 4a.3 — Missing/ambiguous base diagnostics**: surface binder diagnostics for `: base(...)` clauses that fail overload resolution and reuse arity-matched candidates to produce conversion errors instead of falling back to `RAV1501`.
* ✅ **Task 4b — Negative coverage and regression tests**: covered `RAV0312`, `RAV1501`, and `RAV1503` with new semantic tests and added a positive codegen regression proving argument forwarding executes the base constructor exactly once.
* 🔄 **Task 4c — Documentation refresh** *(immediate)*: update the language specification and proposals with the supported initializer syntax once validation hardens.

### Task 1 — Syntax & tokens

* Introduce the `base` keyword (or chosen initializer keyword) in `Tokens.xml` and regenerate the lexer tables.
* Add a `ConstructorInitializerSyntax` node (or equivalent slots) to `Model.xml`, carrying the colon token, `base` keyword, and argument list; regenerate the syntax layer.
* Update `TypeDeclarationParser.ParseConstructorDeclaration` so it parses an optional initializer between the parameter list and the body/expression body.
* Acceptance criteria: constructor declarations with `: base(...)` round-trip through parsing/pretty-printing; named constructors remain unaffected.

### Task 2 — Minimal binding pipeline

* Extend constructor binding to visit an initializer clause, rejecting it for static or named constructors.
* Reuse `BlockBinder.BindConstructorInvocation` (or similar helper) to resolve the chosen base constructor and perform argument conversions.
* Store the resolved constructor symbol and converted arguments on `SourceMethodSymbol` (or an attached structure) for downstream consumers.
* Acceptance criteria: successful binding of `: base(...)` in instance constructors; diagnostics for missing/inaccessible base constructors are deferred to a later pass if necessary.

### Task 3 — Code generation integration

* Teach `MethodBodyGenerator` to detect a bound initializer and emit that call instead of the hard-coded parameterless base constructor.
* Ensure the generator loads `self` before forwarding the call and respects the binder-selected constructor and argument order.
* Keep the existing implicit call path for constructors without an initializer.
* Acceptance criteria: derived constructors invoking `: base(arg)` execute the base constructor exactly once; existing tests relying on implicit chaining continue to pass.

### Task 4 — Validation & tests

* Expand `BaseConstructorTests` to cover explicit base invocations, including argument passing and minimal diagnostic coverage (e.g., wrong argument count).
* Add binder-level tests if necessary to assert diagnostics and symbol information.
* Update documentation/spec proposals once the feature shape solidifies.
* Acceptance criteria: new tests demonstrate successful chaining and at least one representative failure mode; documentation is refreshed.

### Deferred follow-ups

* Enforce full diagnostic coverage (accessibility, duplicate initializers, disallowed contexts) once the minimal path is stable.
* Consider caching or exposing the bound initializer to semantic model APIs.
* Evaluate interactions with future features (e.g., struct constructors, implicit `this(...)` chaining) after basic support ships.

With these steps, we can ship a minimal explicit base constructor call and expand coverage in future tasks without entangling named constructors.
