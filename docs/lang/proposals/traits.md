# Proposal: Traits through conforming extensions

> ℹ️ This proposal is under consideration

For the breaking alternative in which `trait` replaces `interface` throughout
Raven's source and semantic model, see
[A trait-oriented contract system](trait-oriented-type-system.md).

## Summary

Raven should support externally supplied conformance to an interface, but it
should build that feature from Raven's existing interface and extension models
rather than introduce a Rust-style `impl Trait for Type` construct.

The proposed source form is a **conforming extension**:

```raven
interface Printable {
    func print()
}

extension UriPrintable for System.Uri : Printable {
    func print() {
        Console.writeLine(self.toString())
    }
}
```

`Printable` remains an ordinary CLR interface. `System.Uri` is not changed and
does not report `Printable` in its CLR interface list. The named extension
establishes a Raven conformance that is available wherever that extension is
in scope.

This proposal deliberately does not add a `trait` keyword in its first version.
Raven already uses `interface` for open behavioral contracts, supports default
interface members, consumes CLR interfaces, and represents interface
constraints. Renaming that concept would create two source spellings for one
type-system concept without making external conformance easier to implement or
explain. Raven may describe interfaces with external conformances as providing
trait-like behavior, but the declaration spelling remains interoperable and
familiar.

## Motivation

Interfaces describe behavioral contracts, while extensions adapt the vocabulary
of a type that Raven does not own. A conforming extension combines those two
existing meanings:

* the interface defines the capability;
* the extension supplies or forwards the behavior for a receiver type; and
* imports determine where the conformance participates in lookup.

This supports retroactive modeling of existing .NET types without modifying
their metadata, deriving wrapper classes by hand, or making unrelated extension
members stand in for a named contract.

The feature is valuable only when the conformance is usable as a contract. If
it merely enables `value.member()` syntax, Raven's ordinary extensions already
solve the problem with less language and compiler machinery. The distinguishing
capabilities are:

* obtaining an interface view of a foreign value;
* passing that value to interface-oriented code through the selected
  conformance; and
* ultimately satisfying a generalized interface constraint in generic Raven
  code.

Source conformance and member lookup are therefore enabling implementation
stages, not by themselves the reason to adopt the feature.

## Goals

* Preserve ordinary CLR interface behavior for types that declare conformance
  in their base list.
* Permit a named extension to make an interface available for an existing type.
* Reuse Raven's existing import and extension-member lookup rules.
* Support interface defaults without requiring every conforming extension to
  repeat them.
* Keep resolution deterministic and independent of assembly-loading order.
* Allow Raven-aware compilers to discover exported conformances in metadata.
* Avoid allocating an adapter for ordinary extension-style member calls when a
  direct static lowering has the same observable behavior.

## Non-goals

The initial design does not add:

* Rust-style associated types, associated constants, specialization, negative
  implementations, or blanket coherence rules;
* stored state in interfaces or extensions;
* a distinct runtime kind beside CLR interfaces;
* mutation-preserving external conformance for value types;
* external conformance to interfaces with static abstract requirements; or
* automatic global activation of every conformance in every referenced
  assembly.

## Terminology

An **interface** is a CLR interface declared in Raven or imported from metadata.

A **nominal conformance** is an interface implementation listed on a class,
struct, record, or union declaration. It is represented in the emitted type's
CLR metadata.

An **external conformance** is a relationship declared by a conforming
extension between an interface and a receiver type. It does not alter the
receiver type's CLR metadata.

A **trait view** is an interface value backed by an external conformance. It is
an implementation detail unless a program explicitly converts or passes a
receiver as the interface type.

## Declarations

### Interfaces

Interface declarations retain their current syntax and semantics:

```raven
interface Printable {
    func display() -> string

    func print() {
        Console.writeLine(display())
    }
}
```

Methods and accessors without bodies are requirements. Implemented members are
defaults. Interface inheritance, variance, nested declarations, nominal
implementations, explicit interface implementations, and imported CLR
interfaces continue to use the existing rules.

No marker is required merely to distinguish a Raven interface from an imported
CLR interface. Both participate in the same contract model.

### Conforming extensions

A conforming extension adds an interface base list after its receiver type:

```raven
extension UriPrintable for System.Uri : Printable {
    func display() -> string => self.toString()
}
```

Multiple interfaces may be listed:

```raven
extension UriText for System.Uri : Printable, Formattable {
    // members
}
```

The base-list entries must resolve to interfaces. A conforming extension must
have an explicit identifier. This gives the conformance a stable import name,
diagnostic identity, and metadata identity. Existing anonymous extensions
remain valid when they do not declare conformance.

Generic conforming extensions use the existing extension type-parameter and
constraint syntax:

```raven
extension SequenceDisplay<T> for Sequence<T> : Printable
    where T: Formattable {
    func display() -> string {
        // ...
    }
}
```

The extension is applicable only when its receiver can be constructed and its
constraints are satisfied.

## Satisfying requirements

For each interface requirement, the compiler selects an implementation in this
order:

1. an accessible instance member already present on the receiver type with an
   exact interface-compatible signature;
2. a matching member declared in the conforming extension; or
3. an applicable default implementation inherited from the interface.

The selected member must obey the existing interface implementation rules for
parameter types, by-reference modifiers, return type, accessibility, and generic
arity.

If an ordinary receiver member already satisfies a requirement, declaring a
same-signature extension member is an error. Ordinary lookup would always pick
the receiver member, so allowing a different implementation only through an
interface view would make `value.member()` and the corresponding interface call
silently disagree.

All abstract requirements must be satisfied. Interface inheritance contributes
the inherited requirements, and conformance to a derived interface also makes
the corresponding base-interface views available.

The first implementation slice should support methods and computed properties.
An external conformance that requires an unsupported member kind, stored state,
or a static abstract member is rejected with a diagnostic rather than accepted
with incomplete behavior.

## Member lookup

Conforming-extension members use Raven's existing extension lookup:

```raven
import MyApp.UriPrintable.*

let uri = Uri("https://example.com")
uri.print()
```

Lookup proceeds as follows:

1. bind applicable ordinary instance members;
2. if no ordinary member is selected, bind ordinary in-scope extension members;
3. include interface default members made available by applicable in-scope
   conforming extensions; and
4. run normal applicability and overload-resolution rules over the resulting
   extension candidates.

A method supplied directly by a conforming extension is already an ordinary
extension candidate and does not need a second trait-specific lookup path. The
additional lookup is needed only for inherited and default interface members.

An imported conformance never outranks an ordinary receiver member. A
conformance also does not make every member of its interface globally available:
the conforming extension must be visible under the existing import rules.

## Visibility, selection, and conflicts

Conformance visibility follows extension-container visibility and imports:

* a `fileprivate` conforming extension is usable only in its source file;
* an `internal` conforming extension is usable within its assembly; and
* a `public` conforming extension may be discovered by referencing assemblies
  but participates only when its namespace or named container is imported.

Referencing an assembly alone does not activate its conformances.

Two libraries may declare conformance between the same receiver and interface.
This is allowed because neither library necessarily owns either type. If both
conforming extensions are visible and both are equally applicable at a use
site, lookup or conversion is ambiguous. The compiler must report the candidate
extension names and must never use reference order or assembly-loading order as
a tiebreaker.

Importing one named extension and not the other is the initial selection
mechanism. A future proposal may add an expression-level qualification syntax
for the uncommon case where two conformances must remain simultaneously in
scope. This proposal does not reserve Rust's coherence or orphan rules.

## Trait views and conversions

Nominal conformance uses the existing interface conversion. External
conformance requires a trait view because the receiver is not a CLR
implementation of the interface.

The initial design permits an explicit cast when exactly one applicable
conformance is visible:

```raven
let printable = uri :> Printable
printable.print()
```

The result is an ordinary `Printable` value at the CLR boundary. Reflection on
that value may observe a generated adapter type; reflection on `System.Uri`
still does not report that it implements `Printable`.

External trait-view conversion should be explicit in the first implementation.
It may allocate or box, can affect overload selection, and depends on imports.
Keeping it explicit prevents an imported library from silently adding a new
implicit conversion throughout a file. Direct `uri.print()` syntax remains
allocation-free when it lowers to the extension member itself.

If experience shows that explicit conversion is too cumbersome, a later
proposal may permit contextual conversion for a parameter or assignment whose
target is a single known interface type. Such a change should define conversion
ranking before it is implemented.

## Generic constraints

Existing constraints such as `T: Printable` are emitted as CLR interface
constraints. The CLR accepts such an instantiation only when `T` nominally
implements `Printable`; the existence of a generated adapter cannot make
`System.Uri` satisfy that metadata constraint.

Consequently, an external conformance must **not** be treated as satisfying an
ordinary CLR interface constraint in the first implementation. Doing so would
allow source programs that cannot be represented by their emitted generic
signature and whose constrained interface calls are invalid for `T`.

Generalized constraints remain a desired second-stage feature. Their viable
lowering is witness passing:

```text
source:  func print<T: Printable>(value: T)

conceptual Raven entry:
         print<T>(value: T, witness: PrintableWitness<T>)
```

The witness provides interface-member operations for `T`. A nominal witness
uses CLR constrained dispatch; an external witness uses the selected conforming
extension or adapter. The compiler can reuse one witness within the generic
body, so repeated calls do not repeatedly resolve or construct views.

This is not merely a lowering detail for public APIs. A hidden witness changes
the CLR signature and affects calls from other .NET languages. Before external
conformance is allowed to satisfy `T: Printable`, a follow-up ABI proposal must
choose and test one of these boundaries:

* expose a Raven witness-bearing entry point plus a conventional CLR-constrained
  bridge for nominal callers;
* expose only the witness-bearing ABI and document it as Raven-specific; or
* use a different source spelling for generalized conformance constraints and
  retain `T: Printable` as a strictly nominal CLR constraint.

The compiler must not silently specialize or monomorphize public generic
functions as the primary design; that would undermine separate compilation and
ordinary .NET consumption.

## Default implementations

Nominal implementations continue to use the existing interface default-member
semantics and CLR representation.

For an external conformance, a direct member supplied by the receiver or
extension is preferred. If the selected behavior is an interface default, the
compiler may:

* invoke the default through a generated trait view;
* synthesize a forwarding extension member; or
* lower to a helper with equivalent dispatch semantics.

The observable result must agree with a call through the interface view. The
lowering must preserve virtual calls made by the default member and must not
copy default bodies into each use site in a way that changes versioning
behavior.

## CLR and metadata representation

Nominal declarations are unchanged:

```raven
interface Printable {}
class Document : Printable {}
```

emit as an ordinary CLR interface and ordinary interface implementation.

An exported external conformance emits a compiler-generated adapter or witness
surface plus Raven metadata that identifies:

* the interface;
* the receiver type pattern;
* the named extension container;
* any generic parameter mapping and constraints;
* the requirement-to-member mappings; and
* the accessibility of the conformance.

Conceptually, a simple adapter may resemble:

```csharp
[RavenTraitImplementation(typeof(Printable), typeof(System.Uri))]
internal readonly struct UriPrintableView : Printable
{
    private readonly System.Uri value;

    public UriPrintableView(System.Uri value) => this.value = value;
    public void print() => UriPrintable.print(value);
}
```

The attribute name, adapter shape, and factory ABI are intentionally not fixed
by this source-language proposal. Generic receiver patterns cannot be described
correctly by two `System.Type` attribute arguments alone; their type-parameter
mapping must be recoverable from the generated generic signature or additional
metadata.

Raven's metadata importer should index conformance declarations by receiver and
interface without adding them to `INamedTypeSymbol.Interfaces` or
`AllInterfaces`. Those APIs describe nominal CLR facts and must remain truthful
for analyzers and reflection-oriented tools.

## Value types and mutation

A trait view of a value type normally contains a copy, just as boxing a struct
to an interface does. That is sound for read-only behavior but does not preserve
mutation of the caller's original storage.

The first implementation should reject an external conformance that requires a
mutating receiver operation for a value type. Supporting it later requires an
explicit by-reference receiver and lifetime design; a generated adapter with a
copied field is not an acceptable substitute.

## Semantic model

External conformance is a relationship, not a new base type of the receiver.
Compiler-owned semantic state should represent it separately with, at minimum:

* interface type;
* receiver type pattern;
* declaring extension container;
* visibility and applicability constraints; and
* resolved requirement implementations.

Binding and metadata import should own and cache this information. The language
server must consume authoritative semantic APIs rather than maintain a separate
conformance index.

`GetTypeInfo` for the receiver continues to report its real type.
`GetSymbolInfo` for a selected member reports the extension, interface default,
or adapter target actually selected by binding. Symbol display and hover should
identify externally supplied conformance without claiming that the receiver is
a nominal CLR implementation.

## Diagnostics

The feature requires focused diagnostics for:

* a non-interface entry in a conforming extension base list;
* an anonymous extension that declares conformance;
* unsatisfied or multiply satisfied interface requirements;
* a member whose signature does not match its requirement;
* a redundant conflicting extension member when the receiver already supplies
  the requirement;
* unsupported static abstract, storage, indexer, or event requirements in the
  initial implementation;
* ambiguous visible conformances;
* an unavailable or inaccessible conformance;
* an external conformance used as a CLR generic constraint; and
* a mutation-unsafe value-type conformance.

Diagnostics should name the interface, receiver type, and conforming extension
and should point to both candidates when ambiguity is local to source.

## Language-service behavior

Completion should offer members supplied by visible conforming extensions and
their interface defaults after ordinary members. The completion detail and
hover text should name the contributing interface and extension.

Go-to-definition from a supplied member should prefer the concrete extension or
receiver member. For a default member it should navigate to the interface
declaration. Find-references and rename must not treat the receiver type as
nominally implementing the interface.

Semantic tokens require no new declaration category in the initial design: the
contract remains an interface and the implementation remains an extension.

## Implementation shape

The feature should be implemented in independently reviewable stages.

### Stage 1: source conformance and member calls

1. Add an optional interface base list to `ExtensionDeclarationSyntax` between
   the receiver type and `where` clauses.
2. Bind and validate conformances using the existing interface implementation
   signature rules.
3. Include applicable interface defaults in extension lookup.
4. Keep direct calls lowered to existing static extension members where
   possible.
5. Add syntax, semantic-symbol, diagnostics, completion, and observable runtime
   tests.

This stage does not claim that an external conformance satisfies generic
constraints and does not require trait-view conversion.

Stage 1 should be treated as a compiler prototype or an explicitly incomplete
feature. Shipping it as the final surface would add little beyond checked
extension declarations.

### Stage 2: trait views and metadata discovery

1. Define the adapter/factory metadata ABI.
2. Emit and import public conformances across assemblies.
3. Bind explicit interface-view casts.
4. Test reflection shape, cross-assembly discovery, ambiguity, default dispatch,
   and allocation-sensitive direct-call paths.

### Stage 3: generalized constraints

1. Decide the public witness ABI and source spelling.
2. Add a bound conformance/witness representation owned by the compiler.
3. Lower generic member access through a reused witness.
4. Cover nominal and external witnesses, separate compilation, overloads,
   reflection, and calls from C#.

Stage 3 should be a separate acceptance decision. It is the part of the feature
that most strongly diverges from ordinary CLR interface semantics.

## Test plan

Focused coverage should include:

* parse shape, generic receivers, multiple interfaces, and recovery around the
  extension base list;
* requirement matching, defaults, inheritance, constraints, and diagnostics;
* precedence of receiver members over conforming extensions;
* ambiguity controlled by imports rather than reference order;
* direct calls on reference and value receivers;
* explicit view conversion and reflection shape when Stage 2 is implemented;
* source-to-metadata round trips for public generic and non-generic
  conformances;
* semantic-model consistency across `GetTypeInfo`, `GetSymbolInfo`, completion,
  and hover; and
* incremental edits that add or remove a base interface or requirement member.

Tests should assert diagnostics, symbols, metadata shape, and observable runtime
behavior rather than generated adapter names or exact emitted instructions.

## Alternatives considered

### Add `trait` and `impl Trait for Type`

This makes the feature look familiar to Rust users but duplicates Raven's
existing interface and extension concepts. It also suggests Rust coherence,
method-import, and generic-monomorphization assumptions that do not naturally
hold on the CLR. Reusing conforming extensions gives the feature Raven's own
composition model and a natural visibility boundary.

### Rename every source interface to `trait`

This is mechanically possible while Raven is experimental, but it provides no
new semantics: imported .NET interfaces, default members, nominal
implementations, and CLR constraints would still be interfaces at the runtime
boundary. The rename can be reconsidered as a separate language-vocabulary
decision after external conformance proves useful.

### Treat conforming extensions as globally coherent

An orphan rule would prevent important scenarios where Raven owns neither the
interface nor the target type. A process-wide winner would be nondeterministic.
Named, import-scoped conformances preserve retroactive modeling while keeping
selection lexical and reproducible.

### Use ordinary extensions only

Ordinary extensions already provide member syntax and are sufficient when no
generic or conversion-level contract is needed. They cannot express that a set
of members satisfies one named interface, inherit defaults as a unit, or create
an interface view. Conforming extensions add exactly those semantics.

## Recommendation

Accept the **conforming extension** model as the design direction only if Raven
wants retroactive conformance to be usable by interface-oriented and generic
code. That capability is meaningful in Raven: it joins the language's existing
notions of an open behavioral contract and scoped vocabulary adaptation without
requiring ownership or inheritance.

Stage 1 is a natural additive prototype in Raven's current compiler
architecture, but it is not a sufficient user-facing feature. Stage 2 provides
useful interface-oriented composition. Stage 3 supplies the strongest reason
to call the feature trait-like rather than a checked extension. Raven should
choose the witness-bearing generic ABI before committing to the full feature,
even if implementation remains staged afterward.

Do not add the `trait` keyword or claim external satisfaction of `T: Interface`
as part of the first implementation. If Raven does not want witness-based
generic semantics, keep ordinary interfaces and extensions separate instead of
introducing a reduced trait concept.
