# Proposal: A trait-oriented contract system

> ℹ️ This proposal is under consideration

## Summary

This proposal redesigns Raven's behavioral-contract model around **traits**.
`trait` becomes the only source-language declaration for an open behavioral
contract, and Raven projects every imported CLR interface as a trait.

```raven
trait Printable {
    func display() -> string

    func print() {
        Console.writeLine(display())
    }
}

class Document : Printable {
    func display() -> string => "Document"
}
```

Nominal trait implementation remains ordinary CLR interface implementation.
Raven additionally permits a named extension to implement traits for a type
whose declaration cannot be changed:

```raven
extension UriPrintable for System.Uri : Printable {
    func display() -> string => self.toString()
}
```

The source model is trait-oriented even though the CLR representation remains
interface-oriented. A trait constraint means that Raven can provide an
applicable conformance, whether nominal or external. A trait used as a value
type denotes a runtime trait view.

This is an alternative to [Traits through conforming extensions](traits.md).
That proposal preserves `interface` as the source and semantic center. This
proposal intentionally makes a broader, breaking language choice.

## Why redesign instead of rename

Replacing the token `interface` with `trait` while retaining exactly the same
semantics would be vocabulary churn. A trait-oriented redesign is meaningful
only if the language model consistently includes:

* nominal and external conformance;
* conformance-based generic constraints;
* trait views for values crossing dynamic or .NET boundaries;
* reusable defaults dispatched through the selected conformance; and
* compiler APIs and diagnostics that distinguish Raven conformance from CLR
  metadata implementation.

The design therefore starts from Raven semantics and treats CLR interfaces,
adapters, and witness parameters as representations of those semantics.

## Design principles

### Traits describe capabilities

A trait names behavior that generic code or a runtime view may require. It is
not merely a collection of extension methods and does not own per-instance
state.

### Nominal conformance remains the common path

When Raven controls a type, declaring the trait on the type is direct,
allocation-free, and fully interoperable with .NET. External conformance exists
for adaptation, not as the preferred implementation style for owned types.

### Conformance is lexical

External conformances are named and imported. Referencing an assembly does not
globally activate its conformances. A program's meaning must not depend on
assembly enumeration or load order.

### Source semantics do not depend on one lowering

Raven may use direct interface dispatch, static extension calls, adapters, or
witness dictionaries. Those choices must preserve the same selected
conformance and observable behavior.

### The .NET boundary remains honest

Raven does not claim that an externally conformed type implements a CLR
interface. Reflection and compiler APIs must distinguish nominal CLR facts from
Raven conformance facts.

## Syntax

### Trait declarations

`trait` replaces `interface` in Raven source:

```raven
trait Named {
    name: string
}

trait Printable {
    func print()
}
```

Traits may be declared at namespace or nested type scope wherever interfaces
are currently permitted. They use the existing accessibility, attribute,
generic-parameter, variance, and constraint syntax.

The `interface` keyword is not a second declaration form. During migration the
compiler may recognize it only to produce a targeted diagnostic and code fix:
"Raven interfaces are declared with `trait`."

### Trait inheritance

Traits extend other traits with the existing base-list syntax:

```raven
trait Formattable {
    func format() -> string
}

trait Printable : Formattable {
    func print() {
        Console.writeLine(format())
    }
}
```

Trait inheritance is transitive. A conformance to `Printable` must satisfy the
requirements inherited from `Formattable` and provides a `Formattable` view.

### Nominal conformance

Classes, structs, records, and union carriers list traits in their base list:

```raven
class Person : Printable {
    func print() {
        Console.writeLine("Person")
    }
}
```

For class-like declarations, the optional base class remains first and is
followed by traits. Other type declarations accept traits according to their
existing CLR implementation rules.

An explicitly qualified implementation uses trait terminology but retains the
current member syntax:

```raven
class QuietLogger : Logger {
    func Logger.log(message: string) {
        Console.writeLine("[quiet] " + message)
    }
}
```

The qualified member is accessible through a `Logger` view rather than as an
ordinary member of `QuietLogger`.

### External conformance

A named extension may list one or more traits after its receiver:

```raven
extension UriText for System.Uri : Printable, Formattable {
    func format() -> string => self.toString()
}
```

This spelling is deliberately based on Raven's extension construct rather than
Rust's `impl Trait for Type` form. The extension means that behavior is being
adapted onto a receiver. Its trait list states which contracts that behavior
satisfies.

Conforming extensions must be named. The name is used for imports, ambiguity
diagnostics, metadata identity, and explicit selection if selection syntax is
added later. Anonymous extensions remain available for unconstrained helper
members.

Generic and conditional conformances use existing extension constraints:

```raven
extension PrintableList<T> for List<T> : Printable
    where T: Printable {
    func print() {
        for item in self {
            item.print()
        }
    }
}
```

An external conformance is applicable only if its receiver pattern matches and
all of its constraints can themselves be satisfied.

## Trait members

### Requirements and defaults

An instance member without a body is a requirement. A member with a body is a
default:

```raven
trait Equatable<T> {
    func equals(other: T) -> bool

    func notEquals(other: T) -> bool {
        !equals(other)
    }
}
```

Requirements may initially be methods and properties. Indexers and events
should be admitted only when conforming extensions can express and lower them
without hidden state.

Static trait members are meaningful only through a trait name or constrained
type parameter. Imported static abstract CLR interface members are projected as
static trait requirements. A static member in a conforming extension may
satisfy such a requirement:

```raven
trait Parseable<T> {
    static func parse(text: string) -> T
}

extension UriParsing for Uri : Parseable<Uri> {
    static func parse(text: string) -> Uri => Uri(text)
}
```

Static requirements are not callable through a runtime trait view.

### No instance state

Traits cannot declare stored properties or fields. Conforming extensions retain
the current extension rule that properties are computed and no receiver storage
is introduced.

Defaults may use other trait members. Their meaning is expressed in terms of
the active conformance, not a particular adapter layout.

### Requirement matching

For nominal conformance, the existing interface implementation rules continue
to match accessible members and explicitly qualified implementations.

For external conformance, a requirement is satisfied in this order:

1. a matching member declared in the conforming extension;
2. an accessible matching ordinary member on the receiver type; or
3. a default supplied by the trait or its base traits.

Placing the extension member first permits intentional adaptation, including
normalizing a foreign API whose existing member has unsuitable semantics. This
creates a deliberate distinction:

* an ordinary call on the concrete receiver still prefers the receiver's real
  member; and
* a call made through the trait conformance uses the requirement mapping.

To make that distinction visible, hover and diagnostics identify calls that are
dispatched through a conformance. If Raven prefers concrete-call and trait-call
behavior always to agree, it may instead reject an extension requirement that
duplicates a receiver member; that is a policy decision to settle before
implementation.

## Conformance lookup

### Nominal conformance

A nominal implementation is always available and always preferred. An external
conformance cannot replace or shadow a nominal implementation of the same trait
for the same receiver type.

This preserves ordinary .NET meaning and prevents an import from changing
interface dispatch for a type that already implements the interface.

### External conformance visibility

External conformance follows the visibility and import rules of its named
extension:

* `fileprivate` is visible only in its file;
* `internal` is visible within its assembly; and
* `public` is discoverable from referenced assemblies but active only when its
  namespace or named extension container is imported.

The compiler resolves conformances from the lexical binder chain and its
imports. It does not scan all referenced assemblies at each use site or keep
language-service-only semantic caches.

### Ambiguity

If two equally applicable external conformances are visible, the conformance is
ambiguous. Raven reports both named extensions and does not use source order,
import order, reference order, or assembly-load order as a tiebreaker.

The initial disambiguation mechanism is lexical: import only the desired named
extension. Before supporting code that needs two same-pair conformances in one
scope, Raven should design explicit qualification rather than infer a winner.

### No orphan rule

Raven does not require the declaring assembly to own either the trait or target
type. Such a rule would defeat the primary .NET adaptation scenario. Named,
scoped conformances provide determinism without imposing global uniqueness.

## Member lookup and calls

Given:

```raven
value.print()
```

Raven considers candidates in this order:

1. applicable ordinary members on the static receiver type;
2. applicable ordinary imported extension members; and
3. members made available through visible trait conformances, including trait
   defaults.

Ordinary members therefore retain precedence. Trait candidates participate in
normal overload resolution within their lookup tier. Two unrelated traits that
provide equally applicable members produce an ambiguity diagnostic.

After selection, the bound call records both the requirement and selected
conformance. Lowering may call a nominal interface member, a static extension
member, a default helper, or a witness operation. Lookup is not repeated during
lowering.

## Trait constraints

### Source semantics

A constraint means that the compiler must resolve a conformance:

```raven
func print<T: Printable>(value: T) {
    value.print()
}
```

The constraint is satisfied by:

* a nominal implementation of `Printable` by `T`;
* a unique applicable external conformance visible at the call site; or
* a conformance constraint already carried by another type parameter.

Inside the function, the selected conformance is stable. Member calls and
further constrained calls reuse it rather than performing lexical lookup again.

This is the central semantic difference from a renamed CLR interface. Raven's
constraint is a capability requirement, not necessarily a CLR generic
constraint.

### Witness model

Conceptually, each generalized trait constraint supplies a witness:

```text
PrintableWitness<T>
    display(value: T) -> string
    print(value: T) -> unit
```

A nominal witness uses constrained CLR interface dispatch. An external witness
uses the requirement mappings of the selected extension. Defaults receive the
same witness, allowing one default member to call another requirement without
materializing an interface adapter.

Witnesses are immutable compiler-owned semantic values. A generic binder owns
the witness parameters for its constraints, and bound calls refer to those
parameters. Public semantic APIs expose selected conformance information
without exposing incremental cache mechanics.

### Public ABI

Witness passing affects emitted signatures and must be an explicit part of
Raven's interoperability design. A public trait-constrained function should
conceptually emit:

1. a Raven entry point whose signature carries the witnesses required by the
   source contract; and
2. when all constraints have a CLR interface representation, a conventional
   CLR-constrained bridge for nominal .NET callers.

The bridge constructs nominal witnesses and forwards to the Raven entry point.
Raven metadata maps both emitted methods back to one source symbol. C# callers
can use the conventional bridge with nominal implementers; Raven callers can
use either nominal or external conformance.

The exact names, accessibility, attributes, and signature encoding belong in a
separate ABI specification, but the two-entry model is the default direction of
this proposal. Public APIs must not rely on whole-program monomorphization.

## Trait types and views

Using a trait in a value-type position denotes a runtime value that supports the
trait:

```raven
func printNow(value: Printable) {
    value.print()
}
```

For a nominal implementation, the value uses the ordinary CLR interface
conversion. For an external implementation, Raven creates a trait view backed
by the selected conformance.

When exactly one conformance is available, conversion to a known trait target
is implicit:

```raven
import MyApp.UriPrintable.*

let uri = Uri("https://example.com")
printNow(uri)
```

An external trait-view conversion ranks below identity, inheritance, and
ordinary CLR interface conversions. If it would compete with another nonstandard
conversion without a unique best result, overload resolution reports ambiguity.

An explicit cast remains available when the programmer wants to state the
boundary:

```raven
let printable = uri :> Printable
```

The trait view is an ordinary CLR interface value at a .NET boundary. It may
box or allocate. Direct generic witness calls and direct extension-backed calls
should not materialize a view when no interface value is required.

### Value types

A value-type trait view contains or boxes a copy under ordinary CLR semantics.
Nonmutating behavior is therefore straightforward. A conformance whose methods
must mutate the caller's original variable requires by-reference witness and
lifetime semantics and is deferred until Raven defines that model.

The compiler must diagnose unsupported mutation rather than silently mutate an
adapter copy.

## Defaults and dispatch

Trait defaults conceptually lower to generic helpers over the trait witness:

```text
PrintableDefaults.print<T>(value: T, witness: PrintableWitness<T>)
```

This gives defaults one semantic implementation across nominal and external
conformance. For CLR interoperability, a source trait may additionally emit a
default interface member that forwards through nominal dispatch when the body
can be represented faithfully.

The compiler may optimize a default call to ordinary interface dispatch or a
direct helper call, but it must preserve:

* the selected conformance;
* virtual dispatch of requirements called by the default;
* versioning behavior across separately compiled Raven assemblies; and
* receiver value/reference semantics.

## Imported CLR interfaces

Every imported CLR interface is projected as a Raven trait:

```raven
IDisposable
IEnumerable<string>
IComparable<int>
```

The `I` prefix is part of the metadata name and has no Raven semantic meaning.
Documentation, hover, and symbol display describe these types as traits while
retaining their actual metadata names.

Nominal CLR implementations become nominal Raven conformances. Default
interface members become trait defaults. Generic variance, interface
inheritance, explicit implementations, static abstract members, and generic
constraints are projected into their corresponding trait semantics.

Raven may declare an external conformance to an imported interface. That does
not modify the target's metadata, and a C# generic constrained to that interface
still cannot accept the target directly. Raven's witness-bearing entry points
are what make the external conformance usable by Raven generic code.

## CLR representation

### Trait declarations

A Raven trait emits as a CLR interface. A Raven marker attribute records its
source origin and ABI version for trait-aware tooling:

```csharp
[RavenTrait(AbiVersion = 1)]
public interface Printable
{
    string display();
    void print();
}
```

The marker does not create a different runtime type kind. A compiler that does
not understand it treats the declaration as an ordinary interface.

Imported unmarked CLR interfaces are still traits in Raven. The marker exists
for source round-tripping and Raven-specific witness/default metadata, not for
eligibility.

### Nominal conformance

A nominal implementation emits an ordinary interface implementation and may
also provide compiler-generated witness metadata. Reflection reports the
interface normally.

### External conformance

An exported conforming extension emits discoverable Raven metadata containing:

* the trait and receiver type pattern;
* the extension identity and accessibility;
* generic parameter mapping and applicability constraints;
* requirement-to-member mappings;
* default-member participation; and
* the witness or view factory entry points.

The representation may include a generated adapter implementing the CLR
interface and a witness type optimized for generic dispatch. One representation
need not serve both purposes.

Generic receiver patterns cannot be fully encoded as two `System.Type`
attribute arguments. The ABI must preserve their constructed signatures and
type-parameter relationships in metadata.

## Reflection and compiler APIs

Normal reflection exposes the CLR representation:

* a trait appears as an interface;
* nominal conformance appears in `Type.GetInterfaces()`; and
* external conformance does not appear on the target type.

Trait-aware Raven APIs expose both semantic and CLR facts. In a full redesign:

* `TypeKind.Trait` replaces source-facing `TypeKind.Interface`;
* `INamedTypeSymbol.Traits` and `AllTraits` describe nominal trait
  implementation;
* external relationships are returned as conformance symbols rather than added
  to the target's nominal trait list; and
* an explicit interop API exposes whether the underlying metadata type is a CLR
  interface.

Compatibility aliases for `Interfaces`, `AllInterfaces`, and
`TypeKind.Interface` may be retained temporarily for compiler clients, but new
Raven tooling should use trait terminology. This is an intentional divergence
from Roslyn justified by the source-language redesign.

At minimum, a conformance symbol reports its trait, receiver pattern, declaring
extension, constraints, accessibility, origin, and requirement mappings.
`GetSymbolInfo` on a trait-dispatched call reports both the requirement and
selected conformance. `GetTypeInfo` on the original receiver continues to
report its actual concrete type.

## Extensions and traits

Extensions remain useful without conformance:

```raven
extension UriHelpers for Uri {
    func hostName() -> string => self.Host
}
```

This adds vocabulary but does not establish a capability. Generic code cannot
constrain on the existence of `hostName`.

A conforming extension establishes a named capability and must satisfy all
requirements:

```raven
extension UriPrintable for Uri : Printable {
    func display() -> string => self.toString()
}
```

A conforming extension may also contain convenience members not required by the
trait. Those members remain ordinary extensions and are not available through a
trait constraint or trait view.

## Coherence and evolution

Conformance is resolved from a compilation snapshot and lexical scope. Adding a
new imported conformance can make a previously invalid call valid or make a
previously unique external conformance ambiguous. It cannot override a nominal
conformance or alter code in a separately compiled method body.

Public library guidance should prefer:

* nominal conformance when the library owns the type;
* external conformance when adapting foreign types to a meaningful domain or
  framework capability; and
* ordinary extensions for conveniences that do not represent a contract.

Libraries should avoid exporting broad conditional conformances whose
applicability overlaps common conformances from other packages. Raven should
surface conformance origin prominently in completion and diagnostics.

## Diagnostics

The redesign requires diagnostics for:

* use of the retired `interface` keyword;
* a non-trait in a trait base list or conformance list;
* missing, inaccessible, or multiply mapped requirements;
* invalid receiver or generic constraint mappings;
* an external conformance targeting an existing nominal conformance;
* ambiguous visible external conformances;
* unsatisfied trait constraints;
* ambiguous trait-view conversions;
* unsupported value-type mutation through a conformance; and
* trait features that cannot be represented at the selected .NET boundary.

Diagnostics use trait terminology even when the trait originated as a CLR
interface. Interop-oriented details may additionally identify the metadata
interface name.

## Language-service behavior

Completion offers ordinary members first, then extensions, then visible trait
members. The item detail names the contributing trait and conformance.

Hover distinguishes:

* `trait Printable` for the declaration;
* nominal conformance by the receiver type; and
* external conformance through a named extension.

Go-to-definition from a requirement call navigates to the concrete mapped
member when one exists and offers the trait requirement as an alternate target.
Default calls navigate to the trait body. Find-references and rename operate on
requirement mappings without pretending the target metadata was modified.

Semantic tokens classify traits using the existing interface token category at
the LSP boundary unless the protocol later standardizes a separate trait kind.
The VS Code grammar and console highlighter recognize `trait` and stop treating
`interface` as a current declaration keyword.

## Migration

This is a breaking source and compiler-API redesign.

1. Replace Raven `interface` declarations with `trait`.
2. Update diagnostics, pretty-printing, documentation, samples, tests, TextMate
   grammar, and language-service labels.
3. Continue consuming CLR interface metadata without source migration.
4. Offer a syntax code fix for the keyword replacement.
5. Version Raven trait and witness metadata before external conformances ship
   across assembly boundaries.

The compiler may provide one transition release in which `interface` parses
with a deprecation diagnostic, but the specification should have only one
canonical term.

## Implementation plan

The implementation must be vertical rather than shipping a keyword rename as a
standalone feature.

### Phase 0: ABI proof

Before changing source syntax, prototype:

* one trait witness for a nominal conformance;
* one witness for an external conformance;
* a generic function calling a requirement and a default;
* an interface-view adapter; and
* a public nominal CLR bridge callable from C#.

This prototype establishes whether the semantic model can be represented with
acceptable code size, dispatch, and interoperability.

### Phase 1: trait terminology and nominal behavior

* replace interface syntax/model nodes with trait equivalents and regenerate;
* project imported CLR interfaces as traits;
* update symbols, diagnostics, display, docs, grammar, and language services;
* retain ordinary CLR emission for nominal implementations; and
* migrate existing interface tests to trait terminology without weakening their
  metadata assertions.

### Phase 2: external conformance and direct dispatch

* add trait lists to extension declarations;
* bind and validate requirement mappings;
* implement lexical conformance lookup and ambiguity diagnostics;
* bind direct trait member calls through a conformance; and
* add semantic-model and incremental snapshot coverage.

### Phase 3: generalized constraints and defaults

* add binder-owned witness parameters and bound conformance operations;
* lower generic requirement and default calls through witnesses;
* emit/import witness metadata across assemblies; and
* add public CLR bridges for nominal callers.

### Phase 4: trait views

* implement contextual and explicit trait-view conversion;
* generate and import view factories/adapters;
* verify overload ranking, boxing, reflection, and cross-language behavior; and
* optimize calls that do not require a materialized view.

Syntax or bound-model generator inputs change in Phases 1 through 3, so the
solution build script is required after those changes. Each phase also requires
focused syntax, semantic, operations, code-generation/runtime, metadata,
language-service, and incremental-edit tests appropriate to its layer.

## Alternatives within the trait-first design

### Use `impl Printable for Uri`

This is concise but imports Rust's surface vocabulary while Raven already has a
construct whose meaning is external adaptation. Conforming extensions better
connect the feature to Raven's existing lookup and import rules.

### Make external conformances global

Global coherence simplifies generic signatures but makes unrelated referenced
libraries capable of changing a program's behavior and requires ownership rules
that block useful .NET adaptation. Lexical named conformances are preferred.

### Distinguish nominal constraints from generalized trait constraints

Raven could retain `T: Printable` as a CLR-only constraint and introduce a new
spelling for witness-based conformance. That improves raw metadata predictability
but weakens the trait-first model: the most natural constraint would exclude the
feature that distinguishes traits from interfaces. This proposal instead makes
the common source spelling semantic and supplies a .NET bridge.

### Require explicit trait views everywhere

Explicit views make allocations and scope dependence obvious but add ceremony
to ordinary interface-oriented APIs. In a trait-first language, a unique
conformance should support contextual conversion. Explicit casts remain
available for clarity and disambiguation.

## Acceptance criteria

The redesign should be accepted only if Raven is willing to commit to all of
the following:

* `trait` is the sole source contract term;
* imported CLR interfaces are presented consistently as traits;
* `T: Trait` accepts nominal and external conformance;
* witness-bearing Raven APIs have a versioned cross-assembly ABI;
* nominal .NET callers receive a conventional constrained bridge where
  possible;
* external conformance is lexical and may affect type checking through imports;
  and
* compiler APIs distinguish semantic conformance from CLR interface metadata.

If Raven wants only a friendlier word for CLR interfaces, this redesign should
be rejected. If Raven wants behavioral contracts to be independent of type
ownership as a foundational language capability, the trait-first model is more
coherent than adding a narrow external-implementation exception to interfaces.

## Recommendation

Keep this proposal as the stronger, language-defining alternative. It gives
`trait` a real meaning in Raven: a capability with nominal or scoped external
conformance, usable in generic code and as a runtime view.

Do not begin with the keyword migration. First complete the Phase 0 ABI proof.
If witness dispatch, default reuse, cross-assembly discovery, and the nominal
C# bridge form a credible whole, adopt the trait terminology and implement the
feature vertically. If that proof is unattractive, retain Raven's current
interface model and ordinary extensions rather than ship traits in name only.
