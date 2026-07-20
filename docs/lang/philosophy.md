# Raven Language Philosophy

Raven is a .NET language that leans toward expression-oriented code, structural pattern matching, typed recoverable flow, and low-ceremony syntax without pretending that systems programming, interop, and tooling are secondary concerns. It takes cues from C#, F#, Swift, and Rust, but its center of gravity is pragmatic: make code read clearly, keep compiler behavior explainable, and let .NET remain a real home instead of a reluctant target.

This document describes the design pressures that should guide language changes and documentation. It is intentionally grounded in Raven's current shape rather than a wishlist of future ideas.

Use this document as a review aid. A proposed feature does not need to maximize
every principle here, but it should be able to explain which tradeoff it is
making and why that tradeoff belongs in Raven.

## Short version

Raven optimizes for readable, toolable .NET code where important semantic
choices are visible at the point of use.

That means Raven generally chooses:

- composition over cleverness
- explicit semantic markers over ambiguous shorthand
- declaration keywords over context-only interpretation
- carrier-based recoverable flow over exception-heavy domain paths
- one reusable pattern model over unrelated special forms
- plain functions for standalone behavior, without forcing class wrappers
- classes and interfaces when identity, lifecycle, encapsulation, or open
  polymorphism belong in the model
- direct .NET interop over runtime isolation
- compiler and editor clarity over syntax tricks that are hard to diagnose

## Core stance

Raven is not trying to be purely functional, purely object-oriented, or novel for its own sake. It favors a small set of consistent ideas:

- expressions where values help composition
- statements where effects and control flow are clearer
- explicit mutability and explicit narrowing of visibility
- explicit declaration keywords for functions, bindings, events, types, and
  union cases
- explicit bindings when patterns introduce new names
- `Option<T>` and `Result<T, E>` as normal control-flow shapes
- one reusable pattern system across branching, iteration, and deconstruction
- direct, ergonomic interop with the .NET ecosystem

Functional and object-oriented modeling are therefore not competing modes in
Raven. The language should make it natural to use functions and algebraic data
for rules and state transitions, objects for identity and lifecycle, and narrow
function-shaped boundaries between them where that keeps dependencies explicit.
Object-oriented programming is part of the language's intended toolset, not a
compatibility feature kept only because Raven targets .NET. At the same time,
Raven does not require an object merely to contain an entry point, a standalone
operation, or a small group of functions.

These are design defaults, not slogans. When a feature pulls against them, the
feature should bring a concrete benefit that is visible in real Raven programs,
not only in isolated syntax examples.

## Current shape and direction

Raven is experimental, so documentation must separate implemented behavior from
design direction. Philosophy pages can explain where the language wants to go,
but user-facing walkthroughs and examples should prefer syntax and APIs that
work today.

When documenting future-leaning ideas:

- call them proposals, investigations, or design direction
- link to the relevant proposal or spec chapter
- avoid making examples look like current supported syntax unless they are
  covered by tests or samples

This keeps Raven ambitious without making the docs unreliable.

## Expression-oriented, not expression-only

Raven prefers expressions because they compose well and keep local logic compact. `if`, `match`, `try`, and function bodies are designed to work naturally in value position.

```raven
val message = match status {
    Ok(val value) => "Ready: $value"
    Error(val err) => "Failed: $err"
}
```

That does not mean every construct must be forced into expression form. Raven keeps statement forms for loops, mutation, branching, disposal, and early exits because effectful code should stay obvious.

The language aim is composability without pretending that side effects do not exist.

## Flow should stay visible

Raven wants happy-path code to stay linear while still making failure and absence honest. That is why `Option<T>` and `Result<T, E>` are central to the language story instead of being treated as niche library patterns.

```raven
func BuildQuote(id: string) -> Result<string, string> {
    val request = LoadRequest(id)?
    val plan = LoadPlan(request.Carrier)?
    return Ok("${request.Id}: ${plan.Name}")
}
```

`?` removes boilerplate, but it does not hide control flow. Reading the code still tells you that execution may short-circuit. When branching matters, `match` keeps the recovery path explicit.

Raven therefore leans toward:

- modeling absence with `Option<T>` instead of raw nulls in domain code
- modeling expected failures with `Result<T, E>` instead of exceptions alone
- using `match` when a decision deserves to be seen, not hidden

In the standard library, those carriers use plain `union` declarations. That
keeps their runtime shape compatible with the expected .NET struct-union
contract, including the explicit default/uninitialized carrier state.

`null` is still part of the language because .NET interop demands it, but Raven’s
preferred design direction is explicit domain flow through `Option<T>` and
`Result<T, E>`, not nullable-heavy application logic.

## Explicitness where it changes meaning

Raven does not chase minimal syntax at all costs. It removes ceremony when the syntax carries no real information, but it keeps explicit markers when they materially affect semantics.

That is why Raven uses:

- `val` and `var` to make mutability visible
- `func` to declare functions and methods
- `event` to declare events
- `class`, `struct`, `interface`, `record`, `union`, and `case` to make type
  and union declarations scan clearly
- access modifiers to narrow visibility from the public-by-default baseline
- explicit pattern forms when introducing bindings
- explicit return and propagation behavior for carrier-based flow

```raven
val name = "Raven"
var retries = 0

func Normalize(name: string) -> string => name.Trim()

union Decision {
    case Approve
    case Reject(reason: string)
}
```

The principle is simple: if a choice affects how code behaves or what other code may rely on, the source should show that choice plainly.

This is also why Raven favors keyword-led declarations. The reader should not
have to infer whether a member is a method, property, event, union case, or local
binding solely from punctuation or where the declaration appears. Raven syntax
tries to announce the declaration kind first, then let modifiers and types refine
it.

This also guides Raven's shorthand forms. Target-typed expressions such as
`.Active` and `.(1, "Ada")` are useful when the target is visible from an
annotation, parameter, return type, or collection element type. They should not
be treated as a blanket preference for shorter code. When the target type is not
obvious from the local form or nearby name, spelling the type or member
explicitly is clearer.

This principle matters especially in pattern syntax. Raven intentionally moved
away from implicit capture in freestanding and inline patterns because pattern
code becomes harder to read once bare identifiers can silently introduce names.
The language still supports shorthand forms such as:

- `val (a, b) = expr`
- `if let pattern = expr`
- `for let pattern in values`
- `match value { val pattern => ... }`

but the rule remains the same: the syntax that introduces a binding should say so.

## Functions do not need class wrappers

Raven supports top-level functions because many useful operations are not
naturally methods on an object. Parsing, validation, formatting, lookup,
workflow orchestration, and small domain transformations should be easy to name
directly.

```raven
func NormalizeCarrier(name: string) -> string {
    return name.Trim().ToUpperInvariant()
}

func TryParseQuantity(text: string) -> Result<int, string> {
    return try int.Parse(text) match {
        Ok(val value) => Ok(value)
        Error(_) => Error("\"$text\" is not a quantity")
    }
}
```

This is a deliberate divergence from the C# habit of creating static utility
classes solely to hold functions. Raven still has classes, records, structs,
interfaces, extensions, and methods when behavior belongs with a type. It just
does not require a container type when the function itself is the useful unit.

The same applies at the application boundary: top-level statements and plain
`Main` functions are valid entry shapes. A `Program` class is not part of the
language's conceptual or syntactic ceremony.

The design pressure is simple: choose the declaration shape that describes the
program, not a container shape inherited from another language's conventions.

## Objects are domain tools

Raven's support for functions outside types does not demote object-oriented
programming. A class is the natural Raven shape when a concept has identity,
owns mutable state or a resource lifecycle, protects invariants through
encapsulation, or participates in an open polymorphic contract.

```raven
class GreenhouseDevice private (val DeviceId: string) {
    static func Connect(deviceId: string) -> Result<GreenhouseDevice, string> {
        return Ok(GreenhouseDevice(deviceId))
    }

    func ReadTemperature() -> Result<decimal, string> {
        return Ok(21.5)
    }
}
```

The important distinction is between an object that models something and a
class used only as a container. Raven removes the need for the latter while
making the former an intentional part of the language.

Function parameters also provide a narrow composition boundary between these
styles. A workflow can accept `() -> Result<decimal, string>` without knowing
whether the supplied operation is a plain function, a lambda, or a method on a
stateful `GreenhouseDevice`. Use an interface instead when the abstraction is a
cohesive, open protocol rather than one capability.

## Consistency beats novelty

Raven prefers a small number of reusable surface patterns over many one-off syntactic tricks. Functions, tuples, pattern matching, constructor calls, and primary-constructor-based type declarations should feel like parts of one system rather than separate mini-languages.

```raven
func Add(a: int, b: int) -> int => a + b

val op: (int, int) -> int = (x, y) => x + y

record class ShipmentRequest(val Id: string, val Carrier: string)
```

Consistency matters more than novelty because it helps three different audiences at once:

- users reading and writing code
- tooling reasoning about syntax and semantics
- compiler contributors extending the language without introducing arbitrary special cases

This is one reason Raven keeps reusing the same pattern model across:

- `match` arms
- `if value is pattern`
- `if let pattern = expr`
- `for` pattern targets
- deconstruction assignment and declaration

The language gets more powerful without becoming more fragmented.

## .NET is a home, not a compromise

Raven is designed for .NET, not merely capable of targeting it. Importing namespaces, calling BCL APIs, using LINQ-style pipelines, and emitting regular IL are part of the intended experience.

```raven
import System.Console.*
import System.Linq.*

val names = users.Where(x => x.IsActive).Select(x => x.Name)
WriteLine(names.Count().ToString())
```

Interop should feel straightforward. Raven should not require users to abandon the .NET ecosystem just to get the language's ergonomics.

At the same time, Raven should not blindly mirror C# syntax or semantics when a clearer Raven-native model exists. The rule is compatibility where it helps, independence where it clarifies.

That is why Raven is comfortable borrowing ecosystem expectations from C# when
they help interoperability, while still choosing Swift/F#/Rust-like surface
decisions when they produce a clearer language model.

Union carriers are one example of that boundary. Raven follows the conventional
`.NET` contract by exposing a `Value` property and compatible carrier behavior,
but it keeps Raven-native case construction, pattern matching, and extension
driven helper APIs instead of copying C# surface syntax wholesale.

## Extensions over special cases

Raven prefers to push common behavior into ordinary libraries and extension surfaces instead of baking every convenience into the compiler. Features like `Option`/`Result` helpers, collection adapters, and domain-specific APIs should compose through normal method calls, imports, and extension lookup.

```raven
extension StringExt for string {
    func ToSlug() -> string =>
        self.Trim().ToLowerInvariant().Replace(" ", "-")
}
```

This keeps the core language smaller and makes libraries, tooling, and diagnostics easier to reason about.

## Tooling and diagnostics are part of the language

Raven's compiler architecture is part of its philosophy. Immutable syntax trees, precise spans, generated syntax models, and explicit binding stages are not implementation trivia; they are how the language stays understandable and toolable.

Language features should therefore be judged partly by whether they:

- produce precise diagnostics
- recover sensibly in incomplete code
- fit the syntax and bound tree model cleanly
- remain explainable in IDE features and compiler APIs

If a feature is clever but hard to diagnose, hard to lower, or hard to expose through tooling, it carries a much higher burden of proof.

This matters more as Raven becomes more pattern-heavy. A feature is not really
finished when the parser accepts it; it is finished when:

- symbol lookup can resolve its bindings
- hover/completion can explain those bindings
- incomplete code still produces useful recovery
- diagnostics stay specific instead of collapsing into generic error symbols

## Progressive power

Simple Raven code should look simple. Advanced features should build on the same foundations rather than forcing users to learn a different language tier.

Beginners should be able to start with:

```raven
func Main() -> () {
    val greeting = "Hello, Raven!"
}
```

Then scale into:

- pattern matching
- primary constructors and records
- classes, interfaces, and encapsulated object models
- async workflows
- extensions and traits
- richer `Option`/`Result` pipelines

The preferred path is depth through composing functions, data, and objects, not
depth through syntax inflation.

## What Raven is optimizing for

Raven should continue to optimize for:

- readable code with visible control flow
- explicit semantics around mutability, visibility, and failure
- explicit and reusable semantics around binding and patterns
- low-ceremony syntax that still respects structure
- strong .NET interop
- compiler and tooling architectures that stay precise and maintainable

Raven is not trying to be the shortest language to type, the most magical language to compile, or the most academically pure language to defend. It is trying to be coherent.
