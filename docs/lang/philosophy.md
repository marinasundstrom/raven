# Raven Language Philosophy

Raven is a .NET language that leans toward expression-oriented code, structural pattern matching, typed recoverable flow, and low-ceremony syntax without pretending that systems programming, interop, and tooling are secondary concerns. It takes cues from C#, F#, Swift, and Rust, but its center of gravity is pragmatic: make code read clearly, keep compiler behavior explainable, and let .NET remain a real home instead of a reluctant target.

This document describes the design pressures that should guide language changes and documentation. It is intentionally grounded in Raven's current shape rather than a wishlist of future ideas.

## Core stance

Raven is not trying to be purely functional, purely object-oriented, or novel for its own sake. It favors a small set of consistent ideas:

- expressions where values help composition
- statements where effects and control flow are clearer
- explicit mutability and explicit narrowing of visibility
- explicit bindings when patterns introduce new names
- `Option<T>` and `Result<T, E>` as normal control-flow shapes
- one reusable pattern system across branching, iteration, and deconstruction
- direct, ergonomic interop with the .NET ecosystem

## Expression-oriented, not expression-only

Raven prefers expressions because they compose well and keep local logic compact. `if`, `match`, `try`, and function bodies are designed to work naturally in value position.

```raven
val message = status match {
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

`null` is still part of the language because .NET interop demands it, but Raven’s
preferred design direction is explicit domain flow through `Option<T>` and
`Result<T, E>`, not nullable-heavy application logic.

## Explicitness where it changes meaning

Raven does not chase minimal syntax at all costs. It removes ceremony when the syntax carries no real information, but it keeps explicit markers when they materially affect semantics.

That is why Raven uses:

- `val` and `var` to make mutability visible
- access modifiers to narrow visibility from the public-by-default baseline
- explicit pattern forms when introducing bindings
- explicit return and propagation behavior for carrier-based flow

```raven
val name = "Raven"
var retries = 0
```

The principle is simple: if a choice affects how code behaves or what other code may rely on, the source should show that choice plainly.

This principle matters especially in pattern syntax. Raven intentionally moved
away from implicit capture in freestanding and inline patterns because pattern
code becomes harder to read once bare identifiers can silently introduce names.
The language still supports shorthand forms such as:

- `val (a, b) = expr`
- `if val pattern = expr`
- `for val pattern in values`
- `match value { val pattern => ... }`

but the rule remains the same: the syntax that introduces a binding should say so.

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
- `if val pattern = expr`
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
- async workflows
- extensions and traits
- richer `Option`/`Result` pipelines

The preferred path is depth through composition, not depth through syntax inflation.

## What Raven is optimizing for

Raven should continue to optimize for:

- readable code with visible control flow
- explicit semantics around mutability, visibility, and failure
- explicit and reusable semantics around binding and patterns
- low-ceremony syntax that still respects structure
- strong .NET interop
- compiler and tooling architectures that stay precise and maintainable

Raven is not trying to be the shortest language to type, the most magical language to compile, or the most academically pure language to defend. It is trying to be coherent.
