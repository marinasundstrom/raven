# Raven Language Philosophy

Raven is a modern programming language designed with clarity, expressiveness, and composability at its core. It draws inspiration from C#, F#, Swift, and Python — but is not bound by their conventions. Raven's guiding philosophy is to prioritize conceptual integrity, minimize syntactic noise, and empower developers to write code that reads like intention, not ceremony. It values simplicity that keeps syntax honest, symmetry that aligns how constructs relate to one another, and flow that allows ideas to move through code without unnecessary friction.

This document captures the non-negotiable beliefs that shape every layer of the project—from syntax and semantics to the architecture of the compiler itself. They act as a compass when making trade-offs, evaluating features, or deciding how tooling should feel.

Raven embraces a declarative, expression-first design where nearly every construct yields a value, but value is never pursued at the expense of readability or tooling.

---

## ✨ Core Principles

### 1. **Syntactic Clarity over Tradition**

Raven eliminates redundant syntax where it doesn’t add value. Familiar patterns are respected when they communicate intent, but tradition alone is never a sufficient reason to carry ceremony forward. Object construction does not require repeating type names, and type instantiation stays low-ceremony even as APIs grow more complex:

```raven
val user = User(name: "Anna")  // Not new User(...)
```

Constructors are expressed through `init` blocks or named methods, making object creation feel like invoking functionality, not a compiler formality. The language defaults to the shortest spelling that still tells the truth about what the code is doing and encourages the flow of data without redundant wrappers or boilerplate.

```raven
public init WithName(name: string) {
    self.name = name
}
```

The usage:

```raven
val user = User.WithName("Anna") 
```

---

### 2. **Syntactic Symmetry**

Symmetry is a design tool, not a slogan, and Raven applies it directly to syntax. Callables, indexers, and member access unify through the shared `self` surface: types are invocable if they define a `self(...)` method, indexable with `self[...]`, and internally consistent. Because the syntax lines up, destructuring and invocation share a single mental model that users learn once and reuse everywhere.

That syntactic sameness extends to the shapes we use for values and functions. The unit value `()` is the empty tuple, the simplest list of values. Annotated tuples use the same parentheses as expressions: `(a: int, b: int)` declares the shape; `(2, 3)` realizes it. Parameter lists reuse the same surface syntax — `(a: int, b: int) -> int` reads just like a tuple that happens to feed a return value — and function type signatures follow the same rule: `(int, int) -> int` is the tuple of inputs flowing into an output. Even an empty parameter list stays honest: `() -> int` makes it explicit that nothing flows in and a value flows out. The language deliberately avoids inventing new sigils for these closely-related concepts so that every set of parentheses tells the same structural truth.

```raven
public self(x: int) -> string { ... }    // Callable object
public self[x: int] : string { ... }     // Indexer
```

This symmetry improves reasoning and avoids special cases. Everything flows: the same conceptual pipeline moves from function invocation to indexers to member access, so the mental overhead of switching contexts vanishes. That sameness also makes tuple construction and argument passing visually mirror each other, reinforcing the idea that data moves through the system in familiar shapes. A full function signature keeps the correspondence visible end to end:

```raven
func Foo(a: int, b: int) -> (x: int, z: string) {
  val inputs = (a, b)
  return (x: inputs.0 + 1, z: inputs.1.ToString())
}
```

The tuple that enters `Foo`, the tuple it returns, and the signature that frames both all share the same surface form, so reading and writing code feels consistent regardless of whether you are looking at data, parameters, or types. Even when values flow through a standalone type annotation, the shapes line up:

```raven
val add: (int, int) -> int = (x, y) => x + y
```

The type annotation, the lambda parameters, and the tuple literal all share the same syntax, reinforcing the idea that symmetry is not abstract philosophy but a concrete rule the language follows.

---

### 3. **Declarative Defaults**

Raven favors declarative constructs before imperative ceremony. Control flow and data shaping begin as expressions that state *what* should happen, reserving step-by-step mutation for cases where it truly clarifies intent. Pattern matching, comprehensions, and expression-bodied members keep business logic focused on outcomes, not scaffolding.

```raven
val positives = numbers.Where(n => n > 0)
val summary = match (positives, negatives) {
  ([], []) -> "empty",
  (_, []) -> "gains",
  ([], _) -> "losses",
  _ -> "mixed"
}

val totalRevenue = orders
  .Where(order => order.isPaid)
  .Select(x => Invoice(x))
  .Sum(x => x.Total)
```

Raven leans on *extensions* to keep everyday code terse without baking every convenience into the core language. Pipelines stay readable because common “shape” operations live as extension members on familiar .NET abstractions.

```raven
import System.Linq.*

val latePickup = requests.Where(r => !r.OnTime).FirstOrNone()
val lateMessage = latePickup match {
    .Some(val req) => "Late pickup: ${req.Id}"
    .None => "No late pickups today"
}
```

Declarative-first does not forbid imperative code; it simply makes the declarative path the straightest line to clarity.

---

### 4. **Expression-First Design**

Raven is expression-oriented: almost every construct evaluates to a value, encouraging composition and reducing the distinction between statements and expressions. Statements remain where they aid readability, but the language nudges you toward value-oriented composition by default.

```raven
value match {
    (0, val no) -> "zero and $no"
    (_, 0) -> "zero again"
    _ -> "non-zero"
}

val message = if user.isAnonymous
  "Welcome!"
else
  "Welcome back, ${user.displayName}!"
```

### 5. **Option/Result as a Core Control-Flow Shape**


Raven treats absence and failure as *data*, not side channels. `Option<T>` and `Result<T, E>`—and their extension methods—are first-class shapes that show up everywhere — APIs return them, extensions compose them, and `match` makes branching explicit.

Extensions are the glue that makes these shapes feel native in everyday code. Instead of forcing every “one-liner convenience” into the compiler, Raven encourages a small set of composable helpers — `Map`, `Then`, `UnwrapOr(...)`, `IsOkOr(...)`, and LINQ-style helpers like `FirstOrNone()` / `FirstOrError(...)` — so flow stays readable and discoverable in tooling.

```raven
val promo = promoCode
  .Map(code => code.Trim().ToUpperInvariant())
  .Then(code => ValidatePromo(code).IsOkOr(PromoError.Invalid))
  .UnwrapOr(.None)

val request = requests.FirstOrError(r => r.Id == "REQ-1002", () => QuoteError("Request not found: REQ-1002"))?
```

The `?` operator is the flow counterpart: it forwards `.None` / `.Error(...)` without forcing nested `match` blocks, keeping the “happy path” linear while still making early-exit behavior visible.

```raven
func BuildQuoteSummary(requests: IEnumerable<ShipmentRequest>, plans: IEnumerable<RatePlan>) -> Result<QuoteSummary, QuoteError> {
    val request = requests.FirstOrError(r => r.Id == "REQ-1002", () => QuoteError("Request not found: REQ-1002"))?
    val quote = QuoteShipment(request, plans)?
    val decision = DecideQuote(quote, request)
    return .Ok(QuoteSummary(quote, decision))
}
```

When you *do* want to branch, `match` keeps it honest and ergonomic:

```raven
val message = summaryResult match {
    .Ok(val summary) => FormatSummary(summary)
    .Error(val error) => "Quote failed: ${error.Message}"
}
```

---

### 6. **Contextual Precision and Modularity**

Precision is not optional; it is the backbone of tooling. Every part of Raven's compiler is context-sensitive and modular. Parsers, binders, and code generators are composable and disposable — no global state, no implicit magic. This architecture makes Raven suitable for rich tooling, speculative parsing, and precise error recovery, whether in an IDE, a CLI build, or a future language service.

---

### 7. **Syntax is Structure**

Raven's syntax trees are not just parsing artifacts — they are designed objects. Nodes are generated from formal XML specifications, ensuring consistency, testability, and predictability. This emphasis on structural design reflects the language's belief that **syntax is architecture**. When the tree is precise, analyzers, refactorings, and code generators can be equally precise.

---

### 8. **Minimalism with Purpose**

Semicolons are optional. Newlines matter — but only when they should. Blocks, expressions, and types interleave naturally. The language does not aim to reduce all syntax, but only that which obstructs intent.

```raven
if isValid
  doWork()
else
  handleError()
```

Semicolon may act as a divider:

```raven
val x = 2; Console.WriteLine(x)
```

---

### 9. **Target-Typed Member Access and Expression Completion**

Raven supports target-typed expressions, allowing constructs like:

```raven
val x: string = .Empty  // If type context implies a known receiver
```

This enables more concise and predictive IntelliSense experiences and binds expression semantics to the type system, not just to the lexical scope.

#### Target-typed union cases

Target typing pairs naturally with union cases, especially for `Option<T>` and `Result<T, E>`. When the expected type is known, you can write the case directly and let the compiler fill in the receiver.

```raven
val promo: Option<string> = .Some("SAVE5")
val missing: Option<string> = .None
```

This makes APIs feel fluent and keeps “what matters” (the case) in focus rather than the container type.

---

### 10. **`val` vs `var`: Mutability by Design**

Raven uses `val` and `var` to make **mutability an explicit decision**:

* `val` declares an immutable binding (cannot be reassigned)
* `var` declares a mutable one (can be reassigned)

```raven
val name = "Anna"   // Immutable
var counter = 0     // Mutable
```

This distinction aligns with Raven’s declarative nature: it expresses *intent* in the syntax. You don't just declare a variable — you declare whether it should change. That clarity helps avoid bugs, makes code easier to reason about, and aligns with functional best practices without enforcing them dogmatically.

> Mutability is not forbidden — but it should always be obvious.

---

### 11. **Progressive Disclosure of Power**

Raven values approachability. Simple programs should look simple; advanced features should only surface when asked for. Features such as traits, extensions, and advanced pattern constructs build upon the same foundational syntax, enabling newcomers to explore the language in concentric circles of understanding.

---

### 12. **Safety without Ceremony**

Raven pursues correctness through the type system, diagnostics, and analyzers. Safety features are designed to prevent sharp edges without introducing ritualistic syntax. Compiler errors prefer actionable guidance over cryptic jargon, and warnings are invitations to better patterns—not arbitrary punishments.

Error handling follows the same philosophy: `try` is an expression, but Raven also embraces typed outcomes through `Result<T, E>` and absence through `Option<T>`. The propagation operator `?` keeps happy-path code clean by forwarding `.Error(...)`/`.None` automatically when the surrounding context expects it, while `match` makes recovery explicit when you choose to handle failures locally.

```raven
val profile = try fetchProfile(for: userId)

val theme = loadSettings("prefs.json")? match {
  .Ok(val settings) => settings.theme,
  .Error(FileError.NotFound) => "light",
  .Error(val error) => throw error
}
```

Declarative recovery paths make it clear when errors are intentionally absorbed versus rethrown, keeping control flow as legible as the happy path.

---

### 13. **Flow-Oriented Simplicity**

Raven code should feel like it moves. “Everything flows” means APIs, control structures, and data transformations connect without abrupt ceremony.

A big part of that flow is that *absence and failure are values* you can pass along. `Option<T>` and `Result<T, E>` travel through pipelines, extension methods transform them, and `?` keeps early-exit behavior linear when that’s what you want.

The type system supports that flow. Types are inferred wherever intent is already obvious, and explicit annotations appear only when they disambiguate, document, or unlock new capabilities.

Raven’s core “flow types” make control paths readable without ceremony: `Option<T>` represents absence, `Result<T, E>` represents success-or-error. They compose naturally with `match`, with helpers like `UnwrapOr(...)`, and with `?` to keep early-exit behavior visible but lightweight.

```raven
val items = fetchItems()
val active = items.Where(item => item.isActive).FirstOrNone()
val filtered = items.Where(item => item.isActive)
val report: Report = .from(filtered)  // Explicit only when the story needs a cast
```

```raven
val decisionText = QuoteShipment(request, plans)
  .Map(q => DecideQuote(q, request))
  .UnwrapOr(.ManualReview("No quote"))
  .ToString()
```

By default, specificity waits until the developer actually needs it. That restraint keeps the language lightweight, expressive, and ready to scale from scripts to systems without drowning the reader in redundant type names.

---

### 14. **Interop and Tooling as First-Class Citizens**

Raven runs on .NET, and the language treats that runtime as an asset rather than an afterthought. Interop is expected, not exotic:
importing BCL namespaces, consuming LINQ pipelines, and compiling to IL are all core experiences. Compiler services mirror Roslyn's
shape so that IDEs, analyzers, and refactorings can lean on the same immutable trees and binding infrastructure. Features that
complicate debuggability or diagnostics must justify their weight; the happy path includes actionable errors, consistent symbol
metadata, and a syntax tree designed for analysis.

---

## 🛠️ Raven Is:

* A language where **form serves meaning**
* A toolkit for building **tools and compilers**, not just scripts
* A framework for **designing clarity** into every layer — syntax, semantics, and code generation
* A platform for **progressive learning**, meeting developers where they are and inviting them deeper

---

## 🧩 Raven Is Not:

* A syntax experiment for its own sake
* A clone of another language
* A compromise between too many paradigms
* A language that prizes novelty over the developer experience

---

## 💬 Final Word

Raven is designed for developers who care about how things fit together — not just whether they run. It’s a language for building languages, crafting systems, and writing code that reflects thought, not repetition.

> Code is not text — it's thought made concrete.
