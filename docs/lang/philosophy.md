# Raven Language Philosophy

Raven is a modern programming language designed with clarity, expressiveness, and composability at its core. It draws inspiration from C#, F#, Swift, and Python — but is not bound by their conventions. Raven's guiding philosophy is to prioritize conceptual integrity, minimize syntactic noise, and empower developers to write code that reads like intention, not ceremony.

This document captures the non-negotiable beliefs that shape every layer of the project—from syntax and semantics to the architecture of the compiler itself. They act as a compass when making trade-offs, evaluating features, or deciding how tooling should feel.

Raven embraces a declarative, expression-first design where nearly every construct yields a value, but value is never pursued at the expense of readability or tooling.

---

## ✨ Core Principles

### 1. **Syntactic Clarity over Tradition**

Raven eliminates redundant syntax where it doesn’t add value. Familiar patterns are respected when they communicate intent, but tradition alone is never a sufficient reason to carry ceremony forward. Object construction does not require repeating type names:

```raven
let user = User(name: "Anna")  // Not new User(...)
```

Constructors are expressed through `init` blocks or named methods, making object creation feel like invoking functionality, not a compiler formality. The language defaults to the shortest spelling that still tells the truth about what the code is doing.

```raven
public init WithName(name: string) {
    self.name = name
}
```

The usage:

```raven
let user = User.WithName("Anna") 
```

---

### 2. **Symmetric Design**

Symmetry is a design tool, not a slogan. Raven unifies callables, indexers, and member access through a common concept: `self`-bound methods. Types are invocable if they define a `self(...)` method, indexable with `self[...]`, and internally consistent. Users learn one model of invocation and reuse it everywhere.

```raven
public self(x: int) -> string { ... }    // Callable object
public self[x: int] : string { ... }     // Indexer
```

This symmetry improves reasoning and avoids special cases.

---

### 3. **Declarative Defaults**

Raven favors declarative constructs before imperative ceremony. Control flow and data shaping begin as expressions that state *what* should happen, reserving step-by-step mutation for cases where it truly clarifies intent. Pattern matching, comprehensions, and expression-bodied members keep business logic focused on outcomes, not scaffolding.

```raven
let positives = numbers.filter { $0 > 0 }
let summary = match (positives, negatives) {
  ([], []) -> "empty",
  (_, []) -> "gains",
  ([], _) -> "losses",
  _ -> "mixed"
}
```

Declarative-first does not forbid imperative code; it simply makes the declarative path the straightest line to clarity.

---

### 4. **Expression-First Design**

Raven is expression-oriented: almost every construct evaluates to a value, encouraging composition and reducing the distinction between statements and expressions. Statements remain where they aid readability, but the language nudges you toward value-oriented composition by default.

```raven
value match {
    (0, _) -> "zero"
    (_, 0) -> "zero again"
    _ -> "non-zero"
}
```

---

### 5. **Contextual Precision and Modularity**

Precision is not optional; it is the backbone of tooling. Every part of Raven's compiler is context-sensitive and modular. Parsers, binders, and code generators are composable and disposable — no global state, no implicit magic. This architecture makes Raven suitable for rich tooling, speculative parsing, and precise error recovery, whether in an IDE, a CLI build, or a future language service.

---

### 6. **Syntax is Structure**

Raven's syntax trees are not just parsing artifacts — they are designed objects. Nodes are generated from formal XML specifications, ensuring consistency, testability, and predictability. This emphasis on structural design reflects the language's belief that **syntax is architecture**. When the tree is precise, analyzers, refactorings, and code generators can be equally precise.

---

### 7. **Minimalism with Purpose**

Semicolons are optional. Newlines matter — but only when they should. Blocks, expressions, and types interleave naturally. The language does not aim to reduce all syntax, but only that which obstructs intent.

```raven
if isValid
  doWork()
else
  handleError()
```

Semicolon may act as a divider:

```raven
let x = 2; Console.WriteLine(x)
```

---

### 8. **Target-Typed Member Access and Expression Completion**

Raven supports target-typed expressions, allowing constructs like:

```raven
let x: string = .Empty  // If type context implies a known receiver
```

This enables more concise and predictive IntelliSense experiences and binds expression semantics to the type system, not just to the lexical scope.

---

### 9. **`let` vs `var`: Mutability by Design**

Raven uses `let` and `var` to make **mutability an explicit decision**:

* `let` declares an immutable binding (cannot be reassigned)
* `var` declares a mutable one (can be reassigned)

```raven
let name = "Anna"   // Immutable
var counter = 0     // Mutable
```

This distinction aligns with Raven’s declarative nature: it expresses *intent* in the syntax. You don't just declare a variable — you declare whether it should change. That clarity helps avoid bugs, makes code easier to reason about, and aligns with functional best practices without enforcing them dogmatically.

> Mutability is not forbidden — but it should always be obvious.

---

### 10. **Progressive Disclosure of Power**

Raven values approachability. Simple programs should look simple; advanced features should only surface when asked for. Features such as traits, extensions, and advanced pattern constructs build upon the same foundational syntax, enabling newcomers to explore the language in concentric circles of understanding.

---

### 11. **Safety without Ceremony**

Raven pursues correctness through the type system, diagnostics, and analyzers. Safety features are designed to prevent sharp edges without introducing ritualistic syntax. Compiler errors prefer actionable guidance over cryptic jargon, and warnings are invitations to better patterns—not arbitrary punishments.

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
