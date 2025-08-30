# Raven Language Philosophy

Raven is a modern programming language designed with clarity, expressiveness, and composability at its core. It draws inspiration from C#, F#, Swift, and Python — but is not bound by their conventions. Raven's guiding philosophy is to prioritize conceptual integrity, minimize syntactic noise, and empower developers to write code that reads like intention, not ceremony.

It embraces an expression-first, expression-oriented design where nearly every construct yields a value.

---

## ✨ Core Principles

### 1. **Syntactic Clarity over Tradition**

Raven eliminates redundant syntax where it doesn’t add value. Object construction does not require repeating type names:

```raven
let user = User(name: "Anna")  // Not new User(...)
```

Constructors are expressed through `init` blocks or named methods, making object creation feel like invoking functionality, not a compiler formality.

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

Raven unifies callables, indexers, and member access through a common concept: `self`-bound methods. Types are invocable if they define a `self(...)` method, indexable with `self[...]`, and internally consistent.

```raven
public self(x: int) -> string { ... }    // Callable object
public self[x: int] : string { ... }     // Indexer
```

This symmetry improves reasoning and avoids special cases.

---

### 3. **Expression-First Design**

Raven is expression-oriented: almost every construct evaluates to a value, encouraging composition and reducing the distinction between statements and expressions.

---

### 4. **Declarative, Not Just Functional**

Raven embraces declarative programming: describing *what* rather than *how*. Pattern matching, expressions-as-values, and implicit returns enable elegant problem modeling:

```raven
match value {
    (0, _) -> "zero"
    (_, 0) -> "zero again"
    _ -> "non-zero"
}
```

---

### 5. **Contextual Precision and Modularity**

Every part of Raven's compiler is context-sensitive and modular. Parsers, binders, and code generators are composable and disposable — no global state, no implicit magic. This architecture makes Raven suitable for rich tooling, speculative parsing, and precise error recovery.

---

### 6. **Syntax is Structure**

Raven's syntax trees are not just parsing artifacts — they are designed objects. Nodes are generated from formal XML specifications, ensuring consistency, testability, and predictability. This emphasis on structural design reflects the language's belief that **syntax is architecture**.

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

## 🛠️ Raven Is:

* A language where **form serves meaning**
* A toolkit for building **tools and compilers**, not just scripts
* A framework for **designing clarity** into every layer — syntax, semantics, and code generation

---

## 🧩 Raven Is Not:

* A syntax experiment for its own sake
* A clone of another language
* A compromise between too many paradigms

---

## 💬 Final Word

Raven is designed for developers who care about how things fit together — not just whether they run. It’s a language for building languages, crafting systems, and writing code that reflects thought, not repetition.

> Code is not text — it's thought made concrete.
