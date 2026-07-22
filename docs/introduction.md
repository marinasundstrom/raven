# Raven

Raven is a pragmatic, typed application language for .NET that makes functional
composition, algebraic modeling, and object-oriented design complementary parts
of one toolset, with direct access to the .NET runtime and ecosystem.

It is built around a few clear ideas: expression-oriented code where values
help, statement forms where effects are clearer, explicit mutability, explicit
pattern bindings, and pragmatic interop with the .NET ecosystem.

It takes visible inspiration from Swift, Rust, and F#, but Raven is not trying to imitate any one of them exactly. Its current shape is defined more by consistency than by novelty:

- expression-oriented code with statement forms where control flow is clearer
- explicit lexical mutability with `let` and `var`
- explicit pattern bindings, including deconstruction and control-flow patterns
- `Option<T>` and `Result<T, E>` as ordinary control-flow shapes
- records, primary constructors, unions, and structural patterns
- direct use of .NET libraries, collections, async APIs, and IL-based tooling

> Raven has no `void`; it uses `unit`, written as `()`.

## Start without ceremony

Raven introduces behavior with functions because a function is often the most
direct way to name a calculation, validation rule, or workflow step. Functions
can be composed, stored in values, and passed as dependencies without first
creating a class or one-method service interface.

Even the program itself does not need a class wrapper. Top-level statements can
form a small application directly, and a named entry point is a plain `Main`
function rather than a method that must live on a `Program` class.

The same is true for reusable behavior: parsing, validation, formatting, and
workflow functions can live directly in a namespace. They do not need to be
wrapped in a static utility class just to give them somewhere to live.

This low-ceremony starting point does not make Raven a purely functional
language. Object-oriented programming is a first-class part of Raven's toolset,
not merely a compatibility layer for .NET. Classes, interfaces, methods, and
mutable state are available where identity, encapsulation, lifecycle, or
polymorphism are part of the model. Records and unions cover other useful domain
shapes without forcing everything into an object hierarchy.

```raven
record struct Temperature(Value: decimal)

func IsTooHot(limit: Temperature, reading: Temperature) -> bool {
    return reading.Value > limit.Value
}

func Monitor(read: () -> Temperature, report: (Temperature) -> ()) -> () {
    let reading = read()
    report(reading)
}
```

Here `Monitor` receives behavior through function parameters. A production
program can pass device and telemetry functions; a test can pass deterministic
ones. No service wrapper is required. When a stateful device connection really
has a lifecycle, it can still be modeled as a class and expose one of its methods
as the same function-shaped dependency.

If these choices are unfamiliar from C#, the [Raven for C#
developers](raven-for-csharp-developers.md) guide compares common C# structures
with their Raven counterparts.

---

## Hello, World

```raven
import System.Console.*

WriteLine("Hello, World!")
```

Top-level statements are supported. Top-level type declarations are hoisted for binding, so scripts can place helper types near the file-scope code that uses them.

Function and block bodies may also declare local helper `class`, `struct`,
`record`, and `enum` types. These declarations are scoped to the containing
body and are intended for types that are only used locally.

---

## A quick taste

```raven
import System.*
import System.Console.*

func Main() -> () {
    let inputs = ["10", "3", "abc", "42"]

    let message = match ProcessNumbers(inputs) {
        Ok(_) => "All numbers processed"
        Error(let err) => "Failed: $err"
    }

    WriteLine(message)
}

func ProcessNumbers(inputs: string[]) -> Result<(), string> {
    for text in inputs {
        let no = ParseInt(text)?

        let line = match no {
            10 => "Ten exactly!"
            let value => "Parsed: $value"
        }

        WriteLine(line)
    }

    return Ok
}

func ParseInt(text: string) -> Result<int, string> {
    return int.Parse(text) match {
        Ok(let v) => Ok(v)
        Error(_) => Error("\"$text\" is not a number")
    }
}
```

That example shows the current Raven style in miniature:

- `match` is a normal expression
- `Result` values are handled directly instead of being wrapped in exception-heavy flow
- pattern arms use explicit binding keywords where they introduce new values
- selected .NET APIs are projected into Raven's `Option` and `Result`
  vocabulary; here `int.Parse(string)` returns a `Result` whose error channel
  is the union of the framework exceptions that the projection captures

---

## Target-typed shorthand

When the surrounding context supplies a type, Raven lets a leading `.` stand in
for that target. Named member bindings such as `.Active` resolve static members,
enum values, and union cases on the target type. An omitted member name,
`.(...)`, constructs the target type.

```raven
union UserStatus {
    case Active
    case Suspended(reason: string)
}

record class User(
    val Id: int,
    val Name: string,
    val Role: Option<string>,
    val Status: UserStatus)

let users: User[] = [
    .(1, "Ada", Some("compiler engineer"), .Active),
    .(2, "Bo", None, .Suspended("email bounced"))
]
```

The same rule works in assignment, return, argument, and collection-element
positions whenever the target type is known. Use it where that target is clear
from an annotation, an API parameter, or the surrounding name. Prefer the
explicit form (`User(...)`, `UserStatus.Active`) when the shorthand would make a
reader hunt for the target type.

---

## Bindings and mutability

- `let` introduces an immutable lexical binding.
- `var` introduces a mutable lexical binding.
- `val` and `var` describe read-only and writable properties and signature-like
  declarations.

```raven
let name = "Raven"
var count = 0
count = count + 1

let a = 1, b = 2
var left = 10, right = 20
```

Raven also accepts `val` as an alternative spelling for an immutable lexical
binding. The language's standard style and generated examples use `let`, giving
lexical code the familiar `let`/`var` pairing while reserving the `val`/`var`
pairing for properties and signatures. Choosing `val` for a local does not
produce a diagnostic unless a project explicitly enables the optional
`PreferLetInsteadOfValAnalyzer` style policy.

`let` and `val` make the binding read-only; they do not make the referenced
object deeply immutable. A read-only binding cannot be reassigned, but a
mutable object reached through it may still change. Semantic tooling describes
a local introduced with `let` as `val`, because `val` is the resulting binding's
mutability rather than its lexical introduction syntax.

---

## Expressions and matching

`if` and `match` are commonly used in expression position, but Raven is not expression-only. Loops, disposal, mutation, and early returns remain ordinary statement forms when that keeps intent clearer.

```raven
let label = match value {
    0 => "zero"
    _ => "non-zero"
}
```

```raven
let message = if isAnonymous {
    "Welcome"
} else {
    "Welcome back"
}
```

Raven also leans heavily on patterns as a control-flow surface, not only as a `match` feature:

```raven
if let Person(1, name, _) = person {
    WriteLine(name)
}

while let Some(item) = stream.Next() {
    WriteLine(item)
}

for let [first, ...rest] in rows {
    WriteLine(first)
}

loop {
    if work.IsComplete {
        break
    }

    work.Step()
}

let label = match input {
    let Some((x, y)) => "($x, $y)"
    _ => "none"
}
```

The important rule is that pattern bindings stay explicit. In inline and freestanding
patterns, a capture uses a binding keyword. When Raven offers an outer shorthand
form such as `let (...) = expr`, `if let pattern = expr`,
`while let pattern = expr`, `for let pattern in values`, or
`match { let pattern => ... }`, that outer keyword supplies the binding mode for
otherwise bare captures inside the pattern. Those implicit captures may still
carry inline type annotations, as in `let (key: string, value: int) = entry`.

There is also an important surface distinction:

- `is`, `match`, `if let pattern = expr`, `while let pattern = expr`, and
  `for ... in` pattern targets use the general pattern language.
- deconstruction assignment/declaration (`let (...) = expr`, `(...) = expr`,
  `let [...] = expr`, `[...] = expr`) use the deconstruction subset rather than
  every match-only pattern form.

---

## Result and Option

Raven treats recoverable flow as data. In domain code, `Option<T>` is preferred for absence and `Result<T, E>` for expected failures.

In the standard library, both carriers are defined as plain `union` types, which
means they use the default struct carrier shape aligned with the .NET union
contract.

```raven
func Divide(a: int, b: int) -> Result<int, string> {
    if b == 0 {
        return Error("Division by zero")
    }
    return Ok(a / b)
}

let value = Divide(10, 2)?
```

```raven
func FindFirstEven(values: int[]) -> Option<int> {
    for value in values {
        if value % 2 == 0 {
            return Some(value)
        }
    }
    return None
}
```

### Propagation expressions (`?`)

Use `?` to forward failure/absence and keep the happy path linear.

```raven
func BuildLabel(values: int[]) -> Result<string, string> {
    let firstEven = FindFirstEven(values)?
    let quotient = Divide(100, firstEven)?
    return Ok("Result: $quotient")
}
```

### Railroad-style flow with carrier methods

Raven also supports pipeline-friendly methods on `Result` and `Option` so transformations and fallbacks stay in one straight line:

```raven
import System.Linq.*
import System.Collections.Generic.*

let plans = List<RatePlan> {
    RatePlan("NorthStar", 500, 120)
    RatePlan("Oceanic", 450, 150)
}

let requests = List<ShipmentRequest> {
    ShipmentRequest("REQ-1001", "NorthStar", 10, Some("SAVE5"))
    ShipmentRequest("REQ-1002", "Oceanic", 3, None)
}

let summary = match BuildQuoteSummary(requests, plans) {
    Ok(let message) => message
    Error(let err) => "Quote failed: $err"
}

func BuildQuoteSummary(requests: IEnumerable<ShipmentRequest>, plans: IEnumerable<RatePlan>) -> Result<string, string> {
    let request = requests
        .FirstOrError(r => r.Id == "REQ-1002", () => "Request not found")?

    let total = plans
        .FirstOrError(p => p.Carrier == request.Carrier, () => "Rate plan not found")
        .Map(plan => plan.BaseCents + (request.WeightKg * plan.PerKgCents))?

    let promoDiscount = PromoCents(request.PromoCode).UnwrapOr(0)

    return Ok("Quote ${request.Id}: ${total - promoDiscount} cents")
}

func PromoCents(code: Option<string>) -> Option<int> {
    let raw = code?
    let normalized = raw.Trim().ToUpperInvariant()

    match normalized {
        "SAVE5" => Some(500)
        _ => None
    }
}

record class ShipmentRequest(val Id: string, val Carrier: string, val WeightKg: int, val PromoCode: Option<string>)
record class RatePlan(val Carrier: string, val BaseCents: int, val PerKgCents: int)
```

In Raven, `null` still exists for .NET interop, but it is not the preferred domain-modeling tool. The intended direction is:

- use `Option<T>` for absence in Raven code
- use `Result<T, E>` for expected failure
- use nullable types primarily where .NET APIs already speak in those terms

---

## Functions

```raven
func Add(a: int, b: int) -> int => a + b

let addA = func (x: int) => x + 42
let addB = func (x: int) {
    x + 42
}
let result = addA(1)
```

Raven treats declarations and expressions as one function concept:

- Named declarations can appear as top-level functions in namespaces, as methods on types, or as local functions inside blocks.
- Function expressions produce function values: `func (...) => ...`, `func (...) { ... }`, or shorthand `(...) => ...`.

In practice, the difference is binding shape:

- Named declarations introduce top-level, member, or local function symbols.
- Function expressions produce a function value that you bind to `let`/`var` or pass as an argument.

Function type signatures use arrow notation:

```raven
let f: (int, int) -> int
let g = func (a: int, b: int) => a + b
```

Function expressions may omit `func` as shorthand. This is mainly a convenience for higher-order call sites such as LINQ-style APIs:

```raven
let projected = [1, 2, 3].Where(x => x > 1).Select(x => x * 2)
```

Function expressions also support `async`, `static`, and `static async` modifier forms:

```raven
let add = static func (a: int, b: int) {
    a + b
}

let delayedAdd = async func (a: int, b: int) {
    await Task.Delay(2)
    return a + b
}
```

`static` function expressions cannot capture outer locals or parameters.

Function signatures, function expressions, and tuple shapes intentionally reuse the same arrow-and-parentheses vocabulary so higher-order code stays readable instead of introducing a separate syntax family.

---

## Data shapes and patterns

Raven treats structural data inspection as a core language tool.

```raven
union Token {
    case Identifier(text: string)
    case Number(value: int)
    case End

    func IsTerminal() -> bool {
        return self is End
    }
}

func Describe(token: Token) -> string {
    match token {
        .Identifier(let text) => "id: $text"
        .Number(let value) => "number: $value"
        .End => "end"
    }
}
```

The same general pattern model works across:

- `match`
- `if value is pattern`
- `if let pattern = expr`
- `while let pattern = expr`
- `for ... in` with pattern targets
- deconstruction assignment and declaration

This reuse is intentional. Raven wants one pattern system that scales across value
inspection, branching, iteration, and deconstruction instead of splitting those
features into unrelated syntax families.

Union bodies are ordinary member bodies, so case declarations and authored
members live together in the same `union`:

```raven
union Result<T, E> {
    case Ok(value: T)
    case Error(error: E)

    func IsSuccess() -> bool {
        return self is Ok
    }
}
```

At the same time, deconstruction forms stay intentionally narrower than general
pattern-matching forms. They are designed for extraction, not for every kind of
conditional match. That means Raven reuses one pattern model conceptually while
still distinguishing between:

- general matching surfaces such as `is`, `match`, `if let pattern = expr`,
  `while let pattern = expr`, and `for let pattern in values`
- deconstruction surfaces such as `let (a, b) = expr` and `[head, ..tail] = values`

When deconstruction uses a `Deconstruct` shape, Raven also supports named
elements so existing code can stay stable as new properties are added. Both
matching and declaration/assignment deconstruction may spell element names in
any order, for example `Person(Items: let items, Name: let name, Age: 42)` or
`let (Items: items, Name: name, Age: age) = person`. Named elements bind by the
selected `Deconstruct` parameter name; unknown names report the same
member-not-found diagnostic Raven uses for property patterns.
Typed captures inside named elements use the explicit nested-binding spelling
`Name: let name: string`; the ambiguous shorthand `Name: name: string` is
diagnosed.

---

## Extensions

```raven
extension StringExt for string {
    func ToSlug() -> string =>
        self.Trim().ToLowerInvariant().Replace(" ", "-")
}

import MyApp.StringExt.*

let slug = "Hello World".ToSlug()
```

---

## Records and primary constructors

```raven
record class ShipmentRequest(
    val Id: string,
    val Carrier: string,
    val WeightKg: int,
)
```

For classes and structs, primary-constructor parameters become properties only when declared with `val`/`var`.
For records, positional parameters are promoted by default and define the record's complete value shape. Record bodies can add computed members and static storage, but not extra instance fields, storage properties, or secondary constructors.

---

## Accessibility defaults

Raven members are public by default. Use access modifiers (`private`, `internal`, `protected`) when you intentionally narrow visibility.

For type-like declarations, Raven also supports `fileprivate` when a helper should only be visible inside the current source file. File-scoped declarations keep their source name for same-file lookup, but the compiler mangles the emitted type name.

```raven
class Counter(private var count: int = 0) {
    func Increment() -> () {
        count += 1
    }

    val Count: int => count
}
```

```raven
fileprivate class CounterState {
    var value: int = 0
}
```

Extensions and traits are part of Raven’s “interop without surrender” story:
the core language stays compact, while everyday ergonomics can still grow
through normal library surfaces instead of requiring a special compiler feature
for every convenience.

---

## Async and await

```raven
import System.Net.Http.*
import System.Threading.Tasks.*

async func DownloadLength(url: string) -> Task<int> {
    use http = HttpClient()
    let text = await http.GetStringAsync(url)
    return text.Length
}
```

---

## .NET interop

```raven
import System.Console.*
import System.Collections.Generic.*

let numbers: List<int> = [1, 2, 3]
WriteLine(numbers.Count)
```

Raven is designed to work with the .NET ecosystem directly, including BCL APIs, LINQ-style pipelines, tasks, and normal CLR types.

---

## Where to go next

- [Getting started](getting-started.md)
- [Language spec](lang/spec/language-specification.md)
- [Classes and members](lang/spec/classes-and-members.md)
- `samples/cases/`

> This is a living document and may evolve with the language.
