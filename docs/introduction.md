# Raven

Raven is a .NET language built around a few clear ideas: expression-oriented code where values help, statement forms where effects are clearer, explicit mutability, explicit pattern bindings, and pragmatic interop with the .NET ecosystem.

It takes visible inspiration from Swift, Rust, and F#, but Raven is not trying to imitate any one of them exactly. Its current shape is defined more by consistency than by novelty:

- expression-oriented code with statement forms where control flow is clearer
- explicit mutability with `val` and `var`
- explicit pattern bindings, including deconstruction and control-flow patterns
- `Option<T>` and `Result<T, E>` as ordinary control-flow shapes
- records, primary constructors, unions, and structural patterns
- direct use of .NET libraries, collections, async APIs, and IL-based tooling

> Raven has no `void`; it uses `unit`, written as `()`.

---

## Hello, World

```raven
import System.Console.*

WriteLine("Hello, World!")
```

Top-level statements are supported. Top-level type declarations are hoisted for binding, so scripts can place helper types near the file-scope code that uses them.

---

## A quick taste

```raven
import System.*
import System.Console.*

func Main() -> () {
    val inputs = ["10", "3", "abc", "42"]

    val message = ProcessNumbers(inputs) match {
        Ok(_) => "All numbers processed"
        Error(val err) => "Failed: $err"
    }

    WriteLine(message)
}

func ProcessNumbers(inputs: string[]) -> Result<(), string> {
    for text in inputs {
        val no = ParseInt(text)?

        val line = no match {
            10 => "Ten exactly!"
            val value => "Parsed: $value"
        }

        WriteLine(line)
    }

    return Ok
}

func ParseInt(text: string) -> Result<int, string> {
    return try int.Parse(text) match {
        Ok(val v) => Ok(v)
        Error(_) => Error("\"$text\" is not a number")
    }
}
```

That example shows the current Raven style in miniature:

- `match` is a normal expression
- `Result` values are handled directly instead of being wrapped in exception-heavy flow
- pattern arms use explicit binding keywords where they introduce new values
- .NET APIs such as `int.Parse` are used directly

---

## Bindings and mutability

- `val` is immutable.
- `var` is mutable.

```raven
val name = "Raven"
var count = 0
count = count + 1

val a = 1, b = 2
var left = 10, right = 20
```

Raven also accepts `let` as an alias for `val`, but current docs and examples prefer `val`.

---

## Expressions and matching

`if` and `match` are commonly used in expression position, but Raven is not expression-only. Loops, disposal, mutation, and early returns remain ordinary statement forms when that keeps intent clearer.

```raven
val label = value match {
    0 => "zero"
    _ => "non-zero"
}
```

```raven
val message = if isAnonymous {
    "Welcome"
} else {
    "Welcome back"
}
```

Raven also leans heavily on patterns as a control-flow surface, not only as a `match` feature:

```raven
if val Person(1, name, _) = person {
    WriteLine(name)
}

for val [first, ...rest] in rows {
    WriteLine(first)
}

val label = input match {
    val Some((x, y)) => "($x, $y)"
    _ => "none"
}
```

The important rule is that pattern bindings stay explicit. In inline and freestanding
patterns, a capture uses a binding keyword. When Raven offers an outer shorthand
form such as `val (...) = expr`, `if val pattern = expr`, `for val pattern in values`,
or `match { val pattern => ... }`, that outer keyword supplies the binding mode for
otherwise bare captures inside the pattern.

There is also an important surface distinction:

- `is`, `match`, `if val pattern = expr`, and `for ... in` pattern targets use the
  general pattern language.
- deconstruction assignment/declaration (`val (...) = expr`, `(...) = expr`,
  `val [...] = expr`, `[...] = expr`) use the deconstruction subset rather than
  every match-only pattern form.

---

## Result and Option

Raven treats recoverable flow as data. In domain code, `Option<T>` is preferred for absence and `Result<T, E>` for expected failures.

```raven
func Divide(a: int, b: int) -> Result<int, string> {
    if b == 0 {
        return Error("Division by zero")
    }
    return Ok(a / b)
}

val value = Divide(10, 2)?
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
    val firstEven = FindFirstEven(values)?
    val quotient = Divide(100, firstEven)?
    return Ok("Result: $quotient")
}
```

### Railroad-style flow with carrier methods

Raven also supports pipeline-friendly methods on `Result` and `Option` so transformations and fallbacks stay in one straight line:

```raven
import System.Linq.*
import System.Collections.Generic.*

val plans = List<RatePlan> {
    RatePlan("NorthStar", 500, 120)
    RatePlan("Oceanic", 450, 150)
}

val requests = List<ShipmentRequest> {
    ShipmentRequest("REQ-1001", "NorthStar", 10, Some("SAVE5"))
    ShipmentRequest("REQ-1002", "Oceanic", 3, None)
}

val summary = BuildQuoteSummary(requests, plans) match {
    Ok(val message) => message
    Error(val err) => "Quote failed: $err"
}

func BuildQuoteSummary(requests: IEnumerable<ShipmentRequest>, plans: IEnumerable<RatePlan>) -> Result<string, string> {
    val request = requests
        .FirstOrError(r => r.Id == "REQ-1002", () => "Request not found")?

    val total = plans
        .FirstOrError(p => p.Carrier == request.Carrier, () => "Rate plan not found")
        .Map(plan => plan.BaseCents + (request.WeightKg * plan.PerKgCents))?

    val promoDiscount = PromoCents(request.PromoCode).UnwrapOr(0)

    return Ok("Quote ${request.Id}: ${total - promoDiscount} cents")
}

func PromoCents(code: Option<string>) -> Option<int> {
    val raw = code?
    val normalized = raw.Trim().ToUpperInvariant()

    return normalized match {
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

val addA = func (x: int) => x + 42
val addB = func (x: int) {
    x + 42
}
val result = addA(1)
```

Raven treats both forms as functions:

- Named function declaration: `func Add(...) -> ...`
- Unnamed function expression: `func (...) => ...`, `func (...) { ... }`, or shorthand `(...) => ...`

In practice, the difference is binding shape:

- Named declarations introduce a member/local function symbol.
- Unnamed function expressions produce a function value that you bind to `val`/`var` or pass as an argument.

Function type signatures use arrow notation:

```raven
val f: (int, int) -> int
val g = func (a: int, b: int) => a + b
```

Unnamed function expressions may omit `func` as shorthand. This is mainly a convenience for higher-order call sites such as LINQ-style APIs:

```raven
val projected = [1, 2, 3].Where(x => x > 1).Select(x => x * 2)
```

Function expressions also support `async`, `static`, and `static async` modifier forms:

```raven
val add = static func (a: int, b: int) {
    a + b
}

val delayedAdd = async func (a: int, b: int) {
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
    Identifier(text: string)
    Number(value: int)
    End
}

func Describe(token: Token) -> string {
    return token match {
        .Identifier(val text) => "id: $text"
        .Number(val value) => "number: $value"
        .End => "end"
    }
}
```

The same general pattern model works across:

- `match`
- `if value is pattern`
- `if val pattern = expr`
- `for ... in` with pattern targets
- deconstruction assignment and declaration

This reuse is intentional. Raven wants one pattern system that scales across value
inspection, branching, iteration, and deconstruction instead of splitting those
features into unrelated syntax families.

At the same time, deconstruction forms stay intentionally narrower than general
pattern-matching forms. They are designed for extraction, not for every kind of
conditional match. That means Raven reuses one pattern model conceptually while
still distinguishing between:

- general matching surfaces such as `is`, `match`, `if val pattern = expr`, and
  `for val pattern in values`
- deconstruction surfaces such as `val (a, b) = expr` and `[head, ..tail] = values`

---

## Extensions

```raven
extension StringExt for string {
    func ToSlug() -> string =>
        self.Trim().ToLowerInvariant().Replace(" ", "-")
}

import MyApp.StringExt.*

val slug = "Hello World".ToSlug()
```

Raven also supports `trait` as an alternate declaration keyword for the same construct. Both spellings participate in the same extension lookup model.

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
For records, positional parameters are promoted by default.

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
    val text = await http.GetStringAsync(url)
    return text.Length
}
```

---

## .NET interop

```raven
import System.Console.*
import System.Collections.Generic.*

val numbers: List<int> = [1, 2, 3]
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
