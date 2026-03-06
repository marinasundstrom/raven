# Raven

Raven is a modern, expression-oriented programming language for .NET.
It favors explicit flow, clear mutability, and low-ceremony syntax while preserving strong interoperability with the .NET ecosystem.

Raven is built around a few core ideas:

- Expressions first
- Explicit mutability (`val` vs `var`)
- Explicit recoverable flow (`Result`/`Option`)
- Pattern matching as a first-class branching tool
- Public-by-default type members

> Raven has no `void`; it uses `()` (`unit`).

---

## Hello, World

```raven
import System.Console.*

WriteLine("Hello, World!")
```

Top-level statements are supported. Executable file-scope code must appear before type declarations.

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

---

## Bindings and mutability

- `val` is immutable.
- `var` is mutable.

```raven
val name = "Raven"
var count = 0
count = count + 1
```

---

## Expressions and matching

`if` and `match` are commonly used in expression position.

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

---

## Result and Option

Raven encourages recoverable flow as data.

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

Raven supports pipeline-friendly methods on `Result` and `Option` so transformations and fallbacks stay in one straight line:

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

---

## Functions and lambdas

```raven
func Add(a: int, b: int) -> int => a + b

val addA = (x: int) => x + 42
val result = addA(1)
```

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

```raven
class Counter(private var count: int = 0) {
    func Increment() -> () {
        count += 1
    }

    val Count: int => count
}
```

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

---

## Where to go next

- [Getting started](getting-started.md)
- [Language spec](lang/spec/language-specification.md)
- [Classes and members](lang/spec/classes-and-members.md)
- `samples/cases/`

> This is a living document and may evolve with the language.
