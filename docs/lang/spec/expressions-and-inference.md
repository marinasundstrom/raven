# Expressions and type inference

### Target typing


Many expressions rely on the type expected by their context, called the **target type**.
For example, the enum shorthand `.B` in `var grade: Grades = .B` uses the declared type `Grades` to resolve the member.

The constructor shorthand `.(...)` also requires a target type. The omitted
member name means "construct the target type", so `let p: Point = .(2, -1)`
binds as `Point(2, -1)`. The same form works in other target-typed contexts
such as constructor arguments and collection elements.

#### Discriminated union case sugar for `unit`

When a discriminated union case carries exactly one payload of type `unit`, Raven permits the case to be written *without* an argument list in expression position. In such contexts, a bare case name is sugar for supplying the sole `unit` value `()`.

```raven
func Save() -> Result<(), Error> {
    return Ok       // sugar for `Ok(())`
}
```

The target-typed member form `.Ok` is also valid when the target type is known.

This rule applies uniformly to **all** discriminated unions, not only `Result` or `Option`. The case must declare exactly one constructor parameter whose type is `unit`; cases with additional parameters or non-`unit` payloads still require an explicit argument list.

This mirrors the pattern-matching rule where a bare case such as `Ok` matches
`Ok(())` when the payload is `unit`, with `.Ok` remaining available as the
target-typed shorthand.

### Type inference

When an expression or declaration omits an explicit type, Raven infers one from
the expression. If multiple different types can flow to a location—through
conditional branches or early `return` statements—the inferred result is the
nearest compatible type for the flow.

```raven
val pet = if flag { Dog() } else { Cat() }
// pet has an inferred compatible type
```

Literal expressions infer the underlying primitive type when used to initialize
`val` or `var` bindings. Literal types are subset types of their underlying
primitive, so a literal like `1` can be used wherever an `int` is expected.
When inference gathers multiple results—such as the branches of an `if`
expression—it keeps literal precision only when required by explicit
annotations; otherwise literals widen to their underlying primitive types.
To retain a literal's singleton type for a single value, an explicit annotation
is required.

```raven
var i = 0       // i : int
val j = 0       // j : int
var k: 1 = 1    // k : 1
```

Control-flow expressions participate in the same inference. An `if` expression
whose branches produce different types infers the nearest compatible type for
those results. Literal branches remain precise only when explicitly annotated.
By default, literal branches widen to their underlying primitive types:

#### Branch type inference

```raven
val x: int = 3
val value = if x > 2 { 42 } else { x }
// value : int

val other: long = 0
val widened = if x > 2 { other } else { 42 }
// widened has an inferred compatible numeric type
```

Each branch contributes its inferred type. Literal branches widen as needed to
produce a compatible inferred result type.

Numeric literals choose an underlying primitive type according to their form
and optional suffix. The default rules are designed to be predictable while
still allowing safe narrowing through implicit *constant* conversions.

#### Integer literals

* **Unsuffixed integer literals** default to `int`.
  * If the value does not fit in `int`, the literal is typed as `long`.
* **Base prefixes**:
  * `0b` / `0B` — binary integer literal
  * `0x` / `0X` — hexadecimal integer literal
* The following suffixes override the default:
  * `b` / `B` — `byte`
  * `l` / `L` — `long`

Unsuffixed integer literals may still convert implicitly to smaller integral
types (such as `byte` or `char`) **when used as constant expressions** and the
value fits in the target type. This mirrors C#’s constant conversion rules and
avoids accidental narrowing for non-constant values.

```raven
val a = 42        // int
val b = 4_000_000_000 // long

val x: byte = 12 // OK: constant int fits in byte
val y: byte = 300 // error: constant out of range

val z = 32b      // explicit byte literal
val n = 10L      // explicit long literal
val bits = 0b1010_0101 // binary int literal
val mask = 0xFF  // hexadecimal int literal
```

#### Floating-point literals

* **Unsuffixed floating-point literals** (those containing a decimal point or
  exponent) default to `double`.
* The following suffixes override the default:
  * `f` / `F` — `float`
  * `d` / `D` — `double`
  * `m` / `M` — `decimal`

```raven
val d = 3.14     // double
val f = 3.14f    // float
val m = 9.99m    // decimal
val e = 1e3      // double
```

Decimal literals do not support exponent notation. Attempting to combine an
exponent with the `m`/`M` suffix produces a diagnostic.

Literal values participate in overload resolution and type inference using
their underlying primitive type. When a literal appears in a context that
supplies a target type, the compiler may apply implicit constant conversions
before considering explicit casts.

Overload resolution applies the same rule: a literal argument converts to its
underlying type when selecting among method overloads. For example,
`Console.WriteLine(1)` binds to `Console.WriteLine(int)` if such an overload
exists, and `Console.WriteLine("test")` chooses `Console.WriteLine(string)`.

Functions and methods without an annotated return type default to `unit`; the
declaration return type is not inferred from body expressions.

Lambdas without an annotated return type infer their result by collecting:

1. the types of all explicit `return` statements, and
2. the final expression of the outer body when that body has a value-producing tail expression.

Expression statements in nested statement blocks (for example, inside `if`/`while`/`for`
statement bodies) do not participate in lambda return-type inference. If no value-returning
path exists, the lambda return type defaults to `unit`.

```raven
val example = (x: int) -> {
    if x > 0 { return x }
    "neg"
}
// inferred return type is context-dependent
```

When a lambda expression is assigned to a binding without an explicit type, Raven
still materialises a concrete delegate. The compiler synthesises an appropriate
`System.Func`/`System.Action` definition using the lambda's parameter types and
the inferred return type (treating `unit` results as actions). Captured
variables participate in the enclosing flow analysis before the delegate type is
constructed, so the lambda observes the same declared type as any other use of
the variable.

```raven
val a = 42
val makeAdder = () => a + 3

makeAdder() // returns 45, makeAdder : System.Func<int>
```

Async function expressions mirror async functions: placing `async` before the parameter
clause (or using `async func`) permits `await` inside the body. When the function expression return type is not annotated
and no delegate supplies one, the compiler wraps the inferred result in
`System.Threading.Tasks.Task<T>` (or `Task` when the body produces `unit`). A delegate
annotation or target type may still specify a concrete `Task` shape, in which case the
function-expression body must evaluate to the awaited result type rather than the task itself.
Annotating an async function expression with a non-`Task` return type is an error.

### Additional type inference rules (normative)

The following clarifications extend the type inference model:

* **Contextual inference**: Raven computes a contextual type based on both expression shape and target type. Inference is bidirectional.
* **Literal arithmetic**: Non-constant operations widen literals to their base type unless constant-folded.
* **Generic inference**: Type argument inference requires a single consistent set of type arguments that satisfies all constraints.
* **Nullability**: Nullable flow follows `T?` rules and safe-navigation propagation.
* **Pattern narrowing**: See [Pattern matching](pattern-matching.md) for how `is` and `match` refine variables.
* **Tuples**: Tuple element names do not affect type identity.
* **Ref/out parameters**: `ref` requires exact type match; `out` contributes to inference of the parameter type.
* **Flow stability**: Variable declarations have a fixed declared type. Narrowings are ephemeral and do not change declared type. Captured variables use the join of all flows.
* **Diagnostics**: When conversion fails, diagnostics should identify the conflicting conversions and why. For overloads, diagnostics should explain alternative selections.
