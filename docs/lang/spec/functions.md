# Functions

Raven uses a unified function model for callable declarations and callable
values. A named `func` declaration can appear as a top-level function at
namespace scope, as a member of a type where it is a method, or as a statement
inside a block where it is a local function. Top-level functions are implicitly
static; methods follow the usual member rules for instance and static receivers.
Local functions are scoped to the containing body and may capture context from
enclosing scopes.

Function expressions are the expression form of the same concept. They produce
function values that can be assigned, passed, returned, or converted to
compatible delegate types. Lambda syntax is the shorthand function-expression
form and is described in the function-expression section below.

The rules in this section cover shared callable syntax and behavior. See
`Top-level functions and constants` for namespace placement and import rules,
`Classes and members` for method-specific member rules, and `Function
expressions` for the lambda and delegate-conversion form.

### Function declarations

```raven
func Foo(a: int, b: int) -> int {
    a + b
}
```

Arrow bodies are allowed:

```raven
func add(a: int, b: int) -> int => a + b
```

Function declarations (including local `func` statements) may carry declaration
attributes with the same bracket syntax as methods:

```raven
[Trace]
func compute(x: int) -> int => x * 2
```

Return-targeted attribute lists are also supported on functions:

```raven
[return: MaybeNull]
func find(name: string) -> string { /* ... */ }
```

Attribute target prefixes are validated by declaration context:

* `[assembly: ...]` is only valid as a compilation-unit attribute list (before
  top-level functions and constants).
* `[module: ...]` is only valid as a compilation-unit attribute list and applies
  to module metadata.
* `[type: ...]` is valid on type declarations and applies to the declared type
  metadata target (`class`, `struct`, `interface`, `enum`, or `delegate`).
* `[method: ...]` is valid on function and method declarations. On a class,
  struct, or record declaration with a primary constructor, it applies to the
  synthesized primary constructor.
* `[return: ...]` is only valid on callable return positions (function/method
  return metadata).
* `[param: ...]` and `[parameter: ...]` are valid on parameter declarations.
* `[property: ...]` is valid on property declarations.
* `[field: ...]` is valid on field declarations. On a property or event
  declaration with a synthesized backing field, it applies to that backing
  field rather than to the property or event metadata.
* `[event: ...]` is valid on event declarations.

If a target prefix is syntactically recognized but not valid in that position,
the compiler reports an attribute-target diagnostic.

### Parameters

Function, method, and accessor parameters use the `name: Type` syntax. Parameter
names are required and participate in overload resolution alongside their types
and any `ref`/`out` modifiers.

`val`/`var` binding keywords are **not valid** on ordinary function, method,
operator, indexer, or accessor parameters. The only parameter context where
`val`/`var` is valid is primary-constructor parameter promotion.

Lambda parameter lists use the same parameter form and default-value rules.
Parameters in function-like declarations may include attribute lists. This
applies to functions, methods, constructors, delegates, accessors, and lambdas.

Parameters may provide a default value using `= expression` after the type. A
parameter with a default value is optional when invoking the function: callers
can omit that argument and the compiler supplies the stored constant instead.
Only trailing parameters may be optional; omitting an argument fixes the default
for that position and all following parameters must also declare defaults.

```raven
func greet(name: string, punctuation: string = "!") {
    Console.WriteLine("Hello, ${name}${punctuation}")
}

greet("Raven")          // prints "Hello, Raven!"
greet("Raven", "!!!")    // caller-provided punctuation wins
```

Default expressions must be compile-time constants: literals (including `null`),
parenthesized literals, and unary `+`/`-` applied to numeric literals. The value
must convert to the parameter type using an implicit conversion. Nullable value
types accept `null` defaults; other value types require a literal of the
underlying type. Reference-type parameters accept `null` defaults.

When importing methods from other assemblies, Raven also recognizes optional
parameters surfaced through metadata. Parameters marked optional with a stored
default constant or with `System.Runtime.InteropServices.DefaultParameterValueAttribute`
and `System.Runtime.InteropServices.OptionalAttribute` participate in overload
resolution just like source-declared defaults. Omitted positional and named
arguments are synthesized from those metadata-provided constants, after applying
the same constant-conversion rules as source defaults. If only
`System.Runtime.InteropServices.OptionalAttribute` is present, Raven will use the
parameter type’s CLR default value when materializing an omitted argument,
emitting `default(T)` for value types that lack a literal representation.

Method overloads may also be influenced by
`System.Runtime.CompilerServices.OverloadResolutionPriorityAttribute`. When two
or more applicable candidates belong to the same overload set, Raven keeps the
highest-priority candidates before applying its normal overload-comparison
rules. This matches the intended .NET/C# interop behavior for both Raven source
methods and imported metadata methods.

### Generic functions and methods

Functions, methods, types, and local functions may declare one or more **type parameters**
using a type parameter list written after the declaration name:

```raven
func identity<T>(value: T) -> T { value }
class Box<T> { value: T }
```

Each type parameter introduces a distinct generic placeholder type. Type parameter names
must be unique within the same type parameter list. A type parameter is in scope within:

* the remainder of the type parameter list
* parameter types
* return types
* constraint clauses
* the declaration body

Type parameter lists follow the same rule: commas may be written explicitly,
but a newline is also a valid separator between adjacent type parameters.

#### Variance

Where permitted by the enclosing declaration, a type parameter may be annotated with
variance:

* `out T` — covariant
* `in T` — contravariant

Variance annotations affect assignability of constructed generic types and are validated
by the compiler according to the rules of the enclosing declaration kind.

#### Inline constraints

A type parameter may declare **inline constraints** using a colon immediately following
its name:

```raven
func inner<T: struct>(value: T) -> T { value }
func map<T: class, U>(value: T) -> U { /* ... */ }
```

Inline constraints are syntactic sugar for an equivalent `where` clause on the same type
parameter:

```raven
func inner<T>(value: T) -> T where T: struct { value }
```

The compiler normalizes inline constraints and `where` clauses into the same internal
constraint representation.

#### Constraint source rule

For a given type parameter, constraints must be specified using **exactly one** of:

* inline constraints (`T: ...`)
* one or more `where` clauses targeting that parameter

Specifying constraints for the same type parameter using both forms is a compile-time
error.

#### Constraint forms

Each constraint in a constraint list must be one of the following:

* `class` — reference type constraint
* `struct` — non-nullable value type constraint
* `notnull` — non-null constraint
* `unmanaged` — unmanaged value type constraint
* a base class type
* an interface type
* `new()` — public parameterless constructor constraint

Constraints are **conjunctive**: all listed constraints must be satisfied.

The following restrictions apply:

* At most one of `class` or `struct` may appear.
* At most one base class constraint may appear.
* Any number of interface constraints may appear.
* `new()` may appear at most once.
* Duplicate constraints are not permitted.

Violating any of these rules produces a compile-time diagnostic.

#### Constraint ordering

When written, constraints should appear in the following order:

1. `class` or `struct`
2. base class
3. interfaces
4. `new()`

The compiler may diagnose violations of this ordering for consistency.

Functions—including methods declared inside types—may introduce type parameters
by placing `<...>` after the function name. Each type parameter can be used in
the parameter list, return type, and body just like any other type annotation.

```raven
func identity<T>(value: T) -> T { value }

val number = identity(42)         // inferred T = int
val text = identity<string>("hi")
```

Call sites may omit explicit type arguments when inference can determine a
unique solution from the arguments and expected return type. When inference
fails—for example, because multiple type choices satisfy the call—the type
arguments must be provided explicitly.

Method declarations use the same syntax, and local functions follow the exact
rules when they introduce type parameters inside another body:

```raven
class Cache {
    static store<T: class>(value: T) { /* ... */ }
}

Cache.store(System.Text.StringBuilder())   // inference picks T = System.Text.StringBuilder
Cache.store<string>(null)                  // explicit type argument when passing null
```

Type parameter constraints mirror those on generic types. After the colon, list
`class`, `struct`, or specific base/interface types that each argument must
implement. Constraints are conjunctive: every listed requirement must be
satisfied. The `struct` constraint excludes nullable value types, while `class`
admits reference types (including nullable references). Violating a constraint
produces a diagnostic identifying the failing type argument and the unmet
requirement.

### Local functions

Functions may be declared as statements inside other functions, methods, and
block bodies. Such a function is scoped to its containing body and can capture
local variables, parameters, and `self` from enclosing scopes. Local functions
support the same generic syntax and constraints as top-level functions. Place
an optional type parameter list after the function name and declare
constraints using the `:` syntax when needed.
When no instance receiver is available (for example inside `static` members or
`static func` local functions), using `self` reports `RAV2801`.

```raven
func outer() {
    func inner<T: struct>(value: T) -> T { value }

    val y = inner(2)
    val point = inner((x: 1, y: 2))
}
```

Bodies may likewise declare local helper `class`, `struct`, `record`, and
`enum` types when the type should remain encapsulated to that body. Local type
declarations participate in body-local name lookup for the entire containing
body, but the declared type is not exposed as a surrounding namespace or outer
type member in Raven source.

### Async functions

The `async` modifier may appear on top-level functions, methods, and local
functions. An async declaration opts the body into asynchronous control flow so
`await` expressions can suspend and resume execution. When no return type is
annotated, async declarations default to `System.Threading.Tasks.Task`
(`unit`-returning async body). The compiler does not infer `Task<T>` from
omitted return annotations.

Async functions with an explicit return type must annotate one of the supported
task shapes: `System.Threading.Tasks.Task` or `System.Threading.Tasks.Task<T>`.
Annotating any other type produces a diagnostic, and the compiler continues
analysis as though the return type were `Task`. This rule applies uniformly to
methods, top-level functions, and local functions declared inside other
bodies. Property and indexer accessors may also carry `async`; getters must
expose a task-shaped return type to remain valid, while setters may await
asynchronous work before storing values.

Diagnostic analyzers may still suggest adding an explicit return type annotation
based on observed body shape; such suggestions are advisory and do not change
the language binding rule above.

Async declarations support both block bodies and expression bodies. Every
`return` inside an async declaration completes the task produced by the method.
For `async Task<T>` block bodies, a trailing expression statement is treated as
an implicit return value and must convert to `T`.
For `async Task` members each `return` statement must omit the expression;
falling off the end of the body is equivalent to `return;`. For `async Task<T>`
members, return expressions must convert to `T`.

Returning an existing task instance such as `Task.CompletedTask` is not
permitted inside an `async Task` body. Authors must `await` the task to observe
its completion instead of returning it directly. Attempting to return an
expression from an `async Task` member produces a diagnostic that mirrors the
behavior of C# (error RAV2705). Exceptions that escape before the first `await`
propagate directly to the caller. Once asynchronous execution begins, `await`
unwrapped exceptions rethrow when the task is awaited, matching .NET's
observable behaviour.

### Function expressions

Unnamed function expressions support explicit and shorthand forms:
`func (x: int) => x + 1`, `func (x: int) { x + 1 }`,
`async func (x: int) => x + 1`, `static func (x: int) => x + 1`,
and `(x: int) => x + 1` (or `x => x + 1`). The shorthand form is valid
anywhere the equivalent explicit function expression is valid.

Function expressions start with either a parenthesized parameter list
or a single identifier, optionally followed by a return-type arrow. Expression
bodies use `=>`; `func`-introduced block bodies may omit `=>` and use
`func (...) { ... }`. Function expressions may also use modifier forms
`async func`, `static func`, and `static async func`. Function expressions may
appear wherever a function value is expected.

```raven
val addA = func (x: int) => x + 42
val addB = func (x: int) {
    x + 42
}
val addC = x => x + 42
```

Function expressions may optionally declare a local identifier:
`func Fib(n: int) => Fib(n - 1)`. This identifier is visible only inside the
function-expression body. It does not declare a surrounding local/member name;
the emitted backing method name remains compiler-generated.

#### Captured variables

When a function expression or local function statement (`func`) references a
local defined in an outer scope, the compiler lifts that symbol into shared
closure storage so both scopes observe the same value. Each capturing lambda or
local function materializes a synthesized closure class that stores the body as
an instance method and exposes fields for every captured symbol. Reads and writes
in any scope access those fields directly, so mutating a `var` binding after
creating a lambda immediately affects all delegates that captured it. Capturing
`self` produces a reference to the enclosing instance, and capturing parameters
preserves the argument value from the invoking scope. Nested lambdas reuse the
closure instances produced by their enclosing scopes so that captures shared
across multiple lambda layers continue to reference the same storage locations.
Non-capturing lambdas are emitted using the same closure-carrier convention
(with zero capture fields) so lambda emission remains uniform across contexts.
Synthesized lambda method names follow C#-style metadata naming (`<Method>b__...`).
`static func` declarations do not capture enclosing state, and `self` is not
available in static contexts (`RAV2801`). Static function expressions are also
non-capturing; attempting to capture an outer local or parameter reports
`RAV2204`.

`base` is available only in instance members of classes with a base class. It
produces a receiver typed as the direct base class. Invoking a member through
`base` emits non-virtual dispatch to the selected base member, so an override on
the current class is bypassed. `base` is not valid in interfaces, static
members, or contexts without a class base receiver (`RAV2802`). Explicit
interface implementation member access is a separate qualified-member form, for
example `IBase.Member()`, and is not modeled as a `base` expression.

Parenthesized function expressions may place attribute lists immediately before the
parameter list as shorthand. Leading lists are applied contextually:
non-targeted attributes are applied to the first parameter, while
`[return: ...]` lists are applied to the function expression return type.

```raven
val parse = [FromBody](content: string) => content
```

Function-expression parameters may also declare default values using the same trailing
optional-parameter rules as functions and methods.

```raven
val format = (name: string, age: int = 1) => "$name:$age"
```

Function-expression parameter types are optional when the expression is converted to a known
delegate type. The compiler infers the parameter types (and any `ref`/`out`
modifiers) from the delegate's `Invoke` signature and converts the body to the
delegate's return type. If no delegate context is available, diagnostic
`RAV2200` is reported and explicit parameter annotations are required.

Parenthesized function-expression parameter lists also support destructuring
patterns as parameter entries:

* **positional deconstruction** (tuple/`Deconstruct` style), for example `((a, b))`
* **sequence deconstruction** (collection style), for example `([head, ..tail])`

This is primarily target-typed and inference-driven: the underlying parameter
type still comes from the delegate context, and destructuring is then applied
inside the lambda body.

```raven
val pickSecond: ((int, string)) -> string = ((a, b)) => b
val sumTail: (int[]) -> int = ([head, ..tail]) => head + tail[0]
```

Nested deconstruction is recursive in parameter patterns. Positional and
sequence forms may be freely nested as long as each nested segment is
compatible with its inferred input type:

```raven
val project: (((int, string), int[])) -> string =
    (((id, name), [head, ..tail])) => "$id:$name:$head:${tail.Length}"
```

For sequence deconstruction in lambda parameter lists, `..name` and JavaScript-
style `...name` are both accepted as rest syntax.

When a destructuring parameter omits per-element binding keywords, elements are
bound as immutable (`val`) by default. Compatibility is still validated against
the inferred parameter type; non-deconstructable inputs produce the same
deconstruction diagnostics as other pattern-based bindings (for example
`RAV0132`).

Parameters themselves do not carry binding keywords in symbol display/tooling
representations. Hover/signature text renders them as `name: type` (or
`params name: elementType` for collector parameters). In primary-constructor
contexts, `val`/`var` continues to mean promotion to a property rather than a
mutable parameter binding.

Function expressions are target-typed: the same function expression may be assigned to, passed
to, or returned as any compatible delegate type. Compatibility is determined
solely by the delegate's `Invoke` signature (parameter types, `ref`/`out`
modifiers, and return type). Delegate types themselves are **not** implicitly
convertible between one another even when their signatures match; preserving
delegate identity requires an explicit cast when converting from one delegate
type to another.

### Function values and method references

Functions and methods are first-class values. Referencing a function or method
name without invoking it produces a delegate that can be stored, passed around,
or invoked later. The compiler picks an appropriate delegate type using the
same target-typing rules that guide overload resolution. In a binding such as
`val` or `var`, the initializer (or an explicit type annotation) supplies that
context so the delegate type—and corresponding overload—can be determined.
When no delegate context is available, diagnostic `RAV2201` is reported and the
method must either be invoked directly or annotated with a delegate type.

```raven
val writeLine: (string) -> () = Console.WriteLine
writeLine("Hello from Raven!")
```

If the referenced member has no overloads, the compiler may omit the
annotation and still infer the delegate type from that unique signature.

When the referenced method defines multiple overloads, Raven does **not** allow
an unannotated binding to rely solely on type inference; such declarations are
ambiguous and produce diagnostic `RAV2202`. To disambiguate, provide the
delegate type explicitly or use another context with a well-defined target
type.

```raven
val writeLine = Console.WriteLine             // error: overloaded method group
val writeLine: (string) -> () = Console.WriteLine // ok
```

Passing `Console.WriteLine` as an argument to a parameter of function type `(string) -> ()`
(equivalent to `System.Action<string>`) likewise selects the `string` overload without requiring
an explicit annotation at the call site. If no overload matches the target
delegate's signature, diagnostic `RAV2203` is produced.

When the selected method is compatible with the target delegate only through
implicit parameter/return conversion (for example, delegate parameter
`KeyValuePair<string, int>` forwarded to method parameter `object`), Raven
synthesizes an internal compiler-generated bridge method and binds the delegate
to that bridge. The bridge performs the required implicit conversions (including
boxing for value types) before invoking the selected method.

If no compatible delegate type exists in the current context, the compiler
generates one whose signature matches the referenced function or method. The
generated delegate observes the same parameter list (including `ref`/`out`
modifiers) and return type as the source symbol so the resulting value behaves
identically to directly invoking that member. Subsequent uses of the same
signature within the compilation reuse the synthesized delegate.

Instance method references capture their receiver automatically. Evaluating
`self.Member` as a value stores the current instance alongside the referenced
method so later invocations execute against the same object:

```raven
class Counter {
    value: int = 3

    func Increment(delta: int) -> int { self.value + delta }

    func Run() -> int {
        val increment = self.Increment
        increment(7) // returns 10
    }
}
```

Method references may be passed directly to parameters of delegate type. The
overload chosen for the receiving method is the one whose delegate parameter
matches the referenced method's signature:

```raven
func Run(action: System.Action<string>) { action("ready") }
func Run(value: string) { Console.WriteLine(value) }

Run(Console.WriteLine) // selects the Action<string> overload
```

When a referenced method's signature requires a delegate that doesn't already
exist—such as one with `ref`/`out` parameters—Raven synthesizes an internal
delegate type and uses it as the expression's type. These delegates behave like
framework-provided types and faithfully propagate modifiers:

```raven
class Accumulator {
    static func TryAccumulate(ref state: int, out doubled: int) -> bool {
        state = state + 1
        doubled = state * 2
        true
    }

    static func Execute(value: int) -> int {
        val callback = Accumulator.TryAccumulate
        var current = value
        var doubled = 0

        callback(&current, &doubled)
        current + doubled // evaluates to 12 when value is 3
    }
}
```
