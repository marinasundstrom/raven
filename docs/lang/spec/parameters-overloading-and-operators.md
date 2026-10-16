# Parameters, overloading, and operators

Parameters describe the values a function accepts. Defaults and overloads let
an API support common variations without requiring a different name for every
call, while operators let suitable domain types use familiar expression syntax.

Method, constructor, and accessor parameters are immutable by default. They
behave like `let` bindings: the compiler rejects assignments that attempt to
rebind an ordinary by-value parameter. Ordinary method/function parameters do not
accept `val` or `var`; those binding keywords are reserved for primary-constructor
property promotion. Copy an input into a mutable local when reassignment is needed.
Use the by-reference modifiers below when mutation of caller storage is intended.

```raven
func clamp(min: int, value: int, max: int) -> int {
    // value = ...    // error: parameters are immutable by default
    return Math.Max(min, Math.Min(value, max))
}

func TryParse(text: string, out result: int) -> bool {
    result = 0      // ok: the parameter explicitly opts into mutation
    /* ... */
}
```

Declaring a parameter with `ref`, `out`, or `in` passes the argument by
reference. The callee receives an alias to the caller's storage and callers
supply such arguments with the address-of operator `&expr`. Plain parameters are
readonly. `ref` parameters can be read and assigned, `in` parameters are
readonly aliases, and `out` parameters must be assigned before the method
returns. These modifiers already imply by-reference passing, so their declared
types stay plain: use `ref value: int`, not `ref value: &int`. Explicit
`&Type` parameters remain available when the by-reference type itself is the
intended type annotation.

```raven
func Increment(ref value: int) -> () {
    value = value + 1
}

var total = 41
Increment(ref total)
Console.WriteLine(total) // prints 42
```

Low-level APIs may additionally use `scoped` parameters to restrict managed
reference lifetimes. This is a specialized memory-safety feature; see
[Ref structs and ref safety](ref-structs-and-ref-safety.md#scoped-parameters-and-locals)
for its syntax and escape rules.

## Method overloading

Use overloading when the same operation makes sense for different sets or types
of arguments.

Functions and methods may share a name as long as their parameter counts or
types differ. Member methods may also differ in generic arity: `Read(string)`,
`Read<T>(string)` and `Read<T, U>(string)` are distinct declarations. Return types
alone do not distinguish declarations. Overload resolution selects the best match based on argument
types, `out`/by-ref modifiers, and nullability. Ambiguous calls produce a
diagnostic.

```raven
class Printer {
    func Print(x: int) -> () => Console.WriteLine(x)
    func Print(x: string) -> () => Console.WriteLine(x)
}

Print(42)
Print("hi")
```

## Default parameter values

Use a default value when most callers should be able to omit an optional
argument.

Methods, constructors, and other function-like members may specify default
values for trailing parameters using `= expression`. Optional parameters follow
the same rules as top-level functions: once a parameter provides a default, all
subsequent parameters in the list must also supply defaults. The expression is
restricted to compile-time constants—literals (including `null`), parenthesized
literals, or unary `+`/`-` applied to numeric literals—and the resulting value
must convert to the parameter type using an implicit conversion. When the
expression fails these checks, the compiler reports an error and treats the
parameter as required.

## Operator declarations

Types can define operators when an operation has a clear meaning in ordinary
expression syntax, such as adding two vectors.

Classes and structs can declare overloadable operators using function-style
syntax where the operator token is the function name:
`static func <operator>(...) -> ...`.
Supported static operator tokens are `+`, `-`, `*`, `/`, `%`, `^`, `&`, `|`,
`<<`, `>>`, `>>>`, `true`, `false`, `==`, `!=`, `<`, `<=`, `>`, `>=`, `!`, `~`, `++`, `--`.
Operators mirror methods: they take a parenthesized parameter list, optional
return-type arrow, and either a block body or expression body. The parameter
count must match the chosen operator (unary or binary). Operator declarations
are supported in classes, structs, and extensions.

```raven
class Vector {
    static func +(left: Vector, right: Vector) -> Vector => Add(left, right)
    static func -(value: Vector) -> Vector { /* ... */ }
}
```

Conversions follow the same style:
`static func implicit(value: SourceType) -> TargetType` and
`static func explicit(value: SourceType) -> TargetType`.
These conversion members are resolved using the same lookup rules as other
static members.

When retaining a direct null check, prefer `is null` / `is not null` for strict
identity testing. Raven's analyzer recommends these forms over
`== null`/`!= null` and provides a code fix. This is a compatibility preference
within direct null checks, and neither form refines the checked storage. Typed
conditional bindings and type patterns remain the preferred way to introduce a
usable non-null binding. Pointer-like
comparisons are excluded from that recommendation.

## Explicit compound operators

Classes and structs can customize in-place compound assignment:

```raven
class Counter {
    public var Value: int = 0

    func +=(amount: int) {
        Value = Value + amount
    }
}

var counter = Counter()
let alias = counter
counter += 42 // alias.Value is now 42; counter was not replaced
```

Instance compound operators support `+=`, `-=`, `*=`, `/=`, `%=`, `&=`, `|=`, `^=`, `<<=`, `>>=` and `>>>=`.
These are public instance operators with exactly one by-value parameter and a
unit return type (omit the arrow or write `-> ()`). `static` and non-unit results
are errors. The RHS need not have the receiver's type. Normal implicit argument
conversions and overload ambiguity rules apply.

For a writable local, parameter, field or array element, an applicable explicit
compound operator takes precedence over binary-operator fallback. The receiver
and RHS are each evaluated once. Reference receivers preserve object identity;
value receivers are mutated in their storage. Immutable bindings remain invalid
assignment targets. If no explicit compound candidate is applicable, Raven uses
its existing binary operator and assignment path. Ambiguity is an error, not a
reason to fall back. Ordinary properties and indexers retain the binary-operation
and setter path; a getter result is not an addressable variable.

This follows the [C# 14 in-place compound operator model](https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/proposals/csharp-14.0/user-defined-compound-assignment).
Raven keeps its existing unit-valued assignment expressions; it does not introduce
C#'s receiver-valued assignment result. Binary operators remain independent and
need not be declared alongside compound operators.

Current boundaries: extension and interface compound declarations, virtual/override
compound operators and complete checked-context lowering remain implementation gaps.
Instance increment/decrement, remainder and shift compounds are supported. These
gaps are not permanent target restrictions. No Runtime Contract switch is needed. See the
[compiler lowering contract](../../compiler/compound-operators.md).

## Invocation operator

An invocation operator makes an object callable. This is useful for objects
whose main job is to apply a configured operation.

Declaring a method named `self` makes instances of the type invocable with the
call operator `()`.

```raven
class Adder {
    func self(x: int, y: int) -> int => x + y
}

let add = Adder()
let sum = add(1, 2) // calls self(1, 2)
```

Invocation operators can themselves be overloaded by providing multiple `self`
methods with different parameter signatures.

### Binary operator operand conversions

A candidate binary user-defined operator requires supported implicit conversions
from both operands to its parameter types. The existence of an explicit conversion
is insufficient; write a cast when that conversion is intended. This follows the
[C# applicability rule](https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/language-specification/expressions#1245-binary-operator-overload-resolution).

For example, an operator taking `int` can accept a `byte` operand implicitly, while
a `long` operand requires a cast. Mixed `ulong` and signed integral operands do not
acquire an arithmetic or comparison operator merely because numeric metadata exposes
operator methods. Fixed-width implicit numeric conversions follow the C# table,
including `short` to `int`, `uint` to `long`, and `ulong` to floating-point or
`decimal`. Native-sized conversions remain a separate implementation boundary.
Integer-to-floating conversions can lose precision; accepting an implicit conversion
does not promise exact integer representation in the floating-point result.
See [C# numeric conversions](https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/language-specification/conversions#1023-implicit-numeric-conversions).

### Instance increment and decrement

Classes and structs may declare `func ++()` and `func --()` with no parameters
and unit result. These mutate writable variable storage. Prefix expressions
return the updated value; discarded postfix expressions also prefer these
instance operators. A consumed postfix expression uses the static one-parameter
overload to preserve the previous value. Properties and indexers use the static
overload and setter. Static and instance forms may coexist.

### Truth and conditional logical operators

Declare `static func true(value: T) -> bool` and `static func false(value: T) -> bool`
together on the containing type. They express definitely true and definitely false;
they need not be complements. Conditions (`if` and `while`) use `true` after
ordinary implicit Boolean conversion has been considered.

For `a && b` / `a and b`, `false(a)` decides whether to skip `b`; otherwise the
compiler calls `a & b`. For `a || b` / `a or b`, `true(a)` decides whether to
skip `b`; otherwise it calls `a | b`. The selected bitwise operator must have
identical parameter and result types. The left operand is evaluated once.
Direct `func &&`, `func ||`, `func and` and `func or` declarations are rejected;
migrate them to `&`/`|` plus the truth pair. This aligns both targets with .NET
operator metadata and conditional evaluation conventions.

## Checked operator declarations (development)

Place contextual `checked` after `func`: `static func checked +(a: Number, b: Number) -> Number`,
`static func checked explicit(value: Number) -> int`, or `func checked +=(rhs: Number)`.
Checked forms exist for unary `-`, `++`, `--`; binary `+`, `-`, `*`, `/`; explicit
conversion; and instance `++`, `--`, `+=`, `-=`, `*=`, `/=`. Other forms are rejected.
Each checked declaration requires an ordinary declaration in the same type with
matching parameter passing modes, parameter types, static/instance status and
return type. It can appear later or in another partial declaration.

Checked declarations emit distinct standard .NET method names and preserve them in
symbol import/display. A checked declaration does not implicitly make arithmetic
in its body checked.

### Checked expressions (development)

`checked(expression)` enables overflow checking for built-in integer binary `+`,
`-` and `*`. `unchecked(expression)` restores ordinary arithmetic. The nearest
context wins; context does not flow into nested function bodies or called methods.
Floating-point arithmetic retains its ordinary behavior.

For user-defined binary `+`, `-`, `*` and `/`, a checked context first resolves
checked candidates. If none applies, ordinary candidates are used. Ambiguous
checked candidates remain an error; fallback must not hide ambiguity. Unchecked
expressions select ordinary operators. These rules also apply to imported methods.

This first slice checks integer overflow at runtime. Checked blocks, constant
arithmetic evaluation, checked explicit conversions and mutation operators remain gaps.
Checked casts/explicit conversions, compound assignments and increment/decrement are diagnosed with RAV0373, rather than lowered
unchecked. This includes nonnumeric casts for now. Use an explicit inner
`unchecked(...)` only when ordinary behavior is intended.
Lifted nullable and pointer arithmetic in checked contexts are also diagnosed.
Checked binary arithmetic in expression trees is not yet supported. Neither target needs
a new Runtime Contract setting; native emission is qualified separately.


Checked unary `-` selects `op_CheckedUnaryNegation` when applicable, otherwise the
ordinary unary operator. Ambiguity is not a reason to fall back. For built-in
signed `int`/`long` negation, the minimum value overflows; small integers retain
normal promotion to `int`. Floating-point negation retains IEEE behavior, including
negative zero. Existing unsigned/native-integer unary admission is unchanged.
Nested `unchecked(...)` restores ordinary negation. On .NET, checked unary negation
in expression trees lowers to `NegateChecked`. Compile-time arithmetic evaluation
remains a separate gap.
