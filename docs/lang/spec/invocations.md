# Calls and trailing blocks

```raven
Foo(1, 2)
Console.WriteLine("Test")
```

The `()` call operator invokes a function-valued expression. If the target
expression's type defines an invocation operator via a `self` method, that
member is invoked instead.
Invocation operators can be declared on classes or interfaces. Class
declarations may mark them `virtual` or `abstract` to support overrides.

When the target has optional parameters, omitted trailing arguments are filled
in using the defaults declared on the parameter list. The supplied arguments are
matched positionally before defaults are considered.

Parameters may also be declared as collectors using a trailing `...` after the
parameter type. A collector parameter must be the trailing parameter.

For convenience syntax (`items: T ...`), the declared parameter type defaults to
`IList<T>` in semantic binding.

If you need to control the exact collection type, use explicit `params` syntax,
for example `params items: int[]` or `params items: IEnumerable<int>`.

At call sites, any extra positional arguments are packed into the collector.
Calls may also use `...expr` in
argument position to expand an existing sequence into the collector.

Raven also supports an explicit collector marker with `params`:
`params items: IEnumerable<int>`. The `...` form and `params` are mutually
exclusive on the same parameter.

Arguments may also be written with an explicit name using the `name: expression`
syntax. Named arguments may appear in any order, and each one must match a
parameter declared by the target. After a named argument is used, any remaining
positional arguments must correspond to parameters that have not already been
specified by name and that occur after the right-most named argument. Duplicate
or unknown names cause overload resolution to reject the candidate. Named
arguments are supported in function invocations, object creation, and
constructor initializers. Attribute argument lists also support named
arguments, but they use the same `name: expression` form only; the legacy
`name = expression` C# style is not valid Raven syntax.

```raven
func makePoint(x: int, y: int, label: string = "origin") -> string {
    return "$label: ($x, $y)";
}

func sum(items: int ...) -> int {
    return items.Length;
}

func sum2(params items: int[]) -> int {
    return items.Length;
}

val swapped = makePoint(y: 2, x: 1);
val mixed = makePoint(3, label: "axis", y: 0);
val invalid = makePoint(x: 1, 2);  // error: positional argument cannot follow `x:`
val a = sum(1, 2, 3);
val xs = [4, 5];
val b = sum(...xs);
```

The compiler binds each named argument to its declared parameter. The call to
`makePoint` named `mixed` demonstrates that positional arguments may precede the
first named argument, while the `invalid` call is rejected because it attempts
to supply a positional argument (`2`) after specifying `x` by name.

### Trailing blocks

A **trailing block** is syntactic sugar for calling a callable whose final
parameter is a function. The block is written after the callee's argument list
and is supplied as the final argument. This form is ordinary
invocation syntax; it can be used for callbacks, command handlers, resource
scopes, and declarative DSLs.

```raven
func use(action: () -> int) -> int {
    return action()
}

val x = use {
    42
}
```

When the call has no parenthesized arguments, the empty argument list may be
omitted:

```raven
val window = Window {
    StackPanel()
}
```

Parenthesized arguments keep the normal named-argument and default-parameter
rules. The trailing block still supplies the final function-typed parameter,
so earlier optional parameters may be omitted:

```raven
func StackPanel(
    orientation: Orientation = .Vertical,
    spacing: double = 0.0,
    [Builder<UiBuilder>] content: (() -> UiNode)? = null
) -> UiNode {
    ...
}

StackPanel(spacing: 8.0) {
    Label("Inbox")
}
```

In this shape, omitting `content` means the parameter receives its default
value (`null`). Supplying `{ ... }` means the final closure argument is present,
even when the block is empty.

The body of the trailing block is a normal Raven block. Statements inside the
block are ordinary statements; assignments such as `Name = value` are not
initializer entries. Brace trailers after expressions are therefore not object
initializers. Use `Type with { ... }` for object initialization and `value with
{ ... }` for non-destructive updates.

When the selected function parameter accepts input parameters, the trailing block
must declare a parameter clause immediately after the opening brace followed by
`=>`. A single parameter may be written directly, and multiple or destructured
parameters use the normal parenthesized function-expression parameter list.
The parameterized block lowers to the same closure form as an ordinary lambda
argument and participates in overload resolution using its declared arity. A
trailing block without a parameter clause does not introduce implicit `$0`/`$1`
parameters or an `it` alias.

Trailing blocks are function expressions. Async trailing-block closures are not
currently supported; use an explicit async lambda argument until this gap is
closed.

```raven
func Apply(value: int, transform: int -> int) -> int {
    return transform(value)
}

func Combine(left: int, right: int, transform: (int, int) -> int) -> int {
    return transform(left, right)
}

val next = Apply(41) { (value: int) =>
    value + 1
}

val sum = Combine(20, 22) { (left: int, right: int) =>
    left + right
}
```

#### Receiver trailing blocks

If the selected final function-typed parameter is annotated with `[Receiver]`,
an unparameterized trailing block is bound as a receiver closure. The target
delegate must have one input parameter. Inside the block, instance members of
that receiver parameter are available as unqualified names.

```raven
class ConfigBuilder {
    var Name: string = ""

    func Activate() -> () {
    }
}

func Config([Receiver] configure: ConfigBuilder -> unit) -> Configuration {
    val builder = ConfigBuilder()
    configure(builder)
    return Configuration(builder.Name)
}

val config = Config {
    Name = "Foo"
    Activate()
}
```

The block above is equivalent to passing a one-argument function and qualifying
receiver member access through that argument:

```raven
val config = Config(func (receiver: ConfigBuilder) => {
    receiver.Name = "Foo"
    receiver.Activate()
})
```

The receiver is considered only after ordinary lexical lookup. Locals,
parameters, imports, and other normal symbols with the same name win over
receiver members. If no ordinary symbol matches, the nearest enclosing receiver
closure supplies the implicit receiver for member lookup. Explicitly
parameterized trailing blocks do not use receiver lookup; write the parameter
and qualify member access normally.

`[Receiver<T>]` specifies the receiver member lookup type explicitly. For a
normal receiver closure, the delegate's single input parameter must be
assignable to `T`; the closure still receives the delegate parameter value, but
unqualified receiver member lookup is performed on `T`. This lets APIs expose a
narrow receiver contract while passing a more concrete implementation object to
the closure.

Trailing blocks are supported for:

* function and method calls,
* constructor calls,
* extension method calls, after extension lookup selects an extension candidate.

Examples:

```raven
func Around(name: string, body: () -> ()) -> () {
    WriteLine("before " + name)
    body()
    WriteLine("after " + name)
}

Around(name: "method") {
    WriteLine("body")
}

val command = Command(name: "constructor") {
    WriteLine("run")
}

"value".Trace {
    WriteLine("extension callback")
}
```

The complete runtime sample is available in
[`samples/runtime/trailing-blocks-basic.rav`](https://github.com/marinasundstrom/raven/blob/main/samples/runtime/trailing-blocks-basic.rav).

#### Trailing block resolution

Resolution follows ordinary overload resolution with one additional argument:

1. The callee and all parenthesized arguments are bound normally.
2. If a trailing block is present, the compiler creates an unbound
   closure from the block and supplies it to the final visible parameter of the
   candidate callable.
3. Overload resolution accepts only candidates whose final visible parameter can
   receive that closure. Earlier parameters may be supplied positionally,
   supplied by name, or omitted when they have defaults.
4. If the callee has no parenthesized argument list, the call is treated as a
   zero-argument call plus the appended trailing closure.
5. For constructor calls, the selected constructor must accept the appended
   closure. A parameterless constructor alone does not make `Type { ... }`
   valid.
6. For extension method calls, extension lookup first gathers applicable
   extension candidates for the receiver. The trailing closure then participates
   in overload resolution like any other final argument. If an extension is
   selected, the receiver is lowered as the leading argument to the extension
   method.
7. Parameterized trailing blocks filter function-typed candidates by the
   declared parameter count before final overload selection.
8. If the selected closure parameter has `[Receiver]` and the trailing block has
   no explicit parameter clause, the compiler synthesizes one closure parameter
   from the target delegate input parameter and makes that parameter the block's
   implicit receiver. If the attribute is `[Receiver<T>]`, member lookup uses
   `T` after verifying that the target delegate parameter is compatible with
   `T`.

If no candidate can accept the appended closure, overload resolution fails and
the call is rejected.

#### Builder-backed DSL blocks

If the selected closure parameter is annotated with `[Builder<T>]`, the trailing block is bound as a builder block. In this context, `T` is the **result builder**: it controls how the compiler lowers component expressions and control-flow constructs into a block result. The attribute is recognized by the builder type argument; the standard `BuilderAttribute<T>` is intended to be provided by Raven.Core, while compiler bootstrapping code may define an equivalent attribute shape. Expression statements and return expressions become builder components, components are adapted through `BuildExpression` when needed, and the final component list is combined through `BuildBlock`. `if` without `else` requires `BuildOptional` only when its body contributes builder components, `if` with `else` requires `BuildEither` only when at least one branch contributes builder components, and `for` requires `BuildArray` only when the loop body contributes builder components. Without `[Builder<T>]`, the block remains an ordinary closure.

`[Builder<T>]` does not imply receiver lookup. Builder-backed blocks and
receiver trailing blocks are separate contracts: the result builder collects and
combines component expressions, while the receiver supplies the implicit member
access target. A parameter may opt into both contracts by combining
`[Builder<TBuilder>]` with `[Receiver<TReceiver>]` on a zero-argument closure
parameter. In that form, `TReceiver` is often a **receiver builder**: a current
component builder that exposes configuration properties and methods in the block
and produces that component's sub-result from the built child content. Library
authors may name this receiver type `WindowBuilder`, `RouteBuilder`,
`FormBuilder`, or any domain-specific name; the compiler role is that it is the
block receiver.

For combined builder/receiver blocks, the compiler creates a receiver instance
using an accessible parameterless constructor, makes its members available in
the block, and passes it to `TBuilder.BuildFinalResult(component, receiver)`.
Receiver-backed builder blocks therefore require a compatible two-argument
`BuildFinalResult` method so the receiver builder can produce the final
sub-result and configuration cannot be silently ignored.

```raven
func Window(title: string = "Untitled", [Builder<UiBuilder>] content: (() -> UiNode)? = null) -> UiNode {
    ...
}

Window(title: "Tasks") {
    StackPanel(spacing: 8.0) {
        Text("Inbox")
    }
}
```

When a component needs a receiver builder in addition to normal call arguments,
the final closure parameter may combine `[Builder<TBuilder>]` and
`[Receiver<TReceiver>]`:

```raven
func Window([Builder<UiBuilder>, Receiver<WindowBuilder>] content: () -> UiNode) -> UiNode {
    return content()
}

Window {
    Title = "Tasks"

    if danger {
        Title = "DANGER!"
    }

    VStack {
        Text("Inbox")
    }
}
```

This distinction allows the same trailing-block syntax to model different API conventions. A container constructor can use a builder-annotated closure for child content, while a leaf control or command API can use an ordinary final closure as an action handler:

```raven
Window {
    Button(text: "OK") {
        Submit()
    }
}
```

In this example, the outer block is a builder block only if `Window` selects a `[Builder<T>]` closure parameter. The inner block is an ordinary `() -> ()` argument if `Button` selects a final non-builder closure parameter.

Object initializers, required-member checks, and non-destructive `with`
expressions are documented in [Object initialization and copying](object-initialization-and-copying.md).
