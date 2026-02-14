You can wire MVVM in a way that keeps the DSL **pure** (it only produces a spec) and pushes all “live wiring” into the **adapter/reconciler**, using a few conventions:

* Treat **`Binding:` / `Bind:`** as *DSL metadata* (not framework properties)
* Treat **events** (Clicked, TextChanged…) as normal props but compiled to delegates
* Keep a per-node **StateBag** to store subscriptions, binding handles, and cached delegates
* On each reconcile, (re)bind only what changed, and dispose bindings when nodes are removed

Below are three practical MVVM levels, from simplest to “real MAUI MVVM”.

---

## 1) Easiest: bind to ViewModel properties directly (compiled lambdas)

This is “MVVM-ish” without `INotifyPropertyChanged` integration. Great as a first step.

### DSL

```raven
class CounterVm {
    public var Count: int = 0
    func Inc() { Count += 1 }
}

func App(vm: CounterVm, [Builder<MauiBuilder>] content: () -> ViewNode) -> View =
    MauiRuntime.Render(vm, content)

val vm = CounterVm()

val view =
App(vm) {
    VerticalStackLayout(Spacing: 12) {
        Label(Text: $"Count: {vm.Count}")
        Button(Text: "Increment", Clicked: () => { vm.Inc(); MauiRuntime.Invalidate(vm) })
    }
}
```

Works, but updates are explicit (`Invalidate`). Not “real” MVVM yet.

---

## 2) Classic MVVM: `BindingContext` + MAUI `Binding` objects (no framework mods)

This is the most MAUI-native.

### DSL surface

Let users write:

```raven
ContentPage(BindingContext: vm) {
    Label(Text: Bind("CountText"))
    Button(Text: "Increment", Command: Bind("IncCommand"))
}
```

Here `Bind("...")` returns a **binding marker value** (a spec object), not a MAUI `Binding`.
The adapter recognizes it and creates a real MAUI `Binding` at runtime.

### Example ViewModel (C# style concepts, Raven-ish)

```raven
class CounterVm : ObservableObject {
    public var Count: int = 0

    public var CountText: string => $"Count: {Count}"

    public val IncCommand = Command(() => {
        Count += 1
        Notify(nameof(Count))
        Notify(nameof(CountText))
    })
}
```

### DSL

```raven
import Microsoft.Maui.Controls.*

val vm = CounterVm()

val view = MauiRuntime.Render(vm) {
    ContentPage(BindingContext: vm, Title: "Counter") {
        VerticalStackLayout(Spacing: 12, Padding: Thickness(16)) {
            Label(Text: Bind("CountText"))
            Button(Text: "Increment", Command: Bind("IncCommand"))
        }
    }
}
```

### What the adapter does

When applying properties:

* If value is `Bind(path)` → set `BindableObject.SetBinding(targetProp, new Binding(path))`
* If `BindingContext:` is provided → set it normally
* Store binding handles in `StateBag` so you can replace/clear them if the binding changes

This is *fully zero-modification* because MAUI already supports bindings via `BindableObject`.

**Pro:** feels exactly like MAUI MVVM
**Con:** binding paths are strings unless you add typed helpers later.

---

## 3) “Typed MVVM”: compile binding expressions to delegates (no string paths)

If you want “Raven-y strong typing”, make `Bind` accept a lambda:

```raven
Label(Text: Bind(vm => vm.CountText))
Button(Command: Bind(vm => vm.IncCommand))
```

This is still MVVM, just with compile-time checking.

### DSL

```raven
val view = MauiRuntime.Render(vm) {
    ContentPage(BindingContext: vm) {
        VerticalStackLayout {
            Label(Text: Bind<CounterVm>(x => x.CountText))
            Button(Text: "Increment", Command: Bind<CounterVm>(x => x.IncCommand))
        }
    }
}
```

### Runtime strategy (two ways)

#### A) Still use MAUI Binding, but infer the path

If you can extract member access chain from the lambda (`x => x.CountText`), you can build `"CountText"` and call MAUI binding normally.

* Works for simple `x => x.Prop1.Prop2`
* Doesn’t work for arbitrary expressions

#### B) Don’t use MAUI Binding; do your own subscription + property set

You subscribe to `INotifyPropertyChanged` on the BindingContext:

* On change, re-evaluate the lambda and set the property directly

This gives you super flexibility (computed expressions), but you’re basically implementing a tiny binding engine.

**Practical hybrid**: do A for member chains, fallback to B for computed lambdas.

---

# How to integrate MVVM into your reconciler (the mechanics)

## NodeInstance StateBag

Each live node instance keeps:

* `BindingContext` value (current)
* `Bindings`: map `(propName) -> bindingHandle`
* `EventSubscriptions`: map `(eventName) -> token`
* `PropertyCache`: last applied values for diffing

So updates are incremental.

## ApplyProperties step (per node)

For each prop assignment `(name, value)`:

1. If name is reserved metadata (`Key`) → ignore for framework
2. If value is a binding marker:

   * if binding differs from previous → clear old binding, attach new
3. Else:

   * if value differs → set property via cached setter

## BindingContext rules

* If node sets `BindingContext:` explicitly → use it
* Else inherit from parent (adapter responsibility)
* When BindingContext changes:

  * rewire all bindings on that subtree (or lazily on next apply)

---

# A clean Raven DSL API surface for MVVM

Minimal set:

```raven
// metadata marker types (in DSL runtime lib)
func Bind(path: string) -> BindingExpr
func Bind<TVm>(expr: (TVm) -> any) -> BindingExpr

// optional: convenience markers
func On(eventName: string, handler: () -> ()) -> EventExpr
func Key(value: any) -> KeyExpr   // or keep Key: named arg
```

And reserve these named args:

* `BindingContext:`
* `Key:`
* Anything whose value is `BindingExpr` becomes a binding instead of a direct value.

---

## A complete MVVM sample

```raven
import Microsoft.Maui.Controls.*

class CounterVm : ObservableObject {
    public var Count: int = 0
    public var CountText: string => $"Count: {Count}"

    public val IncCommand = Command(() => {
        Count += 1
        Notify(nameof(Count))
        Notify(nameof(CountText))
    })
}

val vm = CounterVm()

val view =
MauiRuntime.Render(vm) {
    ContentPage(Title: "MVVM", BindingContext: vm) {
        VerticalStackLayout(Spacing: 12, Padding: Thickness(16)) {
            Label(Text: Bind("CountText"))
            Button(Text: "Increment", Command: Bind("IncCommand"))
        }
    }
}
```

---

## Recommendation

Start with **string path bindings** (`Bind("Prop")`) because it’s:

* easy to implement
* matches MAUI perfectly
* zero custom binding engine

Then add **typed Bind** later (member-chain extraction) for Raven ergonomics.