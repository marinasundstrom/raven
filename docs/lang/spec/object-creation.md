# Object creation

Objects are created by **calling the type name** directly, just like any
other method.

```raven
val sb = StringBuilder()
sb.AppendLine("Foo")
```

Generic types work the same way:

```raven
val list = List<int>()
list.Add(2)
```

When a generic type name is called without explicit type arguments, Raven may
infer the type arguments from constructor arguments. Constructor inference uses
the same argument-binding rules as method calls, including delegate target
typing for function expressions.

```raven
open class Endpoint {
    init(handler: Delegate) {}
}

class GET<T> : Endpoint {
    init(pattern: string, handler: T -> string) : base(handler) {}
}

val route = GET("/{id:int}", func (id: int) => id.ToString())
// route : GET<int>
```

If a non-generic type and a generic type share the same name, an applicable
non-generic constructor remains preferred. If the non-generic constructor is not
applicable and exactly one generic type can be constructed from the arguments,
that generic construction is selected. If multiple generic type candidates infer
successfully, the call is ambiguous and must be disambiguated with explicit type
arguments or a different type name.

> A standalone type name is not a constructor call in value position.
> Use `Foo()` instead of `Foo`.

The object-initializer `with` form may be used for parameterless construction:

```raven
val window = Window with {
    Title = "Main"
}
```

This form is equivalent to calling `Window()` and then applying the initializer
entries in order. See [Object initialization and copying](object-initialization-and-copying.md)
for initializer entries, required members, and `with` expressions.
