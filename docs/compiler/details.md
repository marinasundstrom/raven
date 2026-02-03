# Details

## Reference assemblies

.NET have implementation assemblies and reference assemblies. Reference assemblies just describe the API surface and doesn't have any implementation. In that regard they are like header files. The implementation are in implementation assemblies that are unique to every runtime version.

### Compiling against reference assemblies

In order to to target different runtimes, you compile against the reference assemblies. Then at execution, the runtime will pick the implementation that is suitable.

### Loading metadata assemblies

The reference assemblies are pure metadata assemblies, so no IL, and therefore must be loaded in a special way. Using the `MetadataLoadContext` class you can load that metadata to query it for references.

## API

The compiler API doesn't keep track of what framework to target.

What version you are targeting depend on the core libraries that you reference.

You can add the `TargetFrameworkAttribute` to an assembly to indicate what version.

## Reflection FAQ

### Why does casting a `MemberInfo` to `MethodInfo` fail?

`Type.GetMembers()` (and similar APIs) return heterogeneous collections of `MemberInfo`
instances: constructors, events, fields, properties, and methods. Only the entries
that actually represent methods derive from `System.Reflection.MethodInfo`. Attempting
to cast every `MemberInfo` to `MethodInfo` therefore throws an `InvalidCastException`
for the non-method members. Filter the list first, for example:

```raven
import System.Reflection.*

val type = typeof(System.String)
val members = type.GetMembers()

for member in members {
    if member is MethodInfo method {
        // Pass the instance the method should execute on (or null for static methods)
        method.Invoke(null, System.Array.Empty<object>())
    }
}
```

Here the pattern guard ensures that only `MethodInfo` instances flow into `Invoke` so the
cast never fails. Notice that the first argument to `Invoke` must be the object instance on
which to invoke the method (or `null` for static methods), not the `MemberInfo` itself.