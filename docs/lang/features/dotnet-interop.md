# Use .NET libraries from Raven

Raven programs compile to .NET assemblies and use framework and package APIs
directly. Interoperability is an everyday programming model, not a separate
foreign-function layer.

## Import a namespace

Import members from a namespace with `.*`:

```raven
import System.*
import System.Collections.Generic.*

let names = List<string>()
names.Add("Raven")

Console.WriteLine(names.Count)
```

You can also keep a type qualified when that makes the boundary clearer.

## Use NuGet packages and framework references

A `.rvnproj` is an SDK-style .NET project. Add `PackageReference` and
`FrameworkReference` items in the same way as other .NET projects. Raven code
then imports and calls the exposed namespaces and types.

The [Web API guide](../../workloads/web-api.md) demonstrates ASP.NET Core
routing, dependency injection, JSON, OpenAPI, tasks, and async streams from a
Raven project.

## Keep boundaries recognizable

Framework-facing functions should normally use the types expected by the
framework. Translate into richer Raven domain shapes behind that boundary:

- accept nullable values where a .NET API is nullable
- use `Option` when absence becomes a domain state
- accept or return tasks for asynchronous .NET APIs
- use records and unions for application models where serializers and
  framework integration support them

This keeps interop straightforward without allowing framework storage shapes
to dictate the whole domain model.

Continue with [asynchronous code](async.md) or the
[project-system guide](../../compiler/project-system.md).
