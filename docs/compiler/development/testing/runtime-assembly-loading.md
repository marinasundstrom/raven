# Runtime assembly loading in tests

Some code generation tests need to execute emitted IL. Use the `TestAssemblyLoader` helper to load compiled assemblies with dependency resolution:

```csharp
var compilation = CreateCompilation(source, references);
using var peStream = new MemoryStream();
compilation.Emit(peStream);

using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
var type = loaded.Assembly.GetType("Program");
```

`TestAssemblyLoader` creates a collectible `AssemblyLoadContext` and resolves dependencies from the provided metadata references. This allows tests to run compiled code without polluting the default context.

When compiling, reference assemblies should be resolved using the fixed target framework defined by `TestTargetFramework.Default` to avoid mismatches with locally installed SDKs.
