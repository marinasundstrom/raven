using System.Reflection;
using System.Runtime.Loader;

using Mono.Cecil;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class MixedGenericMetadataTests
{
    [Theory]
    [InlineData("class", false)]
    [InlineData("struct", false)]
    [InlineData("class", true)]
    [InlineData("struct", true)]
    public void MetadataGenericWithSourceArgumentPreservesIdentityAndRuns(string category, bool retarget)
    {
        var directory = Path.Combine(Path.GetTempPath(), "raven-mixed-generics", Guid.NewGuid().ToString("N"));
        Directory.CreateDirectory(directory);
        var path = Path.Combine(directory, "TargetContainer.dll");
        var paths = TargetFrameworkResolver.GetReferenceAssemblies(TargetFrameworkResolver.ResolveVersion("net11.0"));
        var declarations = Microsoft.CodeAnalysis.CSharp.CSharpCompilation.Create("TargetContainer",
            [Microsoft.CodeAnalysis.CSharp.CSharpSyntaxTree.ParseText("namespace Contracts { public class Box<T> { public T Value; public Box(T value) { Value = value; } public T Get() => Value; } }")],
            paths.Select(p => Microsoft.CodeAnalysis.MetadataReference.CreateFromFile(p)),
            new Microsoft.CodeAnalysis.CSharp.CSharpCompilationOptions(Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary));
        try
        {
            using (var stream = File.Create(path))
            {
                var emitted = declarations.Emit(stream, options: new Microsoft.CodeAnalysis.Emit.EmitOptions(metadataOnly: true));
                Assert.True(emitted.Success, string.Join("\n", emitted.Diagnostics));
            }
            var compilation = Compilation.Create("ApplicationConsumer", [SyntaxTree.ParseText($$"""
                import Contracts.*
                {{category}} Payload { var Number: int }
                func Create(value: Payload) -> Payload {
                    let box = Box<Payload>(value)
                    box.Value = value
                    return box.Get()
                }
                func WrapArray(value: Payload[]) -> Payload[] {
                    let box = Box<Payload[]>(value)
                    box.Value = value
                    return box.Get()
                }
                func WrapNested(value: Box<Payload>) -> Box<Payload> {
                    let box = Box<Box<Payload>>(value)
                    box.Value = value
                    return box.Get()
                }
                func Run() -> int {
                    let value = Payload()
                    value.Number = 42
                    return Create(value).Number
                }
                """)], paths.Append(path).Select(MetadataReference.CreateFromFile).ToArray(),
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
            Assert.Empty(compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error));
            using var output = new MemoryStream();
            var options = retarget
                ? new EmitOptions(AssemblyName.GetAssemblyName(paths.Single(p => Path.GetFileName(p) == "System.Runtime.dll")))
                : new EmitOptions();
            var result = compilation.Emit(output, null, options);
            Assert.True(result.Success, string.Join("\n", result.Diagnostics));
            output.Position = 0;
            using (var assembly = AssemblyDefinition.ReadAssembly(output))
            {
                Assert.DoesNotContain(assembly.MainModule.AssemblyReferences, a => a.Name == "ApplicationConsumer");
                var members = assembly.MainModule.GetMemberReferences()
                    .Where(m => m.DeclaringType is GenericInstanceType).ToArray();
                Assert.NotEmpty(members);
                foreach (var member in members)
                {
                    var owner = Assert.IsType<GenericInstanceType>(member.DeclaringType);
                    Assert.Equal("Contracts.Box`1", owner.ElementType.FullName);
                    Assert.Equal("TargetContainer", owner.Scope.Name);
                    var payload = owner.GenericArguments[0];
                    if (payload is GenericInstanceType nested)
                    {
                        Assert.Equal("Contracts.Box`1", nested.ElementType.FullName);
                        Assert.Equal("TargetContainer", nested.Scope.Name);
                        payload = nested.GenericArguments[0];
                    }
                    if (payload is ArrayType array)
                        payload = array.ElementType;
                    Assert.Equal("Payload", payload.FullName);
                    Assert.Same(assembly.MainModule, payload.Scope);
                    Assert.Equal(category == "struct", payload.IsValueType);
                }
            }

            using var implementation = new MemoryStream();
            var implementationResult = declarations.Emit(implementation);
            Assert.True(implementationResult.Success, string.Join("\n", implementationResult.Diagnostics));
            implementation.Position = 0;
            output.Position = 0;
            var context = new AssemblyLoadContext("MixedGenerics", isCollectible: true);
            try
            {
                var library = context.LoadFromStream(implementation);
                var loaded = context.LoadFromStream(output);
                var functions = loaded.GetTypes().SelectMany(t => t.GetMethods(BindingFlags.Static | BindingFlags.Public | BindingFlags.NonPublic)).ToArray();
                Assert.Equal(42, functions.Single(m => m.Name == "Run").Invoke(null, null));
                var payload = loaded.GetType("Payload", throwOnError: true)!;
                var array = Array.CreateInstance(payload, 1);
                Assert.Same(array, functions.Single(m => m.Name == "WrapArray").Invoke(null, [array]));
                var box = library.GetType("Contracts.Box`1", throwOnError: true)!.MakeGenericType(payload);
                var nested = Activator.CreateInstance(box, [Activator.CreateInstance(payload)]);
                Assert.Same(nested, functions.Single(m => m.Name == "WrapNested").Invoke(null, [nested]));
            }
            finally { context.Unload(); }
        }
        finally { Directory.Delete(directory, recursive: true); }
    }
}
