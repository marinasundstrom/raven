using System.Reflection;
using System.Runtime.Loader;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.CodeGen;

public sealed class UnionOutputInitializationTests
{
    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void FailedTryGetValueClearsPreviouslyPopulatedOutput(bool generic)
    {
        var compilation = Compilation.Create("UnionOutputInitialization", [SyntaxTree.ParseText($$"""
            public union Choice{{(generic ? "<T>" : "")}} {
                case Some(value: {{(generic ? "T" : "int")}})
                case None
            }
            """)], TestMetadataReferences.Default, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var image = new MemoryStream();
        var emitted = compilation.Emit(image);
        Assert.True(emitted.Success, string.Join("\n", emitted.Diagnostics));
        var context = new AssemblyLoadContext("union-output-initialization", isCollectible: true);
        try
        {
            image.Position = 0;
            var assembly = context.LoadFromStream(image);
            var carrier = assembly.GetType(generic ? "Choice`1" : "Choice")!;
            if (generic) carrier = carrier.MakeGenericType(typeof(int));
            var method = carrier.GetMethods().Single(m => m.Name == "TryGetValue" &&
                m.GetParameters()[0].ParameterType.GetElementType()!.GetConstructors().Any(c => c.GetParameters().Length == 1));
            var caseType = method.GetParameters()[0].ParameterType.GetElementType()!;
            var value = Activator.CreateInstance(caseType, [99]);
            var output = new[] { value };
            Assert.Equal(false, method.Invoke(Activator.CreateInstance(carrier), output));
            var payload = caseType.GetProperties(BindingFlags.Public | BindingFlags.Instance).Single(p => p.PropertyType == typeof(int));
            Assert.Equal(0, payload.GetValue(output[0]));
        }
        finally { context.Unload(); }
    }
}
