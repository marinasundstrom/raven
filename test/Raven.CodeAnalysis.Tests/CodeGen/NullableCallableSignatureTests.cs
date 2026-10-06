using Raven.CodeAnalysis.CodeGen.Portable;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Tests.CodeGen;

public class NullableCallableSignatureTests
{
    [Fact]
    public void NullableUnconstrainedParameter_PreservesGenericStorageIdentity()
    {
        var compilation = Compilation.Create("NullableGeneric", [SyntaxTree.ParseText("""
            class Api {
                static func Echo<T>(value: T?) -> T? => value
            }
            """)], new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.Default);
        var method = compilation.GetTypeByMetadataName("Api")!.GetMembers("Echo").OfType<IMethodSymbol>().Single();
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        Assert.True(method.ReturnType.IsNullable);
        Assert.True(CallableSignature.TryType(method.ReturnType, true, out var storage));
        Assert.Same(method.TypeParameters[0], storage.MethodParameter);
        using var image = new MemoryStream();
        var emitted = compilation.Emit(image);
        Assert.True(emitted.Success, string.Join("; ", emitted.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(new MemoryStream(image.ToArray()), TestMetadataReferences.Default);
        var echo = loaded.Assembly.GetType("Api")!.GetMethod("Echo")!.MakeGenericMethod(typeof(string));
        Assert.Null(echo.Invoke(null, new object?[] { null }));
        Assert.Equal("retained", echo.Invoke(null, new object?[] { "retained" }));
    }
}
