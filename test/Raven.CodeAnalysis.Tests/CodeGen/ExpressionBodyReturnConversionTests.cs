using System.Runtime.Loader;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.CodeGen;

public class ExpressionBodyReturnConversionTests
{
    [Theory]
    [InlineData(true)]
    [InlineData(false)]
    public void GenericExpressionBodyPreservesBoxingAndReferenceIdentity(bool isStatic)
    {
        var modifier = isStatic ? "static " : "";
        var app = Compilation.Create("ExpressionBodyBoxing", [SyntaxTree.ParseText($$"""
            public {{modifier}}class Consumer {
                public {{modifier}}func Box<T>(value: T) -> object => value
            }
            """)], TestMetadataReferences.Default, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(app.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        using var image = new MemoryStream();
        var emitted = app.Emit(image);
        Assert.True(emitted.Success, string.Join("\n", emitted.Diagnostics));
        var context = new AssemblyLoadContext("expression-body-boxing", isCollectible: true);
        try
        {
            image.Position = 0;
            var type = context.LoadFromStream(image).GetType("Consumer")!;
            var box = type.GetMethod("Box")!;
            var receiver = isStatic ? null : Activator.CreateInstance(type);
            Assert.Equal(42, box.MakeGenericMethod(typeof(int)).Invoke(receiver, [42]));
            var value = new object();
            Assert.Same(value, box.MakeGenericMethod(typeof(object)).Invoke(receiver, [value]));
            Assert.Null(box.MakeGenericMethod(typeof(object)).Invoke(receiver, [null]));
        }
        finally { context.Unload(); }
    }
}
