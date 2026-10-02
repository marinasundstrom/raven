using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.Semantics.Declarations;

public class QualifiedNamespaceGenericTests
{
    [Fact]
    public void QualifiedExplicitArgumentDoesNotGetReinferred()
    {
        var compilation = Compilation.Create("InvalidQualifiedGeneric",
            [SyntaxTree.ParseText("namespace Names\npublic func Echo<T>(value: T) -> T => value"),
             SyntaxTree.ParseText("func Read() -> string => Names.Echo<string>(42)")], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.Contains(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
    }

    [Theory]
    [InlineData("Names.Echo(42)")]
    [InlineData("Names.Echo<int>(42)")]
    [InlineData("Echo(42)")]
    public void NamespaceGenericFunctionBinds(string call)
    {
        var declaration = SyntaxTree.ParseText("namespace Names\npublic func Echo<T>(value: T) -> T => value");
        var consumer = SyntaxTree.ParseText("import Names.*\nfunc Read() -> int => " + call);
        var compilation = Compilation.Create("QualifiedGeneric", [declaration, consumer], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var invocation = consumer.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single();
        var method = Assert.IsAssignableFrom<IMethodSymbol>(compilation.GetSemanticModel(consumer).GetSymbolInfo(invocation).Symbol);
        Assert.Equal(SpecialType.System_Int32, method.ReturnType.SpecialType);
    }
}
