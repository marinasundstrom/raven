using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public class NamespaceMemberColdQueryTests
{
    [Theory]
    [InlineData(0, false)]
    [InlineData(0, true)]
    [InlineData(1, false)]
    [InlineData(1, true)]
    [InlineData(2, false)]
    [InlineData(2, true)]
    public void ImportedUnionParameterIsIndependentOfQueryAndFileOrder(int queryOrder, bool reverseFiles)
    {
        var declaration = SyntaxTree.ParseText("namespace Example\npublic union ReaderError { case TimedOut }");
        var consumer = SyntaxTree.ParseText("import Example.*\nfunc Describe(error: ReaderError) -> string => \"error\"");
        var compilation = Compilation.Create("ColdImportedUnion", reverseFiles ? [declaration, consumer] : [consumer, declaration],
            TestMetadataReferences.Default, CompilationOptions.DotNet.WithOutputKind(OutputKind.DynamicallyLinkedLibrary));
        var model = compilation.GetSemanticModel(consumer);
        var function = consumer.GetRoot().DescendantNodes().OfType<FunctionStatementSyntax>().Single();
        var name = function.DescendantNodes().OfType<IdentifierNameSyntax>().Single(node => node.Identifier.ValueText == "ReaderError");
        if (queryOrder == 0)
            Assert.True(Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetSymbolInfo(name).Symbol).IsUnion);
        else if (queryOrder == 2)
            Assert.DoesNotContain(compilation.GetDiagnostics(), diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);

        var method = Assert.IsAssignableFrom<IMethodSymbol>(model.GetDeclaredSymbol(function));
        var parameterType = Assert.Single(method.Parameters).Type;
        Assert.True(parameterType.IsUnion);
        Assert.Equal("ReaderError", parameterType.Name);
        Assert.Same(parameterType, model.GetSymbolInfo(name).Symbol);
        Assert.Same(parameterType, model.GetTypeInfo(name).Type);
        Assert.DoesNotContain(compilation.GetDiagnostics(), diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
    }
}
