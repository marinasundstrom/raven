using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.Semantics.Diagnostics;

public class ExpressionBodyReturnDiagnosticsTests
{
    [Theory]
    [InlineData(false, false, false)]
    [InlineData(false, false, true)]
    [InlineData(false, true, false)]
    [InlineData(true, false, false)]
    [InlineData(true, false, true)]
    [InlineData(true, true, false)]
    public void IncompatibleReturnDiagnosesBeforeEmission(bool member, bool block, bool queryFirst)
    {
        var body = block ? "{ return value }" : "=> value";
        var declaration = "func Convert(value: ReturnUnrelated) -> IReturnContract " + body;
        if (member) declaration = "class Container { " + declaration + " }";
        var tree = SyntaxTree.ParseText("import Raven.CodeAnalysis.Tests.Semantics.Diagnostics.*\n" + declaration);
        var compilation = Compilation.Create("ReturnDiagnostics", [tree],
            [.. TestMetadataReferences.Default, MetadataReference.CreateFromFile(typeof(IReturnContract).Assembly.Location)],
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        if (queryFirst)
        {
            var expression = tree.GetRoot().DescendantNodes().OfType<ArrowExpressionClauseSyntax>().Single().Expression;
            _ = compilation.GetSemanticModel(tree).GetTypeInfo(expression);
        }
        for (var i = 0; i < 2; i++)
        {
            var errors = compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error).ToArray();
            Assert.Single(errors);
            Assert.Equal(CompilerDiagnostics.CannotConvertFromTypeToType.Id, errors[0].Id);
        }
    }
}

public interface IReturnContract { int Get(); }
public class ReturnUnrelated { }
