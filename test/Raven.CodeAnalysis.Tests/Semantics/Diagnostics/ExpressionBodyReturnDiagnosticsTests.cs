using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.Semantics.Diagnostics;

public class ExpressionBodyReturnDiagnosticsTests
{
    [Theory]
    [Trait("Target", "Shared")]
    [InlineData("static val Value: int => cache ??= 42", false)]
    [InlineData("static val Value: int => cache ??= 42", true)]
    [InlineData("static func Value() -> int => cache ??= 42", false)]
    [InlineData("static func Value() -> int => cache ??= 42", true)]
    [InlineData("static func Value() -> int { return cache ??= 42 }", false)]
    public void CoalescingAssignmentUnitCannotBeReturnedAsAValue(string declaration, bool queryFirst)
    {
        var tree = SyntaxTree.ParseText("class Container { private static field cache: int? = null; " + declaration + " }");
        var compilation = Compilation.Create("AssignmentReturn", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        if (queryFirst)
        {
            var expression = tree.GetRoot().DescendantNodes().OfType<ArrowExpressionClauseSyntax>().Single().Expression;
            _ = compilation.GetSemanticModel(tree).GetTypeInfo(expression);
        }
        var errors = compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error).ToArray();
        Assert.Single(errors);
        Assert.Equal(CompilerDiagnostics.CannotConvertFromTypeToType.Id, errors[0].Id);
    }

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
