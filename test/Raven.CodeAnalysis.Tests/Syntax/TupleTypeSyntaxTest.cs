using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Syntax.Tests;

public class TupleTypeSyntaxTest
{
    [Fact]
    public void TupleType_WithNamedElements_Parses()
    {
        var code = "val x: (id: string, age: int) = (\"a\", 1)";
        var tree = SyntaxTree.ParseText(code);
        var root = tree.GetRoot();
        var local = (LocalDeclarationStatementSyntax)((GlobalStatementSyntax)root.Members[0]).Statement!;
        var typeSyntax = local.Declaration.Declarators[0].TypeAnnotation!.Type;
        var tuple = Assert.IsType<TupleTypeSyntax>(typeSyntax);
        Assert.Equal(2, tuple.Elements.Count);
        Assert.Equal("id", ((IdentifierNameSyntax)tuple.Elements[0].NameColon!.Name).Identifier.Text);
        Assert.IsType<PredefinedTypeSyntax>(tuple.Elements[0].Type);
        Assert.IsType<PredefinedTypeSyntax>(tuple.Elements[1].Type);
    }

    [Fact]
    public void TupleType_SingleElementNamed_ReportsDiagnostic()
    {
        var code = "val x: (id: string) = (\"a\")";
        var tree = SyntaxTree.ParseText(code);

        Assert.Contains(tree.GetDiagnostics(), d => d.Descriptor == CompilerDiagnostics.SingleElementTupleTypeNotAllowed);
    }

    [Fact]
    public void TupleType_SingleElementWithTrailingComma_ReportsDiagnostic()
    {
        var code = "val x: (string,) = (\"a\",)";
        var tree = SyntaxTree.ParseText(code);

        Assert.Contains(tree.GetDiagnostics(), d => d.Descriptor == CompilerDiagnostics.SingleElementTupleTypeNotAllowed);
    }

    [Fact]
    public void ParenthesizedSingleType_IsGroupingNotTuple()
    {
        var code = "val x: (string) = \"a\"";
        var tree = SyntaxTree.ParseText(code);
        var root = tree.GetRoot();
        var local = (LocalDeclarationStatementSyntax)((GlobalStatementSyntax)root.Members[0]).Statement!;
        var typeSyntax = local.Declaration.Declarators[0].TypeAnnotation!.Type;

        Assert.IsNotType<TupleTypeSyntax>(typeSyntax);
        Assert.IsType<PredefinedTypeSyntax>(typeSyntax);
        Assert.DoesNotContain(tree.GetDiagnostics(), d => d.Descriptor == CompilerDiagnostics.SingleElementTupleTypeNotAllowed);
    }
}
