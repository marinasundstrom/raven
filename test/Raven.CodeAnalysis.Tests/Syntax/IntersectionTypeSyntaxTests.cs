using System.Linq;

using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Syntax.Tests;

public class IntersectionTypeSyntaxTests
{
    [Fact]
    public void Intersection_BindsMoreTightlyThanUnion()
    {
        var type = ParseParameterType("A | B & C | D");
        var union = Assert.IsType<UnionTypeSyntax>(type);
        Assert.Equal(3, union.Types.Count);
        var intersection = Assert.IsType<IntersectionTypeSyntax>(union.Types[1]);
        Assert.Equal(new[] { "B", "C" }, intersection.Types.Select(t => t.ToString()));
        Assert.Equal(SyntaxKind.AmpersandToken, intersection.Types.GetSeparator(0).Kind);
    }

    [Theory]
    [InlineData("(A | B) & C")]
    [InlineData("(A & B) & C")]
    public void Parentheses_PreserveGrouping(string text)
    {
        var intersection = Assert.IsType<IntersectionTypeSyntax>(ParseParameterType(text));
        Assert.IsType<ParenthesizedTypeSyntax>(intersection.Types[0]);
    }

    [Fact]
    public void Intersection_PreservesSuffixesAndGenericArguments()
    {
        var intersection = Assert.IsType<IntersectionTypeSyntax>(ParseParameterType("A? & B<C>[]"));
        Assert.IsType<NullableTypeSyntax>(intersection.Types[0]);
        Assert.IsType<ArrayTypeSyntax>(intersection.Types[1]);

        var generic = Assert.IsType<GenericNameSyntax>(ParseParameterType("Box<A & B>"));
        Assert.Single(generic.DescendantNodes().OfType<IntersectionTypeSyntax>());
    }

    [Fact]
    public void PrefixByRef_RetainsExistingOperandBinding()
    {
        var byRef = Assert.IsType<ByRefTypeSyntax>(ParseParameterType("&A & B"));
        Assert.IsType<IntersectionTypeSyntax>(byRef.ElementType);
    }

    [Fact]
    public void FunctionReturnType_CanContainIntersection()
    {
        var function = Assert.IsType<FunctionTypeSyntax>(ParseParameterType("() -> A & B"));
        Assert.IsType<IntersectionTypeSyntax>(function.ReturnType);
    }

    [Theory]
    [InlineData("class Box<T: A & B> {}")]
    [InlineData("class Box<T> where T: A & B {}")]
    public void Constraints_PreserveIntersectionSyntax(string code)
    {
        var tree = SyntaxTree.ParseText(code);
        Assert.Empty(tree.GetDiagnostics());
        var constraint = Assert.Single(tree.GetRoot().DescendantNodes().OfType<TypeConstraintSyntax>());
        Assert.IsType<IntersectionTypeSyntax>(constraint.Type);
        Assert.Equal(code, tree.GetRoot().ToFullString());
    }

    [Fact]
    public void MissingRightOperand_ReportsDiagnosticAndPreservesFollowingDeclaration()
    {
        var tree = SyntaxTree.ParseText("class Box<T: A &> {}\nclass Next {}");
        Assert.NotEmpty(tree.GetDiagnostics());
        Assert.Equal(2, tree.GetRoot().Members.Count);
        Assert.Single(tree.GetRoot().DescendantNodes().OfType<IntersectionTypeSyntax>());
    }

    private static TypeSyntax ParseParameterType(string text)
    {
        var code = $"func accept(value: {text}) {{ }}";
        var tree = SyntaxTree.ParseText(code);
        Assert.Empty(tree.GetDiagnostics());
        Assert.Equal(code, tree.GetRoot().ToFullString());
        var function = tree.GetRoot().DescendantNodes().OfType<FunctionStatementSyntax>().Single();
        return function.ParameterList.Parameters[0].TypeAnnotation!.Type;
    }
}
