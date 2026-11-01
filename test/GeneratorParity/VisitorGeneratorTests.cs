using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

using NodesShared;

using Raven.Generators;

namespace Raven.GeneratorParity.Tests;

[Trait("Target", "DotNet")]
public class VisitorGeneratorTests
{
    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void VisitorsDispatchTransitiveCasesInOrdinalOrder(bool generic)
    {
        List<SyntaxNodeModel> nodes = [
            new() { Name = "Root", IsAbstract = true },
            new() { Name = "Middle", Inherits = "Root", IsAbstract = true },
            new() { Name = "Zed", Inherits = "Middle" },
            new() { Name = "Alpha", Inherits = "Root" },
            new() { Name = "Other", Inherits = "Missing" },
            new() { Name = "Empty", IsAbstract = true }
        ];
        var result = generic ? VisitorGenerator.GenerateGenericVisitorClass(nodes) : VisitorGenerator.GenerateVisitorClass(nodes);
        Assert.Empty(result.GetDiagnostics());
        var methods = result.DescendantNodes().OfType<MethodDeclarationSyntax>().ToList();
        var dispatch = methods.Single(m => m.Identifier.Text == "VisitRoot").ToFullString();
        Assert.True(dispatch.IndexOf("AlphaSyntax", StringComparison.Ordinal) < dispatch.IndexOf("ZedSyntax", StringComparison.Ordinal));
        Assert.Contains("DefaultVisit(node)", dispatch);
        Assert.DoesNotContain("OtherSyntax", dispatch);
        Assert.DoesNotContain(methods, m => m.Identifier.Text == "VisitEmpty");
        Assert.Contains(methods, m => m.Identifier.Text == "VisitOther");
        Assert.Equal(generic ? "TResult" : "void", methods[0].ReturnType.ToString());
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void RewriterPreservesKindAndChoosesSlotVisitors(bool green)
    {
        List<SyntaxNodeModel> nodes = [new() { Name = "Container", HasExplicitKind = true, Slots = [
            new() { Name = "Token", Type = "Token" },
            new() { Name = "Tokens", Type = "TokenList" },
            new() { Name = "Items", Type = "List", ElementType = "Expression" },
            new() { Name = "Arguments", Type = "SeparatedList", ElementType = "Argument" },
            new() { Name = "Child", Type = "Expression", IsNullable = true }
        ] }];
        var ns = green ? "Example.InternalSyntax" : "Example.Syntax";
        var result = VisitorGenerator.GenerateSyntaxRewriterClass(nodes, false, ns);
        Assert.Empty(result.GetDiagnostics());
        var source = result.ToFullString();
        Assert.Contains("namespace " + ns, source);
        Assert.Contains("internal abstract partial class SyntaxRewriter", source);
        Assert.Contains("node?.Update(node.Kind, VisitToken(node.Token)", source);
        Assert.Contains(green ? "VisitList(node.Tokens)" : "(SyntaxTokenList)VisitTokenList(node.Tokens)", source);
        Assert.Contains("VisitList(node.Items)", source);
        Assert.Contains("VisitList(node.Arguments)", source);
        Assert.Contains("(ExpressionSyntax? )VisitExpression(node.Child)", source);
    }

    [Fact]
    public void EmptyInputProducesValidEmptyVisitors()
    {
        foreach (var result in new[] {
            VisitorGenerator.GenerateVisitorClass([]),
            VisitorGenerator.GenerateGenericVisitorClass([]),
            VisitorGenerator.GenerateSyntaxRewriterClass([]) })
        {
            Assert.Empty(result.GetDiagnostics());
            Assert.Empty(result.DescendantNodes().OfType<MethodDeclarationSyntax>());
            Assert.Single(result.DescendantNodes().OfType<ClassDeclarationSyntax>());
        }
    }
}
