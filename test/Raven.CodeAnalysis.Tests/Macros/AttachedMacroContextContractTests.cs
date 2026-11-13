using System;
using System.Linq;
using System.Threading;

using Raven.CodeAnalysis.Macros;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.Macros;

[Trait("Target", "Shared")]
public sealed class AttachedMacroContextContractTests
{
    [Theory]
    [InlineData("[Example] class Model {}", 0)]
    [InlineData("[Example(42, name: \"value\")] class Model {}", 2)]
    public void ContextRetainsAuthoredNodesAndArgumentOrder(string source, int count)
    {
        var tree = SyntaxTree.ParseText(source);
        var attribute = Assert.Single(tree.GetRoot().DescendantNodes().OfType<AttributeSyntax>());
        var target = Assert.Single(tree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>());
        var current = SyntaxFactory.ParseCompilationUnit("class Replacement {}").Members[0];
        using var cancellation = new CancellationTokenSource();
        var context = new AttachedMacroContext(new NoSemanticQueries(), attribute, target, current, cancellation.Token);
        Assert.Same(attribute, context.Syntax);
        Assert.Same(attribute.ArgumentList, context.ArgumentList);
        Assert.Same(target, context.TargetDeclaration);
        Assert.Same(current, context.CurrentDeclaration);
        Assert.Equal(cancellation.Token, context.CancellationToken);
        Assert.Equal(count, context.Arguments.Length);
        for (var index = 0; index < count; index++)
        {
            var authored = attribute.ArgumentList!.Arguments[index];
            Assert.Equal(authored.Span, context.Arguments[index].Syntax.Span);
            Assert.Equal(authored.ToFullString(), context.Arguments[index].Syntax.ToFullString());
            Assert.Same(tree, context.Arguments[index].Syntax.SyntaxTree);
        }
        if (count > 0)
        {
            Assert.False(context.Arguments[0].IsNamed);
            Assert.Equal("name", context.Arguments[1].Name);
        }
    }

    [Fact]
    public void DiagnosticsUseNameExplicitSyntaxOrArgumentLocations()
    {
        var tree = SyntaxTree.ParseText("[Example(42)] class Model {}");
        var attribute = Assert.Single(tree.GetRoot().DescendantNodes().OfType<AttributeSyntax>());
        var target = Assert.Single(tree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>());
        var context = new AttachedMacroContext(new NoSemanticQueries(), attribute, target, target);
        var diagnostic = context.CreateDiagnostic("message", code: "CUSTOM");
        Assert.Equal(attribute.Name.Span, diagnostic.Location!.SourceSpan);
        Assert.Equal(DiagnosticSeverity.Error, diagnostic.Severity);
        Assert.Equal("CUSTOM", diagnostic.Code);
        Assert.Equal(target.Span, context.CreateDiagnostic("message", syntax: target).Location!.SourceSpan);
        var argument = Assert.Single(context.Arguments);
        var argumentDiagnostic = context.CreateArgumentDiagnostic(argument, "argument", DiagnosticSeverity.Warning);
        Assert.Equal(argument.Syntax.Span, argumentDiagnostic.Location!.SourceSpan);
        Assert.Equal(DiagnosticSeverity.Warning, argumentDiagnostic.Severity);
        Assert.Throws<ArgumentNullException>(() => context.CreateArgumentDiagnostic(null!, "message"));
    }

    [Fact]
    public void InternalCoreValidatesRequiredInputs()
    {
        var tree = SyntaxTree.ParseText("[Example] class Model {}");
        var attribute = Assert.Single(tree.GetRoot().DescendantNodes().OfType<AttributeSyntax>());
        var target = tree.GetRoot().Members[0];
        var services = new NoSemanticQueries();
        Assert.Equal("syntax", Assert.Throws<ArgumentNullException>(() => new AttachedMacroContext(services, null!, target, target)).ParamName);
        Assert.Equal("services", Assert.Throws<ArgumentNullException>(() => new AttachedMacroContext(null!, attribute, target, target)).ParamName);
        Assert.Equal("targetDeclaration", Assert.Throws<ArgumentNullException>(() => new AttachedMacroContext(services, attribute, null!, target)).ParamName);
        Assert.Equal("currentDeclaration", Assert.Throws<ArgumentNullException>(() => new AttachedMacroContext(services, attribute, target, null!)).ParamName);
    }

    private sealed class NoSemanticQueries : MacroArgumentServices
    {
        public TypeSymbol GetSpecialType(SpecialType specialType) => throw new InvalidOperationException("Unexpected semantic query");
        public TypeSymbol? GetArgumentType(Raven.CodeAnalysis.Syntax.ExpressionSyntax expression) => throw new InvalidOperationException("Unexpected semantic query");
        public TypeSymbol? ResolveConstraint(Type runtimeType) => throw new InvalidOperationException("Unexpected semantic query");
        public Conversion ClassifyConversion(TypeSymbol source, TypeSymbol destination) => throw new InvalidOperationException("Unexpected semantic query");
    }
}
