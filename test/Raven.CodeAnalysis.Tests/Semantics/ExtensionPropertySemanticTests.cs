using System;
using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Tests;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class ExtensionPropertySemanticTests : CompilationTestBase
{
    [Fact]
    public void MemberAccess_OnValueTypeReceiver_BindsToExtensionProperty()
    {
        const string source = """
extension IntExtensions for int {
    IsZero: bool {
        get { return self == 0; }
    }
}

val value = 0
val isZero = value.IsZero
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var memberAccess = GetMemberAccess(tree, "IsZero");

        var boundAccess = Assert.IsType<BoundMemberAccessExpression>(model.GetBoundNode(memberAccess));
        var propertySymbol = Assert.IsAssignableFrom<IPropertySymbol>(boundAccess.Member);
        Assert.True(propertySymbol.IsExtensionProperty());
        Assert.Equal("IsZero", propertySymbol.Name);

        var symbolInfo = model.GetSymbolInfo(memberAccess);
        var selectedProperty = Assert.IsAssignableFrom<IPropertySymbol>(symbolInfo.Symbol);
        Assert.True(selectedProperty.IsExtensionProperty());
        Assert.True(SymbolEqualityComparer.Default.Equals(propertySymbol, selectedProperty));
    }

    private static MemberAccessExpressionSyntax GetMemberAccess(SyntaxTree tree, string memberName)
    {
        return tree
            .GetRoot()
            .DescendantNodes()
            .OfType<MemberAccessExpressionSyntax>()
            .Single(node => node.Name.Identifier.Text == memberName);
    }
}
