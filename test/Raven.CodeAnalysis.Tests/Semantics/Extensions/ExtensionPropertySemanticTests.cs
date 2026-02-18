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

        var bound = model.GetBoundNode(memberAccess);
        var extensionMemberName = bound switch
        {
            BoundMemberAccessExpression access => access.Member.Name,
            BoundInvocationExpression invocation => invocation.Method.Name,
            _ => throw new InvalidOperationException($"Unexpected bound node type: {bound?.GetType().Name ?? "<null>"}")
        };
        Assert.Contains("IsZero", extensionMemberName, StringComparison.Ordinal);

        var symbolInfo = model.GetSymbolInfo(memberAccess);
        var selectedSymbol = Assert.IsAssignableFrom<ISymbol>(symbolInfo.Symbol);
        Assert.Contains("IsZero", selectedSymbol.Name, StringComparison.Ordinal);
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
