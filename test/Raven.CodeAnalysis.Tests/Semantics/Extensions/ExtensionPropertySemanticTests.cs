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
    public void ExtensionProperty_AccessingReceiverMember_DoesNotInspectIncompleteSynthesizedProperty()
    {
        const string source = """
import System.Collections.Generic.*

let items = List<int>()
items.CountPlusOne = 5
let value = items.CountPlusOne

extension ListExtensions for List<int> {
    var CountPlusOne: int {
        get => self.Count + 1
        set => self.Add(value)
    }
}
""";

        var (compilation, _) = CreateCompilation(source);

        var exception = Record.Exception(() => compilation.GetDiagnostics());

        Assert.Null(exception);
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void MemberAccess_OnValueTypeReceiver_BindsToExtensionProperty()
    {
        const string source = """
extension IntExtensions for int {
    val IsZero: bool {
        get { return self == 0; }
    }
}

let value = 0
let isZero = value.IsZero
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
