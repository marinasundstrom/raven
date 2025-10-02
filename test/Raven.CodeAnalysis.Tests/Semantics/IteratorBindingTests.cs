using System;
using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class IteratorBindingTests : CompilationTestBase
{
    [Fact]
    public void IteratorMethodReturningIEnumerableT_BindsYieldStatementsAndMarksMethod()
    {
        const string source = """
import System.Collections.Generic.*

class C {
    Iterator() -> IEnumerable<int> {
        yield return 1
        yield break
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var root = tree.GetRoot();

        var methodSyntax = root
            .DescendantNodes()
            .OfType<MethodDeclarationSyntax>()
            .Single();

        var yieldReturnSyntax = root
            .DescendantNodes()
            .OfType<YieldReturnStatementSyntax>()
            .Single();

        var boundYieldReturn = Assert.IsType<BoundYieldReturnStatement>(model.GetBoundNode(yieldReturnSyntax));
        var iteratorKind = boundYieldReturn.IteratorKind;
        Assert.Equal(IteratorMethodKind.None, iteratorKind);
        Assert.Equal(SpecialType.None, boundYieldReturn.Expression.Type.SpecialType);

        var methodSymbol = (IMethodSymbol)model.GetDeclaredSymbol(methodSyntax)!;
        Assert.Equal("IEnumerable<T>", methodSymbol.ReturnType.ToDisplayString());
        var sourceMethod = Assert.IsType<SourceMethodSymbol>(methodSymbol);
        Assert.False(sourceMethod.IsIterator);
        Assert.Equal(IteratorMethodKind.None, sourceMethod.IteratorKind);

        Assert.Null(sourceMethod.IteratorElementType);

        var yieldBreakSyntax = root
            .DescendantNodes()
            .OfType<YieldBreakStatementSyntax>()
            .Single();

        var boundYieldBreak = Assert.IsType<BoundYieldBreakStatement>(model.GetBoundNode(yieldBreakSyntax));
        Assert.Equal(IteratorMethodKind.None, boundYieldBreak.IteratorKind);
    }

    [Fact]
    public void IteratorMethodReturningIEnumeratorT_BindsIteratorMetadata()
    {
        const string source = """
import System.Collections.Generic.*

class C {
    Enumerator() -> IEnumerator<int> {
        yield return 1
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var root = tree.GetRoot();

        var methodSyntax = root
            .DescendantNodes()
            .OfType<MethodDeclarationSyntax>()
            .Single();

        var yieldReturnSyntax = root
            .DescendantNodes()
            .OfType<YieldReturnStatementSyntax>()
            .Single();

        var boundYieldReturn = Assert.IsType<BoundYieldReturnStatement>(model.GetBoundNode(yieldReturnSyntax));
        var iteratorKind = boundYieldReturn.IteratorKind;
        Assert.Equal(IteratorMethodKind.None, iteratorKind);

        var methodSymbol = (IMethodSymbol)model.GetDeclaredSymbol(methodSyntax)!;
        Assert.Equal("IEnumerator<T>", methodSymbol.ReturnType.ToDisplayString());
        var sourceMethod = Assert.IsType<SourceMethodSymbol>(methodSymbol);
        Assert.False(sourceMethod.IsIterator);
        Assert.Equal(IteratorMethodKind.None, sourceMethod.IteratorKind);

        Assert.Null(sourceMethod.IteratorElementType);
    }
}
