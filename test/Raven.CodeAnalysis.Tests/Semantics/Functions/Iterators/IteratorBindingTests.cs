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
    public void Yield_InGenericEnumerableMethod_BindsIteratorElementType()
    {
        const string source = """
import System.Collections.Generic.*

func Numbers() -> IEnumerable<int> {
    yield 1
    yield 2
}
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var root = tree.GetRoot();
        var yieldStatements = root.DescendantNodes().OfType<YieldStatementSyntax>().ToArray();
        var methodDeclaration = root.DescendantNodes().OfType<FunctionStatementSyntax>().Single();

        var firstYield = Assert.IsType<BoundYieldStatement>(model.GetBoundNode(yieldStatements[0]));
        var secondYield = Assert.IsType<BoundYieldStatement>(model.GetBoundNode(yieldStatements[1]));
        var method = Assert.IsAssignableFrom<IMethodSymbol>(model.GetDeclaredSymbol(methodDeclaration));

        Assert.Empty(compilation.GetDiagnostics().Where(static diagnostic => diagnostic.Severity == DiagnosticSeverity.Error));
        Assert.Equal(IteratorMethodKind.Enumerable, firstYield.IteratorKind);
        Assert.Equal(SpecialType.System_Int32, firstYield.ElementType.SpecialType);
        Assert.Equal(SpecialType.System_Int32, firstYield.Expression.Type?.SpecialType);
        Assert.Equal(IteratorMethodKind.Enumerable, secondYield.IteratorKind);
        Assert.True(method.IsIterator);
        Assert.Equal(IteratorMethodKind.Enumerable, method.IteratorKind);
        Assert.Equal(SpecialType.System_Int32, method.IteratorElementType?.SpecialType);
    }
}
