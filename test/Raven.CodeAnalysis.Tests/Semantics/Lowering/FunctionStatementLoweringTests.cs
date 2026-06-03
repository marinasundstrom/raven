using System;
using System.Linq;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Semantics.Tests;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Semantics.Lowering.Tests;

public sealed class FunctionStatementLoweringTests : CompilationTestBase
{
    [Fact]
    public void Lowerer_FunctionStatement_RemainsFunctionStatement()
    {
        const string source = """
class C {
    func Test() {
        func nested() {
            ()
        }

        nested()
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var model = compilation.GetSemanticModel(tree);
        var methodSyntax = tree.GetRoot()
            .DescendantNodes()
            .OfType<MethodDeclarationSyntax>()
            .Single(m => m.Identifier.Text == "Test");

        var methodSymbol = Assert.IsAssignableFrom<IMethodSymbol>(model.GetDeclaredSymbol(methodSyntax));
        var boundBody = Assert.IsType<BoundBlockStatement>(model.GetBoundNode(methodSyntax.Body!));

        var originalFunction = boundBody.Statements.OfType<BoundFunctionStatement>().Single();

        var loweredBody = Lowerer.LowerBlock(methodSymbol, boundBody);
        var loweredFunction = loweredBody.Statements.OfType<BoundFunctionStatement>().Single();

        Assert.Same(originalFunction.Method, loweredFunction.Method);
    }
}
