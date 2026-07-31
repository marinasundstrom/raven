using System.Collections.Concurrent;
using System.Linq;
using System.Threading.Tasks;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;
using Raven.CodeAnalysis.Tests;

namespace Raven.CodeAnalysis.Semantics.Binding;

public sealed class TargetTypeScopeTests
{
    [Fact]
    public void TargetTypeScope_Dispose_IsIdempotentWhenStackAlreadyUnwound()
    {
        var syntaxTree = SyntaxTree.ParseText(
            """
func Main() {
    let x = 1
}
""");

        var compilation = Compilation.Create(
            "target-type-scope-tests",
            [syntaxTree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        var model = compilation.GetSemanticModel(syntaxTree);
        var function = syntaxTree.GetRoot().DescendantNodes().OfType<FunctionStatementSyntax>().Single();
        var binder = Assert.IsAssignableFrom<BlockBinder>(model.GetBinder(function.Body));
        var int32 = compilation.GetSpecialType(SpecialType.System_Int32);

        var scope = binder.PushTargetType(int32);
        binder.PushTargetType(int32).Dispose();

        var ex = Record.Exception(() => scope.Dispose());
        Assert.Null(ex);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void ParenthesizedExpressionCache_DistinguishesTargetTypes(bool bindStringFirst)
    {
        var syntaxTree = SyntaxTree.ParseText(
            """
func Main() {
    let value = (default)
}
""");

        var compilation = Compilation.Create(
            "target-type-cache-tests",
            [syntaxTree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        var model = compilation.GetSemanticModel(syntaxTree);
        var parenthesized = syntaxTree.GetRoot()
            .DescendantNodes()
            .OfType<ParenthesizedExpressionSyntax>()
            .Single();
        var binder = Assert.IsAssignableFrom<BlockBinder>(model.GetBinder(parenthesized));
        var int32 = compilation.GetSpecialType(SpecialType.System_Int32);
        var @string = compilation.GetSpecialType(SpecialType.System_String);

        var firstTarget = bindStringFirst ? @string : int32;
        var secondTarget = bindStringFirst ? int32 : @string;

        BoundExpression firstExpression;
        using (binder.PushTargetType(firstTarget))
            firstExpression = binder.BindExpression(parenthesized);

        BoundExpression secondExpression;
        using (binder.PushTargetType(secondTarget))
            secondExpression = binder.BindExpression(parenthesized);

        AssertTargetType(firstExpression, bindStringFirst);
        AssertTargetType(secondExpression, !bindStringFirst);
        Assert.NotSame(firstExpression, secondExpression);

        void AssertTargetType(BoundExpression expression, bool expectString)
        {
            if (expectString)
            {
                var nullableString = Assert.IsType<NullableTypeSymbol>(expression.Type);
                Assert.Equal(SpecialType.System_String, nullableString.UnderlyingType.SpecialType);
            }
            else
            {
                Assert.True(SymbolEqualityComparer.Default.Equals(int32, expression.Type));
            }
        }
    }

    [Fact]
    public void EnsureDeclarations_IsStableUnderConcurrentCalls()
    {
        var syntaxTree = SyntaxTree.ParseText(
            """
class User(
    var Id: int,
    var Name: string
)
""");

        var compilation = Compilation.Create(
            "target-type-scope-tests",
            [syntaxTree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        var model = compilation.GetSemanticModel(syntaxTree);

        Parallel.For(0, 8, _ => model.EnsureDeclarations());

        var diagnostics = model.GetDiagnostics();
        Assert.DoesNotContain(diagnostics, diagnostic => diagnostic.Id is "RAV0600" or "RAV0111");
    }

    [Fact]
    public void SemanticQueries_AreStableUnderConcurrentBinderAccess()
    {
        var syntaxTree = SyntaxTree.ParseText(
            """
func Apply(value: int, mapper: func(int) -> int) -> int {
    mapper(value)
}

func Main() {
    let result = Apply(1, func item => item + 1)
}
""");

        var compilation = Compilation.Create(
            "target-type-scope-tests",
            [syntaxTree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        var model = compilation.GetSemanticModel(syntaxTree);
        var root = syntaxTree.GetRoot();
        var invocation = root.DescendantNodes().OfType<InvocationExpressionSyntax>().Last();
        var lambdaParameter = root.DescendantNodes().OfType<ParameterSyntax>().Last();
        var lambdaIdentifier = root.DescendantNodes().OfType<IdentifierNameSyntax>().Last(node => node.Identifier.ValueText == "item");
        var failures = new ConcurrentQueue<Exception>();

        Parallel.For(0, 16, _ =>
        {
            try
            {
                model.GetDiagnostics();
                model.GetOperation(invocation);
                model.GetSymbolInfo(lambdaIdentifier);
                model.GetTypeInfo(lambdaIdentifier);
                model.GetFunctionExpressionParameterSymbol(lambdaParameter);
            }
            catch (Exception ex)
            {
                failures.Enqueue(ex);
            }
        });

        failures.ShouldBeEmpty();
    }
}
