using System.Collections.Concurrent;
using System.Linq;
using System.Threading.Tasks;

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
    val x = 1
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
    val result = Apply(1, func item => item + 1)
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
