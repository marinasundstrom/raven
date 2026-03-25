using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class DelegateDeclarationSemanticTests : CompilationTestBase
{
    [Fact]
    public void DelegateDeclaration_BindsInvokeSignatureInNestedType()
    {
        const string source = """
class Container
{
    public delegate Formatter<T>(ref value: T, out result: string) -> bool
}
""";

        var (compilation, tree) = CreateCompilation(source);
        var diagnostics = compilation.GetDiagnostics();
        Assert.Empty(diagnostics.Where(d => d.Severity == DiagnosticSeverity.Error));

        var model = compilation.GetSemanticModel(tree);
        var delegateDeclaration = tree.GetRoot().DescendantNodes().OfType<DelegateDeclarationSyntax>().Single();
        var delegateSymbol = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetDeclaredSymbol(delegateDeclaration));

        Assert.Equal(TypeKind.Delegate, delegateSymbol.TypeKind);
        Assert.Equal("Formatter", delegateSymbol.Name);
        Assert.Single(delegateSymbol.TypeParameters);

        var invoke = delegateSymbol.GetDelegateInvokeMethod();
        Assert.NotNull(invoke);
        Assert.Equal(SpecialType.System_Boolean, invoke!.ReturnType.SpecialType);

        var parameters = invoke.Parameters;
        Assert.Equal(2, parameters.Length);

        Assert.Equal(RefKind.Ref, parameters[0].RefKind);
        Assert.Equal("T", parameters[0].Type.Name);

        Assert.Equal(RefKind.Out, parameters[1].RefKind);
        Assert.Equal(SpecialType.System_String, parameters[1].Type.SpecialType);
    }

    [Fact]
    public void MethodDeclaration_BindsTupleAndFunctionSignatureInNestedType()
    {
        const string source = """
class Container
{
    func Transform(callback: (int, string) -> bool) -> (left: int, right: string)
    {
        return (1, "ok")
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        var diagnostics = compilation.GetDiagnostics();
        Assert.Empty(diagnostics.Where(d => d.Severity == DiagnosticSeverity.Error));

        var model = compilation.GetSemanticModel(tree);
        var methodDeclaration = tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();
        var methodSymbol = Assert.IsAssignableFrom<IMethodSymbol>(model.GetDeclaredSymbol(methodDeclaration));

        var parameter = Assert.Single(methodSymbol.Parameters);
        var callbackType = Assert.IsAssignableFrom<INamedTypeSymbol>(parameter.Type);
        Assert.Equal(TypeKind.Delegate, callbackType.TypeKind);
        var invoke = callbackType.GetDelegateInvokeMethod();
        Assert.NotNull(invoke);
        Assert.Equal(SpecialType.System_Boolean, invoke!.ReturnType.SpecialType);
        Assert.Equal(2, invoke.Parameters.Length);
        Assert.Equal(SpecialType.System_Int32, invoke.Parameters[0].Type.SpecialType);
        Assert.Equal(SpecialType.System_String, invoke.Parameters[1].Type.SpecialType);

        var tupleReturn = Assert.IsAssignableFrom<ITupleTypeSymbol>(methodSymbol.ReturnType);
        Assert.Equal("left", tupleReturn.TupleElements[0].Name);
        Assert.Equal(SpecialType.System_Int32, tupleReturn.TupleElements[0].Type.SpecialType);
        Assert.Equal("right", tupleReturn.TupleElements[1].Name);
        Assert.Equal(SpecialType.System_String, tupleReturn.TupleElements[1].Type.SpecialType);
    }
}
