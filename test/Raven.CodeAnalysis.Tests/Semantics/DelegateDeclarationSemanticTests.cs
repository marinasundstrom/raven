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
        var firstType = Assert.IsType<ByRefTypeSymbol>(parameters[0].Type);
        Assert.Equal("T", firstType.ElementType.Name);

        Assert.Equal(RefKind.Out, parameters[1].RefKind);
        var secondType = Assert.IsType<ByRefTypeSymbol>(parameters[1].Type);
        Assert.Equal(SpecialType.System_String, secondType.ElementType.SpecialType);
    }
}
