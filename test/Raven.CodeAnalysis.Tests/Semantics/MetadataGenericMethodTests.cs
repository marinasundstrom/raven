using System.Linq;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Tests;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class MetadataGenericMethodTests : CompilationTestBase
{
    protected override MetadataReference[] GetMetadataReferences()
        => TestMetadataReferences.DefaultWithExtensionMethods;

    [Fact]
    public void StaticGenericMethod_OnConstructedMetadataType_SubstitutesConstraint()
    {
        const string source = """
            class Program
            {
                static coerce() -> System.Exception
                {
                    let value: System.ArgumentException = null;
                    return Raven.MetadataFixtures.Generics.GenericContainer<System.Exception>.Coerce<System.ArgumentException>(value);
                }
            }
            """;

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(System.Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single();
        var method = Assert.IsAssignableFrom<IMethodSymbol>(model.GetSymbolInfo(invocation).Symbol);

        var exceptionType = compilation.GetTypeByMetadataName("System.Exception");
        var argumentExceptionType = compilation.GetTypeByMetadataName("System.ArgumentException");

        Assert.NotNull(exceptionType);
        Assert.NotNull(argumentExceptionType);

        Assert.Equal("Coerce", method.Name);
        Assert.True(method.IsGenericMethod);
        Assert.True(SymbolEqualityComparer.Default.Equals(method.ReturnType, exceptionType));
        Assert.Single(method.TypeArguments);
        Assert.True(SymbolEqualityComparer.Default.Equals(method.TypeArguments[0], argumentExceptionType));
    }

    [Fact]
    public void StaticGenericMethod_OnConstructedMetadataType_InvalidConstraint_ReportsDiagnostic()
    {
        const string source = """
            class Program
            {
                static invalid() -> unit
                {
                    let value: System.ArgumentException = null;
                    Raven.MetadataFixtures.Generics.GenericContainer<System.ValueType>.Coerce<System.ArgumentException>(value);
                }
            }
            """;

        var (compilation, _) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.Contains(diagnostics, diagnostic => diagnostic.Id == "RAV0320");
    }
}
