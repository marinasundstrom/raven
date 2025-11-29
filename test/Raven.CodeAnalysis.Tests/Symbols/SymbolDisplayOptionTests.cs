using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Semantics.Tests;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Xunit;

namespace Raven.CodeAnalysis.Tests;

public sealed class SymbolDisplayOptionTests : CompilationTestBase
{
    [Fact]
    public void MethodDisplay_RespectsAccessibilityReturnTypeAndParameters()
    {
        var compilation = CreateCompilation();
        var stringType = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.GetTypeByMetadataName("System.String"));

        var methodSymbol = stringType.GetMembers("Concat")
            .OfType<IMethodSymbol>()
            .First(m => m.Parameters.Length == 2 &&
                        m.Parameters.All(p => p.Type.SpecialType == SpecialType.System_String));

        var minimal = SymbolDisplayFormat.MinimallyQualifiedFormat;
        Assert.Equal("Concat", methodSymbol.ToDisplayString(minimal));

        var detailed = minimal
            .WithDelegateStyle(SymbolDisplayDelegateStyle.NameAndSignature)
            .WithTypeQualificationStyle(SymbolDisplayTypeQualificationStyle.NameAndContainingTypesAndNamespaces)
            .WithMemberOptions(
                SymbolDisplayMemberOptions.IncludeAccessibility |
                SymbolDisplayMemberOptions.IncludeType |
                SymbolDisplayMemberOptions.IncludeParameters)
            .WithParameterOptions(
                SymbolDisplayParameterOptions.IncludeType |
                SymbolDisplayParameterOptions.IncludeName);

        var display = methodSymbol.ToDisplayString(detailed);

        Assert.Equal(
            "public System.String.Concat(str0: string, str1: string) -> string",
            display);
    }

    [Fact]
    public void MethodDisplay_TogglesSpecialTypeKeywordsWithOption()
    {
        var compilation = CreateCompilation();
        var stringType = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.GetTypeByMetadataName("System.String"));

        var methodSymbol = stringType.GetMembers("Concat")
            .OfType<IMethodSymbol>()
            .First(m => m.Parameters.Length == 2 &&
                        m.Parameters.All(p => p.Type.SpecialType == SpecialType.System_String));

        var baseFormat = SymbolDisplayFormat.MinimallyQualifiedFormat
            .WithDelegateStyle(SymbolDisplayDelegateStyle.NameAndSignature)
            .WithTypeQualificationStyle(SymbolDisplayTypeQualificationStyle.NameAndContainingTypesAndNamespaces)
            .WithMemberOptions(
                SymbolDisplayMemberOptions.IncludeAccessibility |
                SymbolDisplayMemberOptions.IncludeType |
                SymbolDisplayMemberOptions.IncludeParameters)
            .WithParameterOptions(
                SymbolDisplayParameterOptions.IncludeType |
                SymbolDisplayParameterOptions.IncludeName);

        var withSpecialTypes = baseFormat;
        var withoutSpecialTypes = baseFormat.WithMiscellaneousOptions(SymbolDisplayMiscellaneousOptions.None);

        Assert.Equal(
            "public System.String.Concat(str0: string, str1: string) -> string",
            methodSymbol.ToDisplayString(withSpecialTypes));

        Assert.Equal(
            "public System.String.Concat(str0: System.String, str1: System.String) -> System.String",
            methodSymbol.ToDisplayString(withoutSpecialTypes));
    }

    [Fact]
    public void MethodDisplay_UsesParameterMemberOptionWithoutDelegateStyle()
    {
        var compilation = CreateCompilation();
        var stringType = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.GetTypeByMetadataName("System.String"));

        var methodSymbol = stringType.GetMembers("Concat")
            .OfType<IMethodSymbol>()
            .First(m => m.Parameters.Length == 2 &&
                        m.Parameters.All(p => p.Type.SpecialType == SpecialType.System_String));

        var format = SymbolDisplayFormat.MinimallyQualifiedFormat
            .WithMemberOptions(
                SymbolDisplayMemberOptions.IncludeType |
                SymbolDisplayMemberOptions.IncludeParameters)
            .WithParameterOptions(
                SymbolDisplayParameterOptions.IncludeType |
                SymbolDisplayParameterOptions.IncludeName);

        var display = methodSymbol.ToDisplayString(format);

        Assert.Equal("Concat(str0: string, str1: string) -> string", display);
    }

    [Fact]
    public void LocalDisplay_UsesIncludeTypeOption()
    {
        const string source = """
class Sample {
    test() -> unit {
        let number = 42;
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var local = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(declarator));

        Assert.Equal("number", local.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));

        var format = SymbolDisplayFormat.MinimallyQualifiedFormat
            .WithLocalOptions(SymbolDisplayLocalOptions.IncludeType);

        Assert.Equal("number: int", local.ToDisplayString(format));
    }

    [Fact]
    public void GenericMethodDisplay_OmitsContainingTypeOnTypeParameter()
    {
        var compilation = CreateCompilation();
        var taskType = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.GetTypeByMetadataName("System.Threading.Tasks.Task"));

        var run = taskType.GetMembers("Run")
            .OfType<IMethodSymbol>()
            .First(m => m.IsGenericMethod && m.Parameters.Length == 1 &&
                        m.Parameters[0].Type is INamedTypeSymbol
                        {
                            Name: "Func",
                            TypeArguments.Length: 1,
                            TypeArguments: [INamedTypeSymbol { Name: "Task" }]
                        });

        var format = SymbolDisplayFormat.MinimallyQualifiedFormat
            .WithGenericsOptions(SymbolDisplayGenericsOptions.IncludeTypeParameters)
            .WithMemberOptions(
                SymbolDisplayMemberOptions.IncludeContainingType |
                SymbolDisplayMemberOptions.IncludeType |
                SymbolDisplayMemberOptions.IncludeParameters)
            .WithParameterOptions(
                SymbolDisplayParameterOptions.IncludeType |
                SymbolDisplayParameterOptions.IncludeName);

        Assert.Equal(
            "Task.Run<TResult>(function: Func<Task<TResult>>) -> Task<TResult>",
            run.ToDisplayString(format));
    }
}
