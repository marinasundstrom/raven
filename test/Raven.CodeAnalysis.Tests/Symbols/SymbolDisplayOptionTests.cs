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
            "public string System.String.Concat(string str0, string str1)",
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
            "public string System.String.Concat(string str0, string str1)",
            methodSymbol.ToDisplayString(withSpecialTypes));

        Assert.Equal(
            "public System.String System.String.Concat(System.String str0, System.String str1)",
            methodSymbol.ToDisplayString(withoutSpecialTypes));
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

        Assert.Equal("int number", local.ToDisplayString(format));
    }
}
