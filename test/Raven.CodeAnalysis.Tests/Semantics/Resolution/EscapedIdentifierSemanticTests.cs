using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class EscapedIdentifierSemanticTests : CompilationTestBase
{
    [Fact]
    public void EscapedClassIdentifier_UsesUnescapedSymbolName()
    {
        var source = """
            class @int {}
            """;

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);

        var classDecl = tree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>().Single();
        var symbol = (INamedTypeSymbol)model.GetDeclaredSymbol(classDecl)!;

        Assert.Equal("int", symbol.Name);
    }

    [Fact]
    public void EscapedMemberAndLocalIdentifiers_UseUnescapedSymbolNames()
    {
        var source = """
            class Container
            {
                static func @int(@return: int) -> int
                {
                    val @class = @return
                    return @class
                }
            }
            """;

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);

        var method = tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();
        var methodSymbol = (IMethodSymbol)model.GetDeclaredSymbol(method)!;
        Assert.Equal("int", methodSymbol.Name);

        var parameterSyntax = method.ParameterList.Parameters.Single();
        var parameterSymbol = (IParameterSymbol)model.GetDeclaredSymbol(parameterSyntax)!;
        Assert.Equal("return", parameterSymbol.Name);

        var declarator = method.Body!.DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var localSymbol = (ILocalSymbol)model.GetDeclaredSymbol(declarator)!;
        Assert.Equal("class", localSymbol.Name);
    }

    [Fact]
    public void EscapedIdentifierReferences_BindToDeclaredSymbols()
    {
        const string source = """
class C {
    func @match(@return: int) -> int {
        val @and = @return
        return @and
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var root = tree.GetRoot();
        var method = root.DescendantNodes().OfType<MethodDeclarationSyntax>().Single();
        var parameter = method.ParameterList.Parameters.Single();
        var local = root.DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();

        var parameterSymbol = Assert.IsAssignableFrom<IParameterSymbol>(model.GetDeclaredSymbol(parameter));
        var localSymbol = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(local));
        var references = root.DescendantNodes().OfType<IdentifierNameSyntax>()
            .Where(static identifier => identifier.Identifier.Text is "@return" or "@and")
            .ToArray();

        var parameterReference = Assert.IsAssignableFrom<IParameterSymbol>(
            model.GetSymbolInfo(references.Single(identifier => identifier.Identifier.Text == "@return")).Symbol);
        var localReference = Assert.IsAssignableFrom<ILocalSymbol>(
            model.GetSymbolInfo(references.Single(identifier => identifier.Identifier.Text == "@and")).Symbol);

        Assert.Equal(parameterSymbol.Name, parameterReference.Name);
        Assert.Equal(parameter.Identifier.Span, parameterReference.Locations[0].SourceSpan);
        Assert.Equal(localSymbol.Name, localReference.Name);
        Assert.Equal(local.Identifier.Span, localReference.Locations[0].SourceSpan);
    }

    [Fact]
    public void DollarAndUnicodeIdentifiers_BindAsOrdinaryLocals()
    {
        const string source = """
class C {
    func Test() -> int {
        val $ffiResult = 1
        val 数据 = $ffiResult + 1
        return 数据
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        var diagnostics = compilation.GetDiagnostics()
            .Where(static diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);

        Assert.Empty(diagnostics);

        var model = compilation.GetSemanticModel(tree);
        var locals = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>()
            .ToDictionary(static declarator => declarator.Identifier.ValueText);

        var ffiLocal = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(locals["$ffiResult"]));
        var dataLocal = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(locals["数据"]));

        Assert.Equal("$ffiResult", ffiLocal.Name);
        Assert.Equal("数据", dataLocal.Name);
    }

    [Fact]
    public void ToDisplayString_WithEscapeKeywordOption_EscapesIdentifier()
    {
        var source = """
            class @int {}
            """;

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);

        var classDecl = tree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>().Single();
        var symbol = (INamedTypeSymbol)model.GetDeclaredSymbol(classDecl)!;

        var noEscape = SymbolDisplayFormat.MinimallyQualifiedFormat;
        Assert.Equal("int", symbol.ToDisplayString(noEscape));

        var escapeKeywords = noEscape.WithMiscellaneousOptions(
            noEscape.MiscellaneousOptions | SymbolDisplayMiscellaneousOptions.EscapeKeywordIdentifiers);

        Assert.Equal("@int", symbol.ToDisplayString(escapeKeywords));
    }

    [Fact]
    public void ToDisplayString_WithEscapeKeywordOption_EscapesContainingNames()
    {
        var source = """
            namespace @match {
                class @class
                {
                    class @int {}
                }
            }
            """;

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);

        var innerClass = tree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>().Last();
        var symbol = (INamedTypeSymbol)model.GetDeclaredSymbol(innerClass)!;

        var format = SymbolDisplayFormat.MinimallyQualifiedFormat
            .WithTypeQualificationStyle(SymbolDisplayTypeQualificationStyle.NameAndContainingTypesAndNamespaces)
            .WithMiscellaneousOptions(
                SymbolDisplayMiscellaneousOptions.UseSpecialTypes |
                SymbolDisplayMiscellaneousOptions.EscapeKeywordIdentifiers);

        Assert.Equal("@match.@class.@int", symbol.ToDisplayString(format));
    }

    [Fact]
    public void ToDisplayString_WithEscapeKeywordOption_DoesNotEscapeConstructorDisplayName()
    {
        var source = """
            class Person {
                init(name: string) {}
            }
            """;

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);

        var constructor = tree.GetRoot().DescendantNodes().OfType<ConstructorDeclarationSyntax>().Single();
        var symbol = (IMethodSymbol)model.GetDeclaredSymbol(constructor)!;

        var format = SymbolDisplayFormat.MinimallyQualifiedFormat
            .WithDelegateStyle(SymbolDisplayDelegateStyle.NameAndSignature)
            .WithMemberOptions(
                SymbolDisplayMemberOptions.IncludeType |
                SymbolDisplayMemberOptions.IncludeParameters)
            .WithParameterOptions(
                SymbolDisplayParameterOptions.IncludeType |
                SymbolDisplayParameterOptions.IncludeName)
            .WithMiscellaneousOptions(
                SymbolDisplayMiscellaneousOptions.UseSpecialTypes |
                SymbolDisplayMiscellaneousOptions.EscapeKeywordIdentifiers);

        Assert.Equal("init(name: string) -> ()", symbol.ToDisplayString(format));
    }

    [Fact]
    public void ToDisplayString_WithEscapeKeywordOption_DoesNotEscapeExtensionSelfParameter()
    {
        var source = """
            extension IntExtensions for int {
                func Increment() -> int {
                    return self + 1
                }
            }
            """;

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);

        var method = tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();
        var symbol = (IMethodSymbol)model.GetDeclaredSymbol(method)!;

        var format = SymbolDisplayFormat.MinimallyQualifiedFormat
            .WithDelegateStyle(SymbolDisplayDelegateStyle.NameAndSignature)
            .WithMemberOptions(
                SymbolDisplayMemberOptions.IncludeType |
                SymbolDisplayMemberOptions.IncludeParameters)
            .WithParameterOptions(
                SymbolDisplayParameterOptions.IncludeType |
                SymbolDisplayParameterOptions.IncludeName)
            .WithMiscellaneousOptions(
                SymbolDisplayMiscellaneousOptions.UseSpecialTypes |
                SymbolDisplayMiscellaneousOptions.EscapeKeywordIdentifiers);

        var display = symbol.ToDisplayString(format);
        Assert.Contains("self: int", display);
        Assert.DoesNotContain("@self", display);
    }
}
