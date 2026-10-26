using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class EscapedIdentifierSemanticTests : CompilationTestBase
{
    [Theory]
    [InlineData("_")]
    [InlineData(" ")]
    [InlineData("a+b")]
    [InlineData("e\u0301")]
    [InlineData("é")]
    public void BacktickLocal_PreservesArbitraryName(string name)
    {
        var (compilation, tree) = CreateCompilation("class C { func Run() -> int { let `" + name + "` = 42; return `" + name + "` } }");
        Assert.Empty(compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error));
        var declaration = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        Assert.Equal(name, compilation.GetSemanticModel(tree).GetDeclaredSymbol(declaration)!.Name);
    }

    [Fact]
    public void BacktickIdentifiers_DoNotNormalizeUnicode()
    {
        var (compilation, tree) = CreateCompilation("func Run() -> int { let `é` = 1; let `e\u0301` = 2; return `é` + `e\u0301` }");
        Assert.Empty(compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error));
        var declarations = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().ToArray();
        var model = compilation.GetSemanticModel(tree);
        Assert.NotEqual(model.GetDeclaredSymbol(declarations[0])!.Name, model.GetDeclaredSymbol(declarations[1])!.Name);
    }

    [Fact]
    public void BacktickPropertiesAndEvents_UseLogicalNames()
    {
        var (compilation, tree) = CreateCompilation("""
            class C {
                public val `total price`: int = 50
                public event `price changed`: System.Action
                func Read() -> int => self.`total price`
            }
            """);
        Assert.Empty(compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error));
        var declaration = tree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>().Single();
        var symbol = Assert.IsAssignableFrom<INamedTypeSymbol>(compilation.GetSemanticModel(tree).GetDeclaredSymbol(declaration));
        Assert.IsAssignableFrom<IPropertySymbol>(Assert.Single(symbol.GetMembers("total price")));
        Assert.IsAssignableFrom<IEventSymbol>(Assert.Single(symbol.GetMembers("price changed")));
    }

    [Fact]
    public void BacktickIdentifiers_WorkInImportsNamedArgumentsAndLambdaParameters()
    {
        var (compilation, _) = CreateCompilation("""
            import `Price models`.*
            namespace `Price models` {
                class Calculator {
                    public static func Total(`unit price`: int) -> int => `unit price`
                }
            }
            class Consumer {
                func Run() -> int {
                    let increment: (int) -> int = (`_`) => `_` + 1
                    return increment(Calculator.Total(`unit price`: 49))
                }
            }
            """);
        Assert.Empty(compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error));
    }

    [Fact]
    public void BacktickNamespacesAndWhitespaceTypeNames_UseLogicalNames()
    {
        var (compilation, tree) = CreateCompilation("""
            namespace `Price models` {
                class ` ` {}
            }
            class Consumer {
                func Read(value: `Price models`.` `) -> `Price models`.` ` => value
            }
            """);
        Assert.Empty(compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error));
        var declaration = tree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>().First();
        var symbol = Assert.IsAssignableFrom<INamedTypeSymbol>(compilation.GetSemanticModel(tree).GetDeclaredSymbol(declaration));
        Assert.Equal(" ", symbol.Name);
        Assert.Equal("Price models", symbol.ContainingNamespace.Name);
    }

    [Fact]
    public void BacktickIdentifiers_BindDeclarationsMembersAndOrdinaryReferences()
    {
        var (compilation, tree) = CreateCompilation("""
            class `Price calculator` {
                static func `Calculate total price`(`unit price`: int, quantity: int) -> int {
                    let `total price` = `unit price` * `quantity`
                    return `total price`
                }
                static func Run() -> int => `Price calculator`.`Calculate total price`(10, 5)
            }
            """);
        Assert.Empty(compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error));
        var model = compilation.GetSemanticModel(tree);
        var method = tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().First();
        var symbol = Assert.IsAssignableFrom<IMethodSymbol>(model.GetDeclaredSymbol(method));
        Assert.Equal("Calculate total price", symbol.Name);
        Assert.Equal("unit price", symbol.Parameters[0].Name);
        var reference = tree.GetRoot().DescendantNodes().OfType<IdentifierNameSyntax>()
            .Single(n => n.Identifier.ValueText == "Calculate total price");
        Assert.Equal(symbol, model.GetSymbolInfo(reference).Symbol);
    }

    [Fact]
    public void BacktickIdentifiers_ShareOrdinaryAndAtEscapedNamespace()
    {
        var (compilation, _) = CreateCompilation("""
            class C {
                static func Run() -> int {
                    let value = 1
                    let @class = `value`
                    return `class`
                }
            }
            """);
        Assert.Empty(compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error));
    }

    [Fact]
    public void BacktickIdentifiers_DoNotCreateSeparateOverloads()
    {
        var (compilation, _) = CreateCompilation("""
            class C {
                func Run() -> int => 1
                func `Run`() -> int => 2
            }
            """);
        Assert.Contains(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
    }

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
                    let @class = @return
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
        let @and = @return
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
        let $ffiResult = 1
        let 数据 = $ffiResult + 1
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
