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
    func test() -> unit {
        val number = 42;
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

    [Fact]
    public void TypeDisplay_UsesDeclaringTypeParametersFromMetadata()
    {
        var compilation = CreateCompilation();
        var taskType = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.GetTypeByMetadataName("System.Threading.Tasks.Task`1"));

        var display = taskType.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat);

        Assert.Equal("Task<TResult>", display);
    }

    [Fact]
    public void TypeDisplay_NullableFunctionType_UsesGroupingParens()
    {
        const string source = """
class Sample {
    func test() -> unit {
        val groupedNullable: (() -> ())? = null
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var local = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(declarator));

        Assert.Equal("(() -> ())?", local.Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));
    }

    [Fact]
    public void TypeDisplay_FunctionWithNullableReturn_ShowsNullableOnReturnType()
    {
        const string source = """
class Sample {
    func test() -> unit {
        val nullableReturn: () -> ()? = () => ()
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var local = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(declarator));

        Assert.Equal("() -> ()?", local.Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));
    }

    [Fact]
    public void TypeDisplay_ConstructedMetadataType_UsesConcreteTypeArguments()
    {
        const string source = """
import System.Threading.Tasks.*

class Sample {
    func test() -> unit {
        val value: Task<int> = Task.FromResult(1)
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var local = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(declarator));

        Assert.Equal("Task<int>", local.Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));
        var fullyQualified = local.Type.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat);
        Assert.Contains("System.Threading.Tasks.Task<int>", fullyQualified);
        Assert.DoesNotContain("Task<TResult>", fullyQualified);
    }

    [Fact]
    public void TypeDisplay_ConstructedNestedGenericType_UsesConcreteNestedArguments()
    {
        const string source = """
import System.Threading.Tasks.*

class Sample {
    func test() -> unit {
        val value: Task<Task<int>> = Task.FromResult(Task.FromResult(42))
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var local = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(declarator));

        Assert.Equal("Task<Task<int>>", local.Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));
        var fullyQualified = local.Type.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat);
        Assert.Contains("Task<int>", fullyQualified);
        Assert.DoesNotContain("Task<TResult>", fullyQualified);
    }

    [Fact]
    public void DefaultDisplay_Method_UsesDeclarationKeyword()
    {
        const string source = """
class Program {
    static func Main(args: string[]) -> () { }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var methodSyntax = tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();
        var method = Assert.IsAssignableFrom<IMethodSymbol>(model.GetDeclaredSymbol(methodSyntax));

        Assert.Equal("static func Main(args: string[]) -> ()", method.ToDisplayString());
    }

    [Fact]
    public void DefaultDisplay_FieldAndProperty_UseDeclarationKeywords()
    {
        const string source = """
class Person {
    readonly field id: int = 1
    var Name: string { get; set; }
    val Age: int { get; }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var root = tree.GetRoot();

        var fieldDeclarator = root.DescendantNodes().OfType<VariableDeclaratorSyntax>().Single(d => d.Identifier.Text == "id");
        var field = Assert.IsAssignableFrom<IFieldSymbol>(model.GetDeclaredSymbol(fieldDeclarator));
        var namePropertySyntax = root.DescendantNodes().OfType<PropertyDeclarationSyntax>().Single(p => p.Identifier.Text == "Name");
        var nameProperty = Assert.IsAssignableFrom<IPropertySymbol>(model.GetDeclaredSymbol(namePropertySyntax));
        var agePropertySyntax = root.DescendantNodes().OfType<PropertyDeclarationSyntax>().Single(p => p.Identifier.Text == "Age");
        var ageProperty = Assert.IsAssignableFrom<IPropertySymbol>(model.GetDeclaredSymbol(agePropertySyntax));

        Assert.Equal("readonly field id: int", field.ToDisplayString());
        Assert.Equal("var Name: string", nameProperty.ToDisplayString());
        Assert.Equal("val Age: int", ageProperty.ToDisplayString());
    }
}
