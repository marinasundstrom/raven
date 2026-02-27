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

        Assert.Equal("number: int", local.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));

        var format = SymbolDisplayFormat.MinimallyQualifiedFormat
            .WithLocalOptions(SymbolDisplayLocalOptions.IncludeType);

        Assert.Equal("number: int", local.ToDisplayString(format));
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

        Assert.Contains("() -> ()?", local.Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat), StringComparison.Ordinal);
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
