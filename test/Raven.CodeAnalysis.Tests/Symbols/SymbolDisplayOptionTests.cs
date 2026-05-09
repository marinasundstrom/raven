using System.IO;
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
    public void TypeDisplay_GenericDefinitionUsesTypeParameters()
    {
        const string source = """
union class Option<T> {
    case Some(value: T)
    case None
}
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var declaration = tree.GetRoot().DescendantNodes().OfType<UnionDeclarationSyntax>().Single();
        var optionType = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetDeclaredSymbol(declaration));

        Assert.Equal("Option<T>", optionType.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));
    }

    [Fact]
    public void TypeDisplay_ConstructedGenericUsesTypeArguments()
    {
        const string source = """
union class Option<T> {
    case Some(value: T)
    case None
}

class Sample {
    func test() -> unit {
        val item: Option<string> = null
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var local = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(declarator));

        Assert.Equal("Option<string>", local.Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));
    }

    [Fact]
    public void TypeInfo_GenericNameSyntaxUsesTypeArguments()
    {
        const string source = """
union class Option<T> {
    case Some(value: T)
    case None
}

class Sample {
    func test() -> unit {
        val item: Option<string> = null
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var genericName = tree.GetRoot().DescendantNodes().OfType<GenericNameSyntax>().Single();
        var type = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetTypeInfo(genericName).Type);

        Assert.Equal("Option<string>", type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));
    }

    [Fact]
    public void TypeDisplay_ConstructedMetadataGenericUsesTypeArguments()
    {
        var references = GetMetadataReferences()
            .Concat([MetadataReference.CreateFromFile(GetRavenCorePath())])
            .ToArray();
        var compilation = CreateCompilation(references: references);
        var optionDefinition = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.GetTypeByMetadataName("System.Option`1"));
        var constructedOption = optionDefinition.Construct(compilation.GetSpecialType(SpecialType.System_String));

        Assert.Equal("Option<string>", constructedOption.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));
    }

    [Fact]
    public void TypeInfo_MetadataGenericNameSyntaxUsesTypeArguments()
    {
        const string source = """
import System.*

class Sample {
    func test() -> unit {
        val item: Option<string> = null
    }
}
""";

        var references = GetMetadataReferences()
            .Concat([MetadataReference.CreateFromFile(GetRavenCorePath())])
            .ToArray();
        var (compilation, tree) = CreateCompilation(source, references: references);
        var model = compilation.GetSemanticModel(tree);
        var genericName = tree.GetRoot().DescendantNodes().OfType<GenericNameSyntax>().Single();
        var type = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetTypeInfo(genericName).Type);

        Assert.Equal("Option<string>", type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));
    }

    [Fact]
    public void TypeInfo_TargetTypedMetadataOptionCaseInConstructorArgumentUsesParameterType()
    {
        const string source = """
import System.*

val foo = Foo(
    Name: "Foo",
    Item: Some("Foo")
)

record Foo(
    val Name: string,
    val Item: Option<string>
)

class C {
    func Test() -> unit {
        val localFoo = Foo(
            Name: "Foo",
            Item: Some("Foo")
        )
        val localFoo2 = Foo(
            Name: "Foo",
            Item: .Some("Foo")
        )
    }
}
""";

        var references = GetMetadataReferences()
            .Concat([MetadataReference.CreateFromFile(GetRavenCorePath())])
            .ToArray();
        var (compilation, tree) = CreateCompilation(source, references: references);
        var model = compilation.GetSemanticModel(tree);
        _ = compilation.GetDiagnostics();

        var invocations = tree.GetRoot().DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Where(invocation =>
                invocation.Expression is IdentifierNameSyntax { Identifier.ValueText: "Some" } ||
                invocation.Expression is MemberBindingExpressionSyntax { Name.Identifier.ValueText: "Some" })
            .ToArray();

        Assert.Equal(3, invocations.Length);
        foreach (var invocation in invocations)
        {
            var typeInfo = model.GetTypeInfo(invocation);
            Assert.Equal("Some<string>", typeInfo.Type?.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));
            var type = Assert.IsAssignableFrom<INamedTypeSymbol>(typeInfo.ConvertedType);

            Assert.Equal("Option<string>", type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));
        }
    }

    [Fact]
    public void TypeInfo_TargetTypedSourceOptionCaseInConstructorArgumentProjectsUnionTypeArguments()
    {
        const string source = """
union class Option<T> {
    case Some(value: T)
    case None
}

record Foo(
    val Name: string,
    val Item: Option<string>
)

class C {
    func Test() -> unit {
        val localFoo = Foo(
            Name: "Foo",
            Item: .Some("Foo")
        )
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        _ = compilation.GetDiagnostics();

        var invocation = tree.GetRoot().DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single(invocation => invocation.Expression is MemberBindingExpressionSyntax { Name.Identifier.ValueText: "Some" });
        var typeInfo = model.GetTypeInfo(invocation);

        Assert.Equal("Some<string>", typeInfo.Type?.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));
        var type = Assert.IsAssignableFrom<INamedTypeSymbol>(typeInfo.ConvertedType);
        Assert.Equal("Option<string>", type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));
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
    public void TypeDisplay_NamedDelegate_UsesDelegateDeclarationForm()
    {
        const string source = """
import System.ComponentModel.*

class Sample {
    func test() -> unit {
        val handler: PropertyChangedEventHandler = (sender, args) => ()
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var local = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(declarator));

        Assert.Equal(
            "delegate PropertyChangedEventHandler(sender: object?, e: PropertyChangedEventArgs) -> ()",
            local.Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));
    }

    [Fact]
    public void TypeDisplay_GenericNamedDelegate_IncludesTypeParameters()
    {
        const string source = """
class Container {
    public delegate Formatter<T>(ref value: T, out result: string) -> bool
}
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var delegateDeclaration = tree.GetRoot().DescendantNodes().OfType<DelegateDeclarationSyntax>().Single();
        var delegateSymbol = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetDeclaredSymbol(delegateDeclaration));
        var constructedDelegate = delegateSymbol.Construct(compilation.GetSpecialType(SpecialType.System_Int32));

        Assert.Equal(
            "delegate Formatter<int>(ref value: int, out result: string) -> bool",
            constructedDelegate.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));
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

    [Fact]
    public void TypeDisplay_TupleElementNames_AreShownWhenEnabled()
    {
        const string source = """
val tuple: (id: int, name: string) = (1, "x")
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var local = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(declarator));

        var baseDisplay = local.Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat);
        Assert.Equal("(int, string)", baseDisplay);

        var withTupleNames = SymbolDisplayFormat.MinimallyQualifiedFormat.WithMiscellaneousOptions(
            SymbolDisplayFormat.MinimallyQualifiedFormat.MiscellaneousOptions |
            SymbolDisplayMiscellaneousOptions.IncludeTupleElementNames);

        var display = local.Type.ToDisplayString(withTupleNames);
        Assert.Equal("(id: int, name: string)", display);
    }

    private static string GetRavenCorePath()
    {
        var outputPath = Path.Combine(AppContext.BaseDirectory, "Raven.Core.dll");
        if (File.Exists(outputPath))
            return outputPath;

        var repoRoot = Path.GetFullPath(Path.Combine(AppContext.BaseDirectory, "..", "..", "..", "..", ".."));
        return Path.Combine(repoRoot, "src", "Raven.Core", "bin", "Debug", "net10.0", "Raven.Core.dll");
    }
}
