using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class SemanticFactsTests
{
    [Fact]
    public void IsDerivedFrom_ReturnsTrueForDirectBaseType()
    {
        var source = "class Base {} class Derived : Base {}";
        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
            "test",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        var derived = GetSourceType(compilation, "Derived");
        var baseType = GetSourceType(compilation, "Base");

        Assert.True(SemanticFacts.IsDerivedFrom(derived, baseType));
        Assert.False(SemanticFacts.IsDerivedFrom(baseType, derived));
    }

    [Fact]
    public void IsDerivedFrom_ReturnsTrueForTypeParameterConstraint()
    {
        var source = "class Base {} class Container<T : Base> {}";
        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
            "test",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        var container = GetSourceType(compilation, "Container");
        var typeParameter = container.TypeParameters[0];
        var baseType = GetSourceType(compilation, "Base");

        Assert.True(SemanticFacts.IsDerivedFrom(typeParameter, baseType));
    }

    [Fact]
    public void ImplementsInterface_ReturnsTrueForClassImplementation()
    {
        var source = "interface IMarker {} class Implementation : IMarker {}";
        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
            "test",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        var implementation = GetSourceType(compilation, "Implementation");
        var marker = GetSourceType(compilation, "IMarker");

        Assert.True(SemanticFacts.ImplementsInterface(implementation, marker));
    }

    [Fact]
    public void ImplementsInterface_ReturnsTrueForTypeParameterConstraint()
    {
        var source = "interface IMarker {} class Container<T : IMarker> {}";
        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
            "test",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        var container = GetSourceType(compilation, "Container");
        var typeParameter = container.TypeParameters[0];
        var marker = GetSourceType(compilation, "IMarker");

        Assert.True(SemanticFacts.ImplementsInterface(typeParameter, marker));
    }

    [Fact]
    public void ImplementsInterface_ReturnsTrueForInterfaceItself()
    {
        var source = "interface IMarker {}";
        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
            "test",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        var marker = GetSourceType(compilation, "IMarker");

        Assert.True(SemanticFacts.ImplementsInterface(marker, marker));
    }

    [Fact]
    public void SatisfiesConstraints_ReturnsTrueForClassConstraint()
    {
        var source = "class Base {} class Derived : Base {} class Container<T : Base> {}";
        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
            "test",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        var container = GetSourceType(compilation, "Container");
        var typeParameter = container.TypeParameters[0];
        var derived = GetSourceType(compilation, "Derived");

        Assert.True(SemanticFacts.SatisfiesConstraints(derived, typeParameter));
    }

    [Fact]
    public void SatisfiesConstraints_ReturnsFalseForClassConstraint()
    {
        var source = "class Base {} class Other {} class Container<T : Base> {}";
        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
            "test",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        var container = GetSourceType(compilation, "Container");
        var typeParameter = container.TypeParameters[0];
        var other = GetSourceType(compilation, "Other");

        Assert.False(SemanticFacts.SatisfiesConstraints(other, typeParameter));
    }

    [Fact]
    public void SatisfiesConstraints_ReturnsTrueForInterfaceConstraint()
    {
        var source = "interface IMarker {} class Container<T : IMarker> {} class Implementation : IMarker {}";
        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
            "test",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        var container = GetSourceType(compilation, "Container");
        var typeParameter = container.TypeParameters[0];
        var implementation = GetSourceType(compilation, "Implementation");

        Assert.True(SemanticFacts.SatisfiesConstraints(implementation, typeParameter));
    }

    [Fact]
    public void SatisfiesConstraints_HonorsStructConstraint()
    {
        var source = "class Container<T : struct> {}";
        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
            "test",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        var container = GetSourceType(compilation, "Container");
        var typeParameter = container.TypeParameters[0];
        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var stringType = compilation.GetSpecialType(SpecialType.System_String);

        Assert.True(SemanticFacts.SatisfiesConstraints(intType, typeParameter));
        Assert.False(SemanticFacts.SatisfiesConstraints(stringType, typeParameter));
    }

    [Fact]
    public void SatisfiesConstraints_HonorsReferenceTypeConstraint()
    {
        var source = "class Container<T : class> {}";
        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
            "test",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        var container = GetSourceType(compilation, "Container");
        var typeParameter = container.TypeParameters[0];
        var stringType = compilation.GetSpecialType(SpecialType.System_String);
        var intType = compilation.GetSpecialType(SpecialType.System_Int32);

        Assert.True(SemanticFacts.SatisfiesConstraints(stringType, typeParameter));
        Assert.False(SemanticFacts.SatisfiesConstraints(intType, typeParameter));
    }

    [Fact]
    public void SatisfiesConstructorConstraint_ReturnsFalseForUnionClassWithoutPublicParameterlessConstructor()
    {
        var source = """
union class Response<T> {
    case Success(value: T)
    case Failure(message: string)
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
            "test",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        var response = GetSourceType(compilation, "Response");

        Assert.False(SemanticFacts.SatisfiesConstructorConstraint(response));
    }

    [Fact]
    public void ImplementsInterface_ReturnsTrueForArrayInterfaces()
    {
        var tree = SyntaxTree.ParseText("class C {}");
        var compilation = Compilation.Create(
            "test",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var arrayType = compilation.CreateArrayTypeSymbol(intType);
        var enumerable = (NamedTypeSymbol)compilation.GetTypeByMetadataName("System.Collections.IEnumerable")!;

        Assert.True(SemanticFacts.ImplementsInterface(arrayType, enumerable));
    }

    [Fact]
    public void ImplementsInterface_ArrayHonorsConstructedGenericInterfaces()
    {
        var tree = SyntaxTree.ParseText("class C {}");
        var compilation = Compilation.Create(
            "test",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var arrayType = compilation.CreateArrayTypeSymbol(intType);

        var enumerableDefinition = (NamedTypeSymbol)compilation.GetTypeByMetadataName("System.Collections.Generic.IEnumerable`1")!;
        var enumerableOfInt = (NamedTypeSymbol)enumerableDefinition.Construct(intType);
        var enumerableOfString = (NamedTypeSymbol)enumerableDefinition.Construct(compilation.GetSpecialType(SpecialType.System_String));
        var listDefinition = (NamedTypeSymbol)compilation.GetTypeByMetadataName("System.Collections.Generic.IList`1")!;
        var listOfInt = (NamedTypeSymbol)listDefinition.Construct(intType);

        Assert.True(SemanticFacts.ImplementsInterface(arrayType, enumerableOfInt));
        Assert.False(SemanticFacts.ImplementsInterface(arrayType, enumerableOfString));
        Assert.True(SemanticFacts.ImplementsInterface(arrayType, listOfInt));
    }

    [Fact]
    public void ImplementsInterface_HandlesCovariantInterfaceArguments()
    {
        var tree = SyntaxTree.ParseText("class C {}");
        var compilation = Compilation.Create(
            "test",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        var stringType = compilation.GetSpecialType(SpecialType.System_String);
        var listDefinition = (NamedTypeSymbol)compilation.GetTypeByMetadataName("System.Collections.Generic.List`1")!;
        var listOfString = (NamedTypeSymbol)listDefinition.Construct(stringType);
        var enumerableDefinition = (NamedTypeSymbol)compilation.GetTypeByMetadataName("System.Collections.Generic.IEnumerable`1")!;
        var enumerableOfObject = (NamedTypeSymbol)enumerableDefinition.Construct(compilation.GetSpecialType(SpecialType.System_Object));

        Assert.True(SemanticFacts.ImplementsInterface(listOfString, enumerableOfObject));
    }

    [Fact]
    public void ImplementsInterface_InterfaceHandlesCovariance()
    {
        var tree = SyntaxTree.ParseText("class C {}");
        var compilation = Compilation.Create(
            "test",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        var stringType = compilation.GetSpecialType(SpecialType.System_String);
        var enumerableDefinition = (NamedTypeSymbol)compilation.GetTypeByMetadataName("System.Collections.Generic.IEnumerable`1")!;
        var enumerableOfString = (NamedTypeSymbol)enumerableDefinition.Construct(stringType);
        var enumerableOfObject = (NamedTypeSymbol)enumerableDefinition.Construct(compilation.GetSpecialType(SpecialType.System_Object));

        Assert.True(SemanticFacts.ImplementsInterface(enumerableOfString, enumerableOfObject));
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void ImplementsInterface_HandlesMetadataContravarianceRegardlessOfQueryOrder(bool diagnosticsFirst)
    {
        var source = """
import System.Collections.Generic.*

class Comparer : IComparer<object>
{
    init() {}

    func Compare(x: object?, y: object?) -> int => 0
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
            "test",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        if (diagnosticsFirst)
            Assert.Empty(compilation.GetDiagnostics());

        var comparer = GetSourceType(compilation, "Comparer");
        var comparerDefinition = (NamedTypeSymbol)compilation.GetTypeByMetadataName("System.Collections.Generic.IComparer`1")!;
        var comparerOfString = (NamedTypeSymbol)comparerDefinition.Construct(compilation.GetSpecialType(SpecialType.System_String));

        Assert.True(SemanticFacts.ImplementsInterface(comparer, comparerOfString));

        if (!diagnosticsFirst)
            Assert.Empty(compilation.GetDiagnostics());
    }

    [Theory]
    [InlineData(false, false)]
    [InlineData(false, true)]
    [InlineData(true, false)]
    [InlineData(true, true)]
    public void ImplementsInterface_SourceVarianceMatchesEmittedMetadata(
        bool useMetadata,
        bool diagnosticsFirst)
    {
        const string librarySource = """
namespace VarianceLibrary {
    public interface Producer<out T> {}
    public interface Consumer<in T> {}
    public class Provider : Producer<string> {}
    public class Handler : Consumer<object> {}
}
""";

        var libraryTree = SyntaxTree.ParseText(librarySource);
        MetadataReference[] references = useMetadata
            ? [.. TestMetadataReferences.Default,
                TestMetadataFactory.CreateFromSource(librarySource, "variance_library")]
            : TestMetadataReferences.Default;
        var compilation = Compilation.Create(
            "variance_consumer",
            useMetadata ? [] : [libraryTree],
            references,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        if (diagnosticsFirst)
            Assert.Empty(compilation.GetDiagnostics());

        var producerDefinition = Assert.IsAssignableFrom<NamedTypeSymbol>(
            compilation.GetTypeByMetadataName("VarianceLibrary.Producer`1"));
        var consumerDefinition = Assert.IsAssignableFrom<NamedTypeSymbol>(
            compilation.GetTypeByMetadataName("VarianceLibrary.Consumer`1"));
        var provider = Assert.IsAssignableFrom<NamedTypeSymbol>(
            compilation.GetTypeByMetadataName("VarianceLibrary.Provider"));
        var handler = Assert.IsAssignableFrom<NamedTypeSymbol>(
            compilation.GetTypeByMetadataName("VarianceLibrary.Handler"));
        var objectType = compilation.GetSpecialType(SpecialType.System_Object);
        var stringType = compilation.GetSpecialType(SpecialType.System_String);
        var producerOfObject = Assert.IsAssignableFrom<NamedTypeSymbol>(producerDefinition.Construct(objectType));
        var consumerOfString = Assert.IsAssignableFrom<NamedTypeSymbol>(consumerDefinition.Construct(stringType));

        Assert.Equal(VarianceKind.Out, Assert.Single(producerDefinition.TypeParameters).Variance);
        Assert.Equal(VarianceKind.In, Assert.Single(consumerDefinition.TypeParameters).Variance);
        Assert.True(SemanticFacts.ImplementsInterface(provider, producerOfObject));
        Assert.True(SemanticFacts.ImplementsInterface(handler, consumerOfString));

        if (!diagnosticsFirst)
            Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void ImplementsInterface_SourceVariance_AllowsCovariantMatches()
    {
        var source = """
interface Producer<out T> {}

class Widget : Producer<string> {}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
            "test",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        var model = compilation.GetSemanticModel(tree);
        var root = tree.GetRoot();
        var interfaceSyntax = root.DescendantNodes().OfType<InterfaceDeclarationSyntax>().Single();
        var classSyntax = root.DescendantNodes().OfType<ClassDeclarationSyntax>().Single();

        var producerDefinition = (NamedTypeSymbol)model.GetDeclaredSymbol(interfaceSyntax)!;
        var widget = (NamedTypeSymbol)model.GetDeclaredSymbol(classSyntax)!;

        var producerOfObject = (NamedTypeSymbol)producerDefinition.Construct(compilation.GetSpecialType(SpecialType.System_Object));

        Assert.True(SemanticFacts.ImplementsInterface(widget, producerOfObject));
    }

    [Fact]
    public void ImplementsInterface_SourceVariance_AllowsContravariantMatches()
    {
        var source = """
interface Consumer<in T> {}

class Handler : Consumer<object> {}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
            "test",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        var model = compilation.GetSemanticModel(tree);
        var root = tree.GetRoot();
        var interfaceSyntax = root.DescendantNodes().OfType<InterfaceDeclarationSyntax>().Single();
        var classSyntax = root.DescendantNodes().OfType<ClassDeclarationSyntax>().Single();

        var consumerDefinition = (NamedTypeSymbol)model.GetDeclaredSymbol(interfaceSyntax)!;
        var handler = (NamedTypeSymbol)model.GetDeclaredSymbol(classSyntax)!;

        var consumerOfString = (NamedTypeSymbol)consumerDefinition.Construct(compilation.GetSpecialType(SpecialType.System_String));

        Assert.True(SemanticFacts.ImplementsInterface(handler, consumerOfString));
    }

    [Theory]
    [InlineData("out", "int", SpecialType.System_Object, false)]
    [InlineData("in", "object", SpecialType.System_Int32, false)]
    [InlineData("out", "int", SpecialType.System_Int32, true)]
    [InlineData("out", "string", SpecialType.System_Object, true)]
    public void ImplementsInterface_VarianceRequiresReferenceConversion(
        string variance, string argument, SpecialType targetArgument, bool expected)
    {
        var tree = SyntaxTree.ParseText($"interface Contract<{variance} T> {{}} class Provider : Contract<{argument}> {{}}");
        var compilation = Compilation.Create("VarianceConversion", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var contract = GetSourceType(compilation, "Contract");
        var provider = GetSourceType(compilation, "Provider");
        var target = (NamedTypeSymbol)contract.Construct(compilation.GetSpecialType(targetArgument));
        Assert.Equal(expected, SemanticFacts.ImplementsInterface(provider, target));
    }

    [Theory]
    [InlineData("class Bound {}", false)]
    [InlineData("interface Bound {}", true)]
    public void NamedConstraintFollowsForwardedSourceParameter(string declaration, bool interfaceBound)
    {
        var tree = SyntaxTree.ParseText($"{declaration}\nclass Forward<T, U> where T: U where U: Bound {{}}");
        var compilation = Compilation.Create("ForwardedBounds", [tree], TestMetadataReferences.Default,
            CompilationOptions.DotNet.WithOutputKind(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(compilation.GetDiagnostics(), diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
        var bound = GetSourceType(compilation, "Bound");
        Assert.Equal(interfaceBound, bound.TypeKind == TypeKind.Interface);
        var t = compilation.GetTypeByMetadataName("Forward`2")!.TypeParameters[0];
        Assert.True(SemanticFacts.SatisfiesTypeConstraint(t, bound));
    }

    private static NamedTypeSymbol GetSourceType(Compilation compilation, string name)
    {
        foreach (var syntaxTree in compilation.SyntaxTrees)
        {
            var declaration = syntaxTree.GetRoot()
                .DescendantNodes()
                .OfType<BaseTypeDeclarationSyntax>()
                .FirstOrDefault(type => type.Identifier.ValueText == name);

            if (declaration is not null)
                return Assert.IsAssignableFrom<NamedTypeSymbol>(
                    compilation.GetSemanticModel(syntaxTree).GetDeclaredSymbol(declaration));
        }

        return Assert.IsAssignableFrom<NamedTypeSymbol>(compilation.SourceGlobalNamespace.LookupType(name));
    }
}
